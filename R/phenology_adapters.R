# Internal input adapters. Overlap arithmetic remains in check_temporal_overlap().
.etn_date <- function(x) {
  if (inherits(x, "Date")) {
    if (anyNA(x) || any(!is.finite(as.numeric(x))))
      stop("Dates must be non-missing and valid YYYY-MM-DD dates.")
    return(x)
  }
  if (!is.character(x))
    stop("Date columns must contain Date values or YYYY-MM-DD strings.")
  parsed <- tryCatch(as.Date(x, format = "%Y-%m-%d"), error = function(e)
    stop("Dates must be non-missing and valid YYYY-MM-DD dates."))
  valid <- !is.na(x) & !is.na(parsed)
  valid[valid] <- format(parsed[valid], "%Y-%m-%d") == x[valid]
  if (!all(valid)) stop("Dates must be non-missing and valid YYYY-MM-DD dates.")
  parsed
}

.etn_validate_presence <- function(p) {
  if (!is.matrix(p) || !(is.numeric(p) || is.logical(p)) ||
      any(!is.na(p) & !(p %in% c(0, 1))))
    stop("presence_matrix must be a numeric or logical matrix of 0, 1, or NA.")
  species <- colnames(p)
  if (is.null(species) || anyNA(species) || any(!nzchar(trimws(species))) ||
      anyDuplicated(species)) stop("presence_matrix requires unique, nonempty species column names.")
  if (!nrow(p)) stop("presence_matrix must have at least one day.")
  # Rows denote consecutive days; supplied Date labels must agree with that contract.
  labels <- rownames(p)
  if (!is.null(labels) && length(labels) &&
      all(grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", labels))) {
    dates <- .etn_date(labels)
    if (any(diff(as.numeric(dates)) != 1))
      stop("presence_matrix Date rows must be consecutive and chronological.")
  }
  p
}

.etn_presence_runs <- function(p) {
  rows <- lapply(seq_len(ncol(p)), function(j) {
    active <- which(!is.na(p[, j]) & p[, j] == 1L)
    if (!length(active)) return(NULL)
    episode <- cumsum(c(TRUE, diff(active) > 1L))
    do.call(rbind, lapply(split(active, episode), function(idx) {
      data.frame(species = colnames(p)[j], start_day = min(idx), end_day = max(idx))
    }))
  })
  rows <- Filter(Negate(is.null), rows)
  if (!length(rows)) return(data.frame(species=character(), start_day=integer(), end_day=integer()))
  result <- do.call(rbind, rows); rownames(result) <- NULL; result
}

.etn_survey_overlap <- function(p) {
  runs <- .etn_presence_runs(p)
  if (!nrow(runs)) return(list(edges=NULL, weights=numeric()))
  # Unique episode IDs let the interval API remain one interval per identifier.
  episodes <- runs
  episodes$species <- paste0("episode_", seq_len(nrow(runs)))
  overlap <- check_temporal_overlap(episodes)
  if (is.null(overlap$edges)) return(overlap)
  first_id <- match(overlap$edges[, 1L], episodes$species)
  second_id <- match(overlap$edges[, 2L], episodes$species)
  from <- match(runs$species[first_id], colnames(p))
  to <- match(runs$species[second_id], colnames(p))
  keep <- from != to
  from <- from[keep]; to <- to[keep]; weights <- overlap$weights[keep]
  if (!length(from)) return(list(edges=NULL, weights=numeric()))
  # Aggregate disjoint episode intersections; no day can be counted twice.
  low <- pmin(from, to); high <- pmax(from, to)
  key <- paste(low, high, sep=":")
  unique_pairs <- unique(data.frame(low=low, high=high, key=key))
  unique_pairs <- unique_pairs[order(unique_pairs$low, unique_pairs$high), , drop=FALSE]
  summed <- rowsum(weights, group=key, reorder=FALSE)
  weights <- as.numeric(summed[match(unique_pairs$key, rownames(summed)), 1L])
  species <- colnames(p)
  first <- vapply(species, function(sp) min(runs$start_day[runs$species==sp], Inf), numeric(1))
  last <- vapply(species, function(sp) max(runs$end_day[runs$species==sp], -Inf), numeric(1))
  rank <- integer(length(species)); rank[order(first,last,species)] <- seq_along(species)
  from <- unique_pairs$low; to <- unique_pairs$high
  swap <- rank[from] > rank[to]
  tmp <- from[swap]; from[swap] <- to[swap]; to[swap] <- tmp
  list(edges=cbind(species[from], species[to]), weights=weights)
}

.etn_graph_from_overlap <- function(overlap, species, direction, presence = NULL) {
  edges <- data.frame(from=character(), to=character(), weight=numeric())
  if (!is.null(overlap$edges) && nrow(overlap$edges)) {
    edges <- data.frame(from=overlap$edges[,1L], to=overlap$edges[,2L], weight=overlap$weights)
  }
  # This is the single graph assembly point for interval, survey and null inputs.
  g <- igraph::graph_from_data_frame(edges, directed=direction=="onset",
                                    vertices=data.frame(name=species))
  igraph::E(g)$overlap_days <- igraph::E(g)$weight
  if (!is.null(presence)) {
    pairs <- igraph::as_edgelist(g, names=TRUE)
    support <- integer(nrow(pairs)); jaccard <- numeric(nrow(pairs))
    for (k in seq_len(nrow(pairs))) {
      a <- presence[, pairs[k,1L]]; b <- presence[, pairs[k,2L]]
      known <- !is.na(a) & !is.na(b)
      support[k] <- sum(known)
      # Numerator reuses the existing overlap calculation, not a second count.
      jaccard[k] <- igraph::E(g)$weight[k] / sum(a[known]==1L | b[known]==1L)
    }
    igraph::E(g)$jointly_known_days <- support
    igraph::E(g)$overlap_jaccard <- jaccard
  }
  g
}

# Register existing dplyr data-mask symbols for package static checking.
utils::globalVariables(c("species", "start_day", "end_day", "next_start_day",
                         "next_end_day", "gap_to_next"))
