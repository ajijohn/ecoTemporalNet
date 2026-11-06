# Internal helpers use a distinct prefix to avoid changing existing functions.
.etn_seed <- function(seed, fun) {
  if (is.null(seed)) return(fun())
  if (length(seed) != 1L || !is.numeric(seed) || !is.finite(seed) ||
      seed < 0 || seed != floor(seed) || seed > .Machine$integer.max) {
    stop("seed must be NULL or one nonnegative integer.")
  }
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (had_seed) old_seed <- get(".Random.seed", envir = .GlobalEnv)
  on.exit({
    if (had_seed) assign(".Random.seed", old_seed, envir = .GlobalEnv)
    else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
      rm(".Random.seed", envir = .GlobalEnv)
  })
  set.seed(seed)
  fun()
}

.etn_summary <- function(p, g) {
  positive <- rowSums(p == 1L, na.rm = TRUE)
  unknown <- rowSums(is.na(p))
  known_empty <- positive == 0L & unknown == 0L
  unresolved <- positive == 0L & unknown > 0L
  persistence <- calculate_persistence_metrics(g)
  degree <- if (igraph::is_directed(g)) calculate_avg_temporal_network_degree(g)
            else calculate_avg_temporal_degree(g)
  data.frame(
    n_species = ncol(p), window_days = nrow(p),
    n_edges = igraph::ecount(g),
    mean_degree = degree,
    connectance = if (ncol(p) > 1L) igraph::edge_density(g, loops = FALSE) else NA_real_,
    mean_overlap_days = persistence$avg_edge_persistence,
    days_with_presence = sum(positive > 0L),
    known_empty_days = sum(known_empty),
    unresolved_days = sum(unresolved),
    coverage_lower = mean(positive > 0L),
    coverage_upper = mean(positive > 0L | unresolved),
    unknown_fraction = mean(is.na(p))
  )
}

.etn_intervals <- function(p, dates) {
  runs <- .etn_presence_runs(p)
  data.frame(species = runs$species,
             start_date = dates[runs$start_day],
             end_date = dates[runs$end_day],
             duration_days = runs$end_day - runs$start_day + 1L,
             row.names = NULL)
}

#' Prepare and analyse community phenology surveys
#'
#' @param observations Data frame with species, date and presence. Optional site
#'   and year columns define groups. Presence is logical or numeric 0/1/NA.
#' @param method Either "observed" or "interval".
#' @param observation_window Optional two integer days of year, shared across
#'   groups. Otherwise each group uses its first and last survey dates.
#' @param direction "onset" (default) for onset-directed networks or
#'   "undirected" for symmetric co-occurrence networks.
#' @return An object of class eco_phenology containing original observations,
#'   settings, and a named list of group analyses. Each group contains prepared
#'   observations, daily presence, reconstructed episodes, an igraph graph,
#'   metadata, and a community summary.
#' @details
#' Species means presence of the selected phenological stage, not organismal
#' occurrence. Unrecorded species-date combinations and explicit NA remain
#' unknown with method = "observed"; absence is never inferred from an omitted
#' row. One row per site, year, species and date is required. Aggregate individual
#' observations before calling this function. A missing site column becomes
#' "community"; year is derived from dates and checked if supplied.
#'
#' The interval method assumes continuous presence from first to last recorded
#' presence and absence outside that span within the window. An observed zero
#' inside the span is rejected. Species with no positive record retain their
#' original observations and unknown days. This assumption is stored in settings.
#'
#' Graphs are built with create_temporal_network(), which reuses the package's
#' check_temporal_overlap() calculation on positive episodes. Graphs include every recorded species and connect pairs sharing at least one
#' presence day. By default, edges point from earlier to later first presence
#' in the analysis matrix. Equal first-presence dates are ordered by earlier
#' last presence, then species name. This deterministic tie-breaking convention
#' does not imply biological precedence. Repeated episodes use the first and
#' last presence across the whole window, not episode-specific directions.
#' With observed data, first presence is an observed onset, not necessarily the
#' true onset. Use direction = "undirected" to ignore onset ordering.
#' weight and overlap_days count shared days;
#' overlap_jaccard is intersection divided by union on jointly known days only.
#' These are potential co-occurrence connections, not measured interactions.
#' With unknown values, absent edges do not prove biological non-overlap.
#' Coverage bounds report confirmed presence days and the maximum possible
#' coverage after unresolved days are included. Bounds are not confidence limits.
#' @examples
#' surveys <- data.frame(
#'   species = rep(c("A", "B"), each = 3),
#'   date = rep(c("2026-04-01", "2026-04-03", "2026-04-05"), 2),
#'   presence = c(1, 1, 0, 0, 1, 1)
#' )
#' x <- prepare_phenology(surveys)
#' summarise_phenology_networks(x)
#' x_interval <- prepare_phenology(surveys, method = "interval")
#' x_undirected <- prepare_phenology(surveys, direction = "undirected")
#' @export
prepare_phenology <- function(observations, method = c("observed", "interval"),
                              observation_window = NULL,
                              direction = c("onset", "undirected")) {
  method <- match.arg(method)
  direction <- match.arg(direction)
  if (!requireNamespace("igraph", quietly = TRUE)) stop("igraph is required.")
  if (!is.data.frame(observations) || !nrow(observations))
    stop("observations must be a nonempty data.frame.")
  missing <- setdiff(c("species", "date", "presence"), names(observations))
  if (length(missing)) stop("Missing required columns: ", paste(missing, collapse = ", "))
  d <- observations
  d$species <- as.character(d$species)
  if (anyNA(d$species) || any(!nzchar(trimws(d$species)))) stop("species must be nonempty and non-missing.")
  d$date <- .etn_date(d$date)
  if (!(is.numeric(d$presence) || is.logical(d$presence)) ||
      any(!is.na(d$presence) & !(d$presence %in% c(0, 1))))
    stop("presence must be logical or numeric 0, 1, or NA.")
  d$presence <- as.integer(d$presence)
  if (!"site" %in% names(d)) d$site <- "community"
  d$site <- as.character(d$site)
  if (anyNA(d$site) || any(!nzchar(trimws(d$site)))) stop("site must be nonempty and non-missing.")
  year <- as.integer(format(d$date, "%Y"))
  if ("year" %in% names(d) && any(is.na(d$year) | as.character(d$year) != as.character(year)))
    stop("year must match the calendar year of date.")
  d$year <- year
  if (anyDuplicated(d[c("site", "year", "species", "date")]))
    stop("Duplicate species-date rows within a site/year; aggregate individuals first.")
  if (!is.null(observation_window)) {
    w <- observation_window
    if (!is.numeric(w) || length(w) != 2L || any(!is.finite(w)) ||
        any(w != floor(w)) || w[1L] < 1L || w[2L] > 366L || w[1L] > w[2L])
      stop("observation_window must be two increasing integer days of year in 1:366.")
  }
  groups <- unique(d[c("site", "year")])
  groups <- groups[order(groups$site, groups$year), , drop = FALSE]
  analyses <- lapply(seq_len(nrow(groups)), function(k) {
    group <- groups[k, , drop = FALSE]
    z <- d[d$site == group$site & d$year == group$year, , drop = FALSE]
    z <- z[order(z$date, z$species), , drop = FALSE]; rownames(z) <- NULL
    if (is.null(observation_window)) {
      bounds <- range(z$date)
    } else {
      origin <- as.Date(paste0(group$year, "-01-01"))
      bounds <- origin + observation_window - 1L
      if (any(format(bounds, "%Y") != as.character(group$year)))
        stop("observation_window exceeds calendar year for group ", group$site, "/", group$year, ".")
      if (any(z$date < bounds[1L] | z$date > bounds[2L]))
        stop("Observations fall outside observation_window.")
    }
    dates <- seq(bounds[1L], bounds[2L], by = "day")
    species <- sort(unique(z$species))
    p <- matrix(NA_integer_, length(dates), length(species),
                dimnames = list(as.character(dates), species))
    p[cbind(match(z$date, dates), match(z$species, species))] <- z$presence
    observed <- p
    if (method == "interval") {
      for (j in seq_along(species)) {
        active <- which(!is.na(p[, j]) & p[, j] == 1L)
        if (!length(active)) next
        span <- seq.int(min(active), max(active))
        if (any(p[span, j] == 0L, na.rm = TRUE))
          stop("Observed absence inside first-to-last presence span for ",
               species[j], " in ", group$site, "/", group$year,
               "; use method = 'observed' or resolve repeated episodes explicitly.")
        # Reuse the existing interval-to-daily-presence function for reconstruction.
        filled <- get_daywise_overlap(data.frame(
          species = species[j], start_day = min(active), end_day = max(active)))
        p[, j] <- 0L
        p[as.integer(rownames(filled)), j] <- filled[, 1L]
      }
    }
    g <- create_temporal_network(presence_matrix = p, direction = direction)
    list(metadata = data.frame(site = group$site, year = group$year,
                               start_date = bounds[1L], end_date = bounds[2L]),
         observations = z, observed_presence = observed, presence = p,
         dates = dates, intervals = .etn_intervals(p, dates),
         graph = g, summary = .etn_summary(p, g))
  })
  names(analyses) <- paste0("group_", seq_along(analyses))
  structure(list(observations = observations, groups = analyses,
                 settings = list(method = method, observation_window = observation_window,
                   direction = direction, directed = direction == "onset",
                   onset_ties = "earlier last presence, then species name",
                   weight = "overlap_days",
                   missing = "unknown; interval mode reconstructs species with positive records")),
            class = "eco_phenology")
}

#' Extract consistent community or species summaries
#' @param x An eco_phenology object from prepare_phenology().
#' @param level Either "community" or "species".
#' @return A data frame with site/year labels and summaries.
#' @details
#' Community summaries include all recorded species, including isolates and
#' species never recorded present. Unknown days are retained in coverage bounds.
#' Species summaries report degree, known presence days, unknown days, and days
#' when a species is the only species present and all others are known absent.
#' Degree is total in-degree plus out-degree for directed graphs and ordinary
#' degree for undirected graphs. Species tables additionally report in_degree
#' and out_degree for onset-directed graphs; these are NA for undirected graphs.
#' Connectance divides edge count by n * (n - 1) for directed graphs or
#' n * (n - 1) / 2 for undirected graphs. Because onset ordering retains only
#' one edge per overlapping pair, onset-directed connectance cannot exceed 0.5.
#' It is not rescaled to the number of allowed forward pairs. Mean overlap is
#' the mean across retained edges. Coverage is independent of direction.
#' Network degree and edge persistence reuse calculate_node_temporal_degree(),
#' calculate_avg_temporal_network_degree(), calculate_avg_temporal_degree(),
#' and calculate_persistence_metrics(). Only survey-specific coverage and missing
#' observation summaries are calculated here.
#' Summaries describe the observation or reconstruction method stored in x.
#' Site/year groups are descriptive units, not assumed independent replicates.
#' @examples
#' d <- data.frame(species = c("A", "B"), date = "2026-04-01", presence = c(1, 0))
#' x <- prepare_phenology(d)
#' summarise_phenology_networks(x)
#' summarise_phenology_networks(x, level = "species")
#' @export
summarise_phenology_networks <- function(x, level = c("community", "species")) {
  if (!inherits(x, "eco_phenology")) stop("x must be an eco_phenology object.")
  level <- match.arg(level)
  rows <- lapply(x$groups, function(a) {
    if (level == "community") return(cbind(a$metadata, a$summary))
    p <- a$presence
    exclusive <- rowSums(p == 1L, na.rm = TRUE) == 1L & rowSums(is.na(p)) == 0L
    degree_values <- function(mode) {
      values <- calculate_node_temporal_degree(a$graph, mode = mode)
      values$degree[match(colnames(p), values$species)]
    }
    tab <- data.frame(species = colnames(p),
      degree = degree_values("total"),
      in_degree = if (igraph::is_directed(a$graph)) degree_values("in")
                  else rep(NA_real_, ncol(p)),
      out_degree = if (igraph::is_directed(a$graph)) degree_values("out")
                   else rep(NA_real_, ncol(p)),
      presence_days = colSums(p == 1L, na.rm = TRUE),
      unknown_days = colSums(is.na(p)),
      exclusive_presence_days = colSums((p == 1L) & exclusive, na.rm = TRUE),
      row.names = NULL)
    cbind(a$metadata[rep(1L, nrow(tab)), , drop = FALSE], tab)
  })
  out <- do.call(rbind, rows); rownames(out) <- NULL; out
}

#' Compare phenological structure with randomized interval placement
#' @param x An eco_phenology object.
#' @param nsim Number of randomizations, at least two.
#' @param seed Optional nonnegative integer seed. A supplied seed does not alter
#'   the caller's random-number state.
#' @return A list with settings, group-labelled observed/null summaries, and all
#'   simulation metrics. Reference quantiles are not confidence intervals.
#' @details
#' Requires complete daily presence matrices and at most one continuous presence
#' episode per species. Unknown histories and repeated episodes are rejected.
#' Within each site's yearly window, each species with presence is independently
#' assigned a uniformly sampled feasible onset, preserving its interval duration.
#' Species with no presence remain absent; the recorded species pool is fixed.
#' Intervals never wrap around the window boundary. Every replicate rebuilds the
#' graph using the direction setting stored in x. Onset direction and tie
#' ordering are recalculated from each randomized presence matrix; observed
#' edge directions are not held fixed. Older objects without a direction setting
#' must be rebuilt with prepare_phenology().
#'
#' Reports mean degree, connectance, mean overlap duration and flowering coverage.
#' Mean overlap is undefined for edgeless graphs; undefined replicates are counted
#' and omitted from that metric's descriptive null summaries. Standardized effects
#' are undefined for zero null variance. Tail probabilities are reported only
#' when every simulation and the observed value are finite; add-one corrections
#' are used. The upper/lower tails are one-sided, with no multiplicity correction.
#'
#' This reference model conditions on durations and window, not seasonal climate,
#' abundance, observation error, or interaction mechanisms. In interval mode it
#' also conditions on the chosen reconstruction. It does not reproduce a survey
#' sampling process and should not be interpreted as evidence of causation.
#' @examples
#' d <- data.frame(
#'   species = c("A", "A", "B", "B"),
#'   date = c("2026-04-01", "2026-04-05", "2026-04-03", "2026-04-07"),
#'   presence = 1
#' )
#' x <- prepare_phenology(d, method = "interval")
#' result <- test_phenological_structure(x, nsim = 19, seed = 42)
#' result$summary
#' @export
test_phenological_structure <- function(x, nsim = 999L, seed = NULL) {
  if (!inherits(x, "eco_phenology")) stop("x must be an eco_phenology object.")
  if (length(nsim) != 1L || !is.numeric(nsim) || !is.finite(nsim) ||
      nsim < 2L || nsim != floor(nsim) || nsim > .Machine$integer.max)
    stop("nsim must be an integer of at least two.")
  direction <- x$settings$direction
  if (length(direction) != 1L || !direction %in% c("onset", "undirected"))
    stop("Rebuild x with prepare_phenology() to record its direction setting.")
  for (a in x$groups) {
    if (!identical(igraph::is_directed(a$graph), direction == "onset"))
      stop("Graph direction does not match x$settings$direction; rebuild x.")
  }
  metrics <- c("mean_degree", "connectance", "mean_overlap_days", "coverage_lower")
  # Validate every group before consuming random numbers.
  for (a in x$groups) {
    p <- a$presence
    if (anyNA(p)) stop("Null comparisons require complete daily histories; unknown days remain.")
    for (j in seq_len(ncol(p))) {
      active <- which(p[, j] == 1L)
      if (length(active) && length(active) != max(active) - min(active) + 1L)
        stop("Null comparisons require at most one continuous presence episode per species.")
    }
  }
  .etn_seed(seed, function() {
    summaries <- list(); simulations <- list()
    for (k in seq_along(x$groups)) {
      a <- x$groups[[k]]; p <- a$presence
      duration <- colSums(p == 1L)
      sim <- do.call(rbind, lapply(seq_len(nsim), function(i) {
        q <- matrix(0L, nrow(p), ncol(p), dimnames = dimnames(p))
        for (j in seq_len(ncol(p))) {
          if (!duration[j]) next
          start <- sample.int(nrow(p) - duration[j] + 1L, 1L)
          q[seq.int(start, length.out = duration[j]), j] <- 1L
        }
        g <- create_temporal_network(presence_matrix = q, direction = direction)
        values <- .etn_summary(q, g)[metrics]
        data.frame(simulation = i, values)
      }))
      simulations[[k]] <- cbind(a$metadata[rep(1L, nsim), , drop = FALSE], sim)
      summaries[[k]] <- do.call(rbind, lapply(metrics, function(metric) {
        observed <- a$summary[[metric]]
        v <- sim[[metric]]; valid <- v[is.finite(v)]
        mu <- if (length(valid)) mean(valid) else NA_real_
        sigma <- if (length(valid) > 1L) stats::sd(valid) else NA_real_
        quant <- if (length(valid)) stats::quantile(valid, c(.025, .975), names = FALSE) else c(NA_real_, NA_real_)
        complete <- length(valid) == nsim && is.finite(observed)
        tab <- data.frame(metric = metric, observed = observed,
          null_mean = mu, null_sd = sigma, null_lower = quant[1L], null_upper = quant[2L],
          n_valid = length(valid), n_undefined = nsim - length(valid),
          standardized_effect = if (is.finite(observed) && is.finite(sigma) && sigma > 0) (observed - mu) / sigma else NA_real_,
          p_upper = if (complete) (1 + sum(valid >= observed)) / (nsim + 1) else NA_real_,
          p_lower = if (complete) (1 + sum(valid <= observed)) / (nsim + 1) else NA_real_)
        cbind(a$metadata, tab)
      }))
    }
    summary <- do.call(rbind, summaries); rownames(summary) <- NULL
    simulations <- do.call(rbind, simulations); rownames(simulations) <- NULL
    list(settings = list(model = "independent_uniform_onset_fixed_duration",
                          nsim = nsim, seed = seed, preparation = x$settings),
         summary = summary, simulations = simulations)
  })
}
