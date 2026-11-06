
# Internal helper: accept numeric days or dates within one calendar year.
prepare_species_data <- function(species_data) {
  if (!is.data.frame(species_data)) {
    stop("species_data must be a data.frame.")
  }

  if (!"species" %in% names(species_data)) {
    stop("species_data is missing required columns: species.")
  }

  date_cols <- c("start_date", "end_date")

  # Date columns take precedence over existing day columns.
  if (any(date_cols %in% names(species_data))) {
    if (!all(date_cols %in% names(species_data))) {
      stop("Provide both start_date and end_date.")
    }

    start_date <- .etn_date(species_data$start_date)
    end_date <- .etn_date(species_data$end_date)

    if (anyNA(start_date) || anyNA(end_date)) {
      stop("Dates must be non-missing and valid YYYY-MM-DD dates.")
    }

    if (any(start_date > end_date)) {
      stop("start_date must be on or before end_date.")
    }

    years <- unique(c(
      format(start_date, "%Y"),
      format(end_date, "%Y")
    ))

    if (length(years) > 1L) {
      stop("All dates must belong to the same calendar year.")
    }

    species_data$start_day <- as.integer(format(start_date, "%j"))
    species_data$end_day <- as.integer(format(end_date, "%j"))
  }

  required_cols <- c("start_day", "end_day")
  missing_cols <- setdiff(required_cols, names(species_data))

  if (length(missing_cols) > 0L) {
    stop(
      "species_data is missing required columns: ",
      paste(missing_cols, collapse = ", ")
    )
  }

  for (column in required_cols) {
    values <- species_data[[column]]

    if (!is.numeric(values) ||
        any(!is.finite(values)) ||
        any(values != floor(values))) {
      stop(column, " must contain finite, non-missing integer day values.")
    }
  }

  if (any(species_data$start_day > species_data$end_day)) {
    stop("start_day must be on or before end_day.")
  }

  species_data$species <- as.character(species_data$species)
  if (anyNA(species_data$species) || any(!nzchar(trimws(species_data$species)))) {
    stop("species must contain nonempty, non-missing identifiers.")
  }
  species_data <- species_data[
    order(species_data$start_day, species_data$end_day, species_data$species),
    , drop = FALSE
  ]
  rownames(species_data) <- NULL
  species_data
}

#' Create the time-ordered network by checking temporal overlap
#' @param species_data A data.frame with species and start_day/end_day, or
#'   start_date/end_date within a single calendar year.
#' @details
#' Accepts one continuous interval per species, using integer start_day/end_day
#' columns or start_date/end_date columns of Date values or YYYY-MM-DD strings.
#' Date columns must refer to one calendar year and take precedence over day
#' columns. Both endpoints count as present; intervals meeting on a shared day
#' have one day of overlap.
#'
#' The shared input helper sorts by start_day, end_day, then species. Each pair
#' is examined once with the original inclusive interval-intersection formula.
#' Edges point from earlier to later onset. Equal onsets use earlier end, then
#' species name: this is a deterministic convention, not biological precedence.
#' Duplicate species identifiers are rejected. Zero or one species yields no
#' edges. With no overlaps, edges is NULL and weights is empty.
#'
#' This function is also the overlap engine for survey episodes supplied through
#' create_temporal_network(). For repeated episodes or incomplete daily surveys,
#' use that function's presence_matrix input or prepare_phenology(), rather than
#' collapsing records into a single first-to-last presence interval.
#' @examples
#' species_data <- data.frame(
#'   species = c("A", "B", "C"),
#'   start_day = c(1, 4, 7),
#'   end_day = c(5, 8, 10)
#' )
#' check_temporal_overlap(species_data)
#'
#' # Sorting the rows differently does not change the result.
#' check_temporal_overlap(species_data[c(3, 1, 2), ])
#'
#' # Calendar dates use the same inclusive-day calculation.
#' dates <- data.frame(
#'   species = c("A", "B"),
#'   start_date = c("2026-04-01", "2026-04-04"),
#'   end_date = c("2026-04-05", "2026-04-08")
#' )
#' check_temporal_overlap(dates) # A -> B, two shared days
#' @export
check_temporal_overlap <- function(species_data) {
  edges <- list()
  weights <- c()  # Store the edge weights (duration of overlap)

  #https://github.com/ajijohn/ecoTemporalNet/issues/5
  # Support for survey dates in calendar format

  species_data <- prepare_species_data(species_data)
  if (anyDuplicated(species_data$species)) {
    stop("Provide one interval per species; use presence_matrix for repeated episodes.")
  }
  if (nrow(species_data) < 2L) {
    return(list(edges = NULL, weights = numeric()))
  }

  for (i in 1:(nrow(species_data) - 1)) {
    for (j in (i + 1):nrow(species_data)) {
      species_i <- species_data[i, ]
      species_j <- species_data[j, ]

      # Check if there is a temporal overlap between species i and j
      # The overlap condition is if one species ends after the other starts
      # third condition -  # A → B only if A starts no later than B (Enforce time-respecting direction)
      if (species_i$end_day >= species_j$start_day &&
          species_i$start_day <= species_j$end_day &&
          species_i$start_day <= species_j$start_day) {
        overlap_start <- max(species_i$start_day, species_j$start_day)
        overlap_end <- min(species_i$end_day, species_j$end_day)
        overlap_duration <- overlap_end - overlap_start + 1  # Calculate overlap duration in days
        edges <- append(edges, list(c(species_i$species, species_j$species)))  # Create directed edge
        weights <- c(weights, overlap_duration)  # Store the overlap duration as edge weight
      }
    }
  }

  return(list(edges = do.call(rbind, edges), weights = weights))
}

#' Create a temporal ecological network
#'
#' @param species_data A data.frame of species intervals, or NULL when supplying
#'   presence_matrix.
#' @param direction "onset" (default) or "undirected".
#' @param presence_matrix Optional daily matrix of 0, 1, or NA, with unique species
#'   column names. Rows must be consecutive days in chronological order. Supply
#'   this instead of species_data for survey observations or repeated episodes.
#' @return An igraph object
#' @details
#' The interval and survey interfaces share one graph constructor and the
#' inclusive overlap calculation in check_temporal_overlap(). Supply species_data
#' or presence_matrix, not both. Every supplied species is retained as a vertex,
#' including isolates and species in an edgeless community.
#'
#' Interval inputs assume continuous presence of the selected phenological stage
#' between both endpoints. Daily input requires rows representing consecutive
#' calendar days, ordered chronologically. Include missing survey days explicitly
#' as NA rows; never remove them and compress time. Unique nonempty species column
#' names are required. The matrix interface uses row position for elapsed days;
#' when ISO date row names are supplied, consecutive chronological dates are checked.
#' Use prepare_phenology() to create this matrix from long-format survey records.
#'
#' Daily input is split into confirmed continuous positive episodes. Both zero and
#' NA interrupt episodes; NA remains unknown, not inferred absence. The existing
#' interval overlap function calculates episode intersections, which are summed
#' once per species pair without filling gaps. Default direction is earlier to
#' later first presence across the entire window. Equal onsets use earlier last
#' presence, then species name; this tie convention does not establish biological
#' precedence. Repeated episodes share that species-level direction. Set direction
#' to "undirected" to represent symmetric co-occurrence instead.
#'
#' Both weight and overlap_days store shared presence days. Daily input additionally
#' stores jointly_known_days and overlap_jaccard: overlap divided by the number of
#' days with either species present, restricted to jointly known days. Missingness
#' therefore affects support for overlap estimates. An absent edge in an incomplete
#' survey does not establish biological non-overlap. Graph weights describe temporal
#' co-occurrence opportunity rather than measured ecological interactions.
#' @examples
#' species_data <- data.frame(
#'   species = c("A", "B", "C"),
#'   start_day = c(1, 4, 7),
#'   end_day = c(5, 8, 10)
#' )
#' g <- create_temporal_network(species_data)
#' igraph::as_data_frame(g)
#'
#' # Direction is optional; overlap weights are unchanged.
#' g_undirected <- create_temporal_network(species_data, direction = "undirected")
#' igraph::is_directed(g_undirected)
#'
#' # Species without overlaps are retained.
#' with_isolate <- rbind(species_data,
#'   data.frame(species = "D", start_day = 20, end_day = 22))
#' g_with_isolate <- create_temporal_network(with_isolate)
#' igraph::degree(g_with_isolate)["D"] # zero
#'
#' # Daily survey input: rows are consecutive days; NA means unknown.
#' presence <- cbind(
#'   A = c(1L, NA_integer_, 0L, 1L, 1L),
#'   B = c(0L, 1L, 1L, 1L, 0L)
#' )
#' rownames(presence) <- as.character(as.Date("2026-04-01") + 0:4)
#' g_survey <- create_temporal_network(presence_matrix = presence)
#' igraph::as_data_frame(g_survey) # one shared day; the missing day is not filled
#'
#' # Long-format records can use the integrated survey workflow.
#' surveys <- data.frame(
#'   species = rep(c("A", "B"), each = 3),
#'   date = rep(c("2026-04-01", "2026-04-03", "2026-04-05"), 2),
#'   presence = c(1, 1, 0, 0, 1, 1)
#' )
#' analysis <- prepare_phenology(surveys)
#' analysis$groups[[1]]$graph
#' @export
create_temporal_network <- function(species_data = NULL,
                                    direction = c("onset", "undirected"),
                                    presence_matrix = NULL) {
  direction <- match.arg(direction)
  if (!is.null(presence_matrix)) {
    if (!is.null(species_data)) stop("Supply species_data or presence_matrix, not both.")
    p <- .etn_validate_presence(presence_matrix)
    overlap <- .etn_survey_overlap(p)
    return(.etn_graph_from_overlap(overlap, colnames(p), direction, p))
  }
  if (is.null(species_data)) stop("Supply species_data or presence_matrix.")
  species_data <- prepare_species_data(species_data)
  overlap <- check_temporal_overlap(species_data)
  .etn_graph_from_overlap(overlap, species_data$species, direction)
}

#' Plot a snapshot of the temporal network
#' @param g temporal graph
#' @details
#' Uses igraph conversion and plotting with a Fruchterman-Reingold layout. Edge
#' labels display overlap durations; widths are scaled relative to the largest
#' edge weight, to a maximum of five. This is a display of the aggregate graph,
#' not a series of daily snapshots.
#'
#' The current plotting implementation rebuilds a directed graph from the edge
#' list. Isolated vertices are therefore omitted from the display, and undirected
#' input is displayed with arrows. The underlying supplied graph is unchanged.
#' Use igraph::plot.igraph() directly when displaying isolates or undirected graphs.
#' A graph with at least one edge and positive weights is expected.
#' @examples
#' library(igraph)
#' species_data <- data.frame(
#'   species = c("A", "B", "C"),
#'   start_day = c(1, 4, 7),
#'   end_day = c(5, 8, 10)
#' )
#' g <- create_temporal_network(species_data)
#' plot_temporal_network(g)
#'
#' # Use igraph directly for an undirected display.
#' g_undirected <- create_temporal_network(species_data, direction = "undirected")
#' igraph::plot.igraph(g_undirected)
#' @export
plot_temporal_network <- function(g) {
  # Plot the network with edge weights (temporal overlap in days)
  plot(igraph::graph_from_data_frame(as_edgelist(g), directed = TRUE),
       vertex.size = 30,
       vertex.label.cex = 1.2,
       vertex.label.color = "black",
       edge.arrow.size = 0.5,
       edge.width = E(g)$weight / max(E(g)$weight) * 5,  # Adjust edge width by overlap duration
       edge.label = E(g)$weight,  # Show overlap duration (days) on edges
       main = "Time-Ordered Network: Species Temporal Overlap (Edge Weights = Overlap in Days)",
       layout = layout_with_fr)
}


#' Function to calculate distance (shortest paths with weights representing overlap duration)
#' @param g An igraph overlap network.
#'
#' @details
#' Wraps igraph::shortest.paths(), using the graph's weight attribute as an
#' additive path cost. Consequently, longer overlap durations contribute larger
#' costs; weights are not inverted. The default shortest-path mode treats the
#' graph as undirected. Returns a matrix of distances, not the paths themselves.
#' Unreachable pairs have infinite distance. These costs do not represent
#' elapsed-time travel through the flowering season.
#' @examples
#' library(igraph)
#' species_data <- data.frame(
#'   species = c("A", "B", "C"),
#'   start_day = c(1, 4, 7),
#'   end_day = c(5, 8, 10)
#' )
#' g <- create_temporal_network(species_data)
#' # Each overlap costs two days; the A-C distance is four.
#' calculate_weighted_overlap_distance(g)
#' @export
calculate_weighted_overlap_distance <- function(g) {
  distance_matrix <- shortest.paths(g, weights = E(g)$weight)  # Shortest paths with overlap duration as weights
  return(distance_matrix)
}

#' Function to calculate distance (smallest number of nodes, hops, between species)
#' @param g An igraph overlap network.
#'
#' @details
#' Wraps igraph::shortest.paths() with mode = "all", ignoring edge direction.
#' In the current implementation, weights = NULL uses the graph's weight
#' attribute when present. Thus a graph created by create_temporal_network()
#' produces overlap-weighted distances rather than unweighted hop counts.
#' Without a weight attribute, distances count edges. Unreachable pairs have
#' infinite distance.
#' @examples
#' library(igraph)
#' species_data <- data.frame(
#'   species = c("A", "B", "C"),
#'   start_day = c(1, 4, 7),
#'   end_day = c(5, 8, 10)
#' )
#' g <- create_temporal_network(species_data)
#' # Current implementation uses existing weights: A-C distance is four.
#' calculate_overlap_hop_distance(g)
#' # Remove the weight attribute for unweighted hops: A-C distance is two.
#' g_unweighted <- igraph::delete_edge_attr(g, "weight")
#' calculate_overlap_hop_distance(g_unweighted)
#' @export
calculate_overlap_hop_distance <- function(g) {
  distance_matrix <- shortest.paths(g, v = V(g), mode = "all", weights = NULL)  # Shortest paths (unweighted)
  return(distance_matrix)
}

#' Calculate directed latency between species
#'
#' By default, latency is the minimum number of directed overlap steps
#' needed to reach one species from another.
#'
#' If use_overlap_strength = TRUE, edge cost is 1 / overlap_duration so
#' longer overlaps imply lower traversal cost.
#'
#' @param g A directed igraph object from create_temporal_network()
#' @param use_overlap_strength Logical; if TRUE use 1/weight as edge cost.
#' @return A matrix of directed latencies
#' @details
#' Wraps igraph::distances() with mode = "out", following outgoing edges.
#' With use_overlap_strength = FALSE, edge weights are ignored and distances
#' count directed edges. With TRUE, costs are inverse overlap durations and
#' must be positive and finite. Unreachable pairs have infinite distance.
#'
#' Latency here denotes a graph distance, not elapsed time or an earliest
#' arrival time. A graph with no vertices produces a zero-by-zero matrix.
#' @examples
#' species_data <- data.frame(
#'   species = c("A", "B", "C"),
#'   start_day = c(1, 4, 7),
#'   end_day = c(5, 8, 10)
#' )
#' g <- create_temporal_network(species_data)
#' calculate_latency(g) # A reaches C in two directed steps; C cannot reach A.
#' calculate_latency(g, use_overlap_strength = TRUE) # inverse-overlap costs
#' @export
calculate_latency <- function(g, use_overlap_strength = FALSE) {
  if (igraph::vcount(g) == 0) {
    return(matrix(
      numeric(0),
      nrow = 0,
      ncol = 0,
      dimnames = list(character(), character())
    ))
  }

  if (use_overlap_strength) {
    w <- igraph::E(g)$weight
    if (is.null(w) || any(!is.finite(w)) || any(w <= 0)) {
      stop("All edge weights must be positive and finite when use_overlap_strength = TRUE.")
    }
    cost <- 1 / w
  } else {
    cost <- NA  # unweighted directed shortest paths = hop latency
  }

  igraph::distances(g, mode = "out", weights = cost)
}


#' Function to compute shortest paths between all species pairs
#' @param g An igraph overlap network.
#' @details
#' Wraps igraph::shortest.paths() and returns a matrix of shortest-path
#' lengths, not vertex or edge sequences. Default mode treats edges as
#' undirected. Because weights = NULL, an existing weight attribute is used
#' as additive cost; otherwise paths are unweighted. Unreachable pairs have
#' infinite distance.
#' @examples
#' library(igraph)
#' species_data <- data.frame(
#'   species = c("A", "B", "C"),
#'   start_day = c(1, 4, 7),
#'   end_day = c(5, 8, 10)
#' )
#' g <- create_temporal_network(species_data)
#' # Returns distances, using overlap weights and ignoring direction.
#' calculate_overlap_shortest_paths(g)
#' @export
calculate_overlap_shortest_paths <- function(g) {
  # Calculate all shortest paths between every pair of nodes
  shortest_paths_matrix <- shortest.paths(g, weights = NULL)
  return(shortest_paths_matrix)
}

#' Create a function to calculate edge persistence (pairwise overlap duration)
#' @param species_data Species dataframe
#' @details
#' Delegates to check_temporal_overlap() rather than maintaining a second
#' intersection algorithm. Accepts one continuous interval per species, in numeric
#' days or single-calendar-year dates. Inclusive overlap days are returned for
#' overlapping pairs only. Pair labels follow onset order and its end-date/name
#' tie convention; overlap duration itself is symmetric. With no overlaps, edges
#' is NULL and persistence is empty. To obtain overlap durations for incomplete
#' or repeated-episode surveys, build a graph with create_temporal_network() or
#' prepare_phenology() and inspect its weight attribute.
#' @examples
#' species_data <- data.frame(
#'   species = c("A", "B", "C"),
#'   start_day = c(1, 4, 7),
#'   end_day = c(5, 8, 10)
#' )
#' calculate_edge_persistence(species_data)
#' # Edge persistence reuses the same overlaps and ordering.
#' check_temporal_overlap(species_data)
#' @export
calculate_edge_persistence <- function(species_data) {
  overlap <- check_temporal_overlap(species_data)
  list(edges = overlap$edges, persistence = overlap$weights)
}

#' Calculate node betweenness centrality in the directed overlap network
#' @importFrom stats setNames
#'
#' @param g A directed igraph object from create_temporal_network()
#' @param use_overlap_strength Logical; if TRUE use 1/weight as edge cost
#'   so stronger overlaps are treated as shorter paths.
#' @param normalized Logical; return normalized betweenness if TRUE.
#' @return A named numeric vector of betweenness values
#' @details
#' Wraps igraph::betweenness() with directed = TRUE. With
#' use_overlap_strength = TRUE, inverse overlap durations are shortest-path
#' costs, so longer overlaps have lower cost. With FALSE, the current
#' implementation passes weights = NULL, which uses an existing graph weight
#' attribute rather than disabling weights. The normalized argument is passed
#' directly to igraph. A graph with no vertices returns an empty named vector.
#'
#' This is betweenness on the aggregated overlap graph, not a calculation of
#' time-dependent movement or elapsed-time paths.
#' @examples
#' species_data <- data.frame(
#'   species = c("A", "B", "C"),
#'   start_day = c(1, 4, 7),
#'   end_day = c(5, 8, 10)
#' )
#' g <- create_temporal_network(species_data)
#' calculate_betweenness(g)
#' calculate_betweenness(g, normalized = FALSE)
#' # FALSE currently uses existing overlap weights, not unweighted paths.
#' calculate_betweenness(g, use_overlap_strength = FALSE)
#' @export
calculate_betweenness <- function(g,
                                  use_overlap_strength = TRUE,
                                  normalized = TRUE) {
  if (igraph::vcount(g) == 0) {
    return(setNames(numeric(0), character(0)))
  }

  weights <- NULL
  if (use_overlap_strength) {
    w <- igraph::E(g)$weight
    if (is.null(w) || any(!is.finite(w)) || any(w <= 0)) {
      stop("All edge weights must be positive and finite when use_overlap_strength = TRUE.")
    }
    weights <- 1 / w
  }

  igraph::betweenness(
    g,
    directed = TRUE,
    weights = weights,
    normalized = normalized
  )
}


#' Calculate signed interval separation to the next species in start-day order
#'
#' Positive values mean a gap.
#' Zero means consecutive intervals have no intervening unoccupied day.
#' Negative values mean overlap.
#'
#' @param species_data Data frame with species, start_day, end_day
#' @return species_data ordered by start_day with new columns describing
#'   the next interval and signed separation
#' @details
#' Accepts numeric day columns or date columns within one calendar year and
#' orders rows by start_day, end_day, and species. Intervals include both
#' endpoints. Signed separation is next_start_day - end_day - 1: positive
#' values count intervening unoccupied days, zero indicates consecutive days
#' without a shared day, and negative values indicate overlap.
#'
#' Actual overlap uses the earlier end date, including for nested intervals.
#' Its magnitude need not equal the absolute signed separation. Comparisons
#' are between consecutive rows in onset order, not gaps in the union of all
#' species' intervals. The last row has missing next-interval summaries.
#' @examples
#' library(dplyr)
#' species_data <- data.frame(
#'   species = c("A", "B", "C"),
#'   start_day = c(1, 4, 10),
#'   end_day = c(5, 8, 15)
#' )
#' inter_event_times(species_data) # signed gaps: -2, 1, NA
#'
#' # A short interval nested within a longer one shares only its own days.
#' nested <- data.frame(
#'   species = c("A", "B"), start_day = c(1, 4), end_day = c(10, 5)
#' )
#' inter_event_times(nested) # overlap duration: 2, NA
#' @export
inter_event_times <- function(species_data) {

  species_data <- prepare_species_data(species_data)
  required_cols <- c("species", "start_day", "end_day")
  missing_cols <- setdiff(required_cols, names(species_data))
  if (length(missing_cols) > 0) {
    stop("species_data is missing required columns: ",
         paste(missing_cols, collapse = ", "))
  }

  species_data %>%
    dplyr::arrange(start_day, end_day, species) %>%
    dplyr::mutate(
      next_species      = dplyr::lead(species),
      next_start_day    = dplyr::lead(start_day),
      next_end_day      = dplyr::lead(end_day),
      gap_to_next       = next_start_day - end_day - 1L,
      gap_positive      = pmax(gap_to_next, 0L),
      # Fix the issue where there is an overlap
      # https://github.com/ajijohn/ecoTemporalNet/issues/7
      #overlap_duration  = pmax(0L, end_day - next_start_day + 1L)
      overlap_duration = pmax(
        0L,
        pmin(end_day, next_end_day) - next_start_day + 1L
      )
    )
}


#' Calculate lag-1 memory in interval gaps, discounted by network latency
#'
#' Steps:
#' 1. Order species by start_day
#' 2. Compute non-negative gaps between consecutive species
#' 3. Compute lag-1 correlation, weighted by directed latency between the
#'    corresponding species in the overlap network
#'
#' @param species_data Data frame with species, start_day, end_day
#' @return Numeric scalar in [-1, 1] or NA if too few usable intervals
#' @details
#' Obtains nonnegative consecutive-interval gaps from inter_event_times()
#' and directed hop distances from calculate_latency(). Adjacent gap pairs
#' are weighted by 1 / (1 + d), where d connects the first gap's source species
#' to the second gap's destination species. Unreachable pairs have zero weight
#' and are excluded. A weighted Pearson correlation is calculated from the
#' remaining pairs; insufficient pairs or zero variance produce NA.
#'
#' Under the intended onset-ordered overlap construction, positive gaps
#' prevent directed paths from crossing the corresponding onset boundary.
#' This can exclude the variation needed by the statistic and yield undefined
#' results. The weighting is not a measure of elapsed-time latency.
#' @examples
#' library(dplyr)
#' species_data <- data.frame(
#'   species = c("A", "B", "C"),
#'   start_day = c(1, 4, 7),
#'   end_day = c(5, 8, 10)
#' )
#' # The current weighting can leave no usable gap variance; NA is expected.
#' calculate_memory_with_latency(species_data)
#' @export
calculate_memory_with_latency <- function(species_data) {
  if (nrow(species_data) < 3) {
    return(NA_real_)
  }

  x <- inter_event_times(species_data)

  # Use non-negative gap lengths; overlaps count as 0 gap for this metric
  gaps <- x$gap_positive
  valid_gap_idx <- which(!is.na(gaps))

  if (length(valid_gap_idx) < 2) {
    return(NA_real_)
  }

  # Species pairs corresponding to each consecutive gap
  species_from <- x$species[valid_gap_idx]
  species_to   <- x$next_species[valid_gap_idx]

  g <- create_temporal_network(species_data)
  if (igraph::vcount(g) == 0) {
    return(NA_real_)
  }

  lat <- calculate_latency(g, use_overlap_strength = FALSE)

  # Need adjacent gap pairs: gaps[k] with gaps[k+1]
  gap1 <- gaps[valid_gap_idx[-length(valid_gap_idx)]]
  gap2 <- gaps[valid_gap_idx[-1]]

  from_nodes <- species_from[-length(species_from)]
  to_nodes   <- species_to[-1]

  # Weight by how close these linked species are in the directed network
  w <- mapply(
    function(a, b) {
      if (is.na(a) || is.na(b) || !(a %in% rownames(lat)) || !(b %in% colnames(lat))) {
        return(NA_real_)
      }
      d <- lat[a, b]
      if (!is.finite(d)) return(0)
      1 / (1 + d)
    },
    from_nodes,
    to_nodes
  )

  ok <- is.finite(gap1) & is.finite(gap2) & is.finite(w) & (w > 0)
  if (sum(ok) < 2) {
    return(NA_real_)
  }

  gap1 <- gap1[ok]
  gap2 <- gap2[ok]
  w    <- w[ok]

  mu1 <- stats::weighted.mean(gap1, w)
  mu2 <- stats::weighted.mean(gap2, w)

  sd1 <- sqrt(stats::weighted.mean((gap1 - mu1)^2, w))
  sd2 <- sqrt(stats::weighted.mean((gap2 - mu2)^2, w))

  if (sd1 == 0 || sd2 == 0) {
    return(NA_real_)
  }

  stats::weighted.mean((gap1 - mu1) * (gap2 - mu2), w) / (sd1 * sd2)
}

#' Calculate burstiness of overlap durations across network edges
#'
#' This metric summarizes how variable pairwise overlap durations are among
#' all overlapping species pairs. It uses the classic burstiness formula:
#' \deqn{B = (\sigma - \mu) / (\sigma + \mu)}
#' where \eqn{\mu} is the mean overlap duration and \eqn{\sigma} is the
#' standard deviation of overlap durations.
#'
#' @param species_data A data frame with columns `species`, `start_day`, and `end_day`.
#'
#' @return A numeric scalar in [-1, 1], or `NA_real_` if fewer than two
#'   overlapping pairs exist or if the statistic is undefined.
#'
#' @details
#' Uses inclusive overlap durations returned by calculate_edge_persistence(),
#' not waiting times between events. The standard deviation is calculated with
#' stats::sd(), using the sample standard-deviation convention. The result
#' summarizes heterogeneity among overlapping pairs; pairs sharing a species
#' are not independent observations. Fewer than two durations or undefined
#' moments produce NA.
#' @examples
#' species_data <- data.frame(
#'   species = c("A", "B", "C"),
#'   start_day = c(1, 4, 6),
#'   end_day = c(5, 8, 10)
#' )
#' calculate_edge_persistence(species_data)$persistence # two overlap durations
#' calculate_overlap_burstiness(species_data)
#' @export
calculate_overlap_burstiness <- function(species_data) {
  ep <- calculate_edge_persistence(species_data)

  if (is.null(ep) || is.null(ep$persistence)) {
    return(NA_real_)
  }

  x <- ep$persistence

  if (length(x) < 2) {
    return(NA_real_)
  }

  mu <- mean(x, na.rm = TRUE)
  sigma <- stats::sd(x, na.rm = TRUE)

  if (!is.finite(mu) || !is.finite(sigma)) {
    return(NA_real_)
  }

  if ((mu + sigma) == 0) {
    return(NA_real_)
  }

  (sigma - mu) / (sigma + mu)
}

#' Calculate burstiness (B) from non-negative interval gaps
#'
#' Uses gap_positive from inter_event_times():
#' - overlap => 0 waiting time
#' - gap => positive waiting time
#'
#' @param species_data Data frame with species, start_day, end_day
#' @return Numeric scalar in [-1, 1], or NA if too few usable gaps
#' @details
#' Uses nonnegative gaps between consecutive species in onset order from
#' inter_event_times(). Overlapping or adjacent intervals contribute zero;
#' positive gaps count intervening days. Applies (sd - mean) / (sd + mean)
#' using the sample standard deviation. Fewer than two usable gaps or a zero
#' denominator produce NA. These gaps are not necessarily periods without
#' flowering anywhere in the community.
#' @examples
#' library(dplyr)
#' species_data <- data.frame(
#'   species = c("A", "B", "C", "D"),
#'   start_day = c(1, 4, 10, 20),
#'   end_day = c(5, 8, 15, 22)
#' )
#' inter_event_times(species_data)$gap_positive # 0, 1, 4, NA
#' calculate_burstiness(species_data)
#' @export
calculate_burstiness <- function(species_data) {
  x <- inter_event_times(species_data)

  gaps <- x$gap_positive[!is.na(x$gap_positive)]

  if (length(gaps) < 2) {
    return(NA_real_)
  }

  m_tau <- mean(gaps)
  sigma_tau <- stats::sd(gaps)

  if (!is.finite(m_tau) || !is.finite(sigma_tau) || (m_tau + sigma_tau) == 0) {
    return(NA_real_)
  }

  (sigma_tau - m_tau) / (sigma_tau + m_tau)
}

#' Compute inter‐event durations from sorted event times
#' @param times Numeric vector of event times (will be sorted)
#' @return Numeric vector of inter‐event durations
#' @details
#' Coerces times to numeric, sorts them, and returns successive differences
#' using diff(). Tied times produce zero durations. Units follow the numeric
#' representation of the input; there is no inclusive-day adjustment. This
#' function operates on event times rather than flowering intervals.
#' @examples
#' times <- c(12, 1, 7, 3, 20)
#' interevent_durations(times) # 2, 4, 5, 8
#' @export
interevent_durations <- function(times) {
  t <- sort(as.numeric(times))
  diff(t)
}


#' Simple burstiness (Goh & Barabási, 2008)
#'
#' B = (σ – μ) / (σ + μ)
#' @param ied Numeric vector of inter‐event durations
#' @return Numeric scalar in [–1,1]
#' @details
#' Computes (sd - mean) / (sd + mean) from the supplied durations using
#' stats::sd(). Durations are expected to be finite and nonnegative; no sorting
#' or missing-value removal is performed. When mean and standard deviation
#' are both zero, the implementation returns 1 by convention. Insufficient
#' observations or missing values are not explicitly guarded against.
#' @examples
#' ied <- c(2, 4, 5, 8, 3)
#' burstiness_simple(ied)
#' burstiness_simple(c(3, 3, 3, 3)) # -1: equal durations
#' @export
burstiness_simple <- function(ied) {
  m <- mean(ied)
  s <- stats::sd(ied)
  # degenerate: all intervals zero
  if (m + s == 0) return(1)
  (s - m) / (s + m)
}



#' Finite‐size burstiness, unbounded case (Eq. 22, Kim & Jo 2016)
#'
#' Aₙ(r) = [√(n+1)·r − √(n−1)] / [r·(√(n+1)−2) + √(n−1)]
#' where r = CV = σ/μ
#' @param ied Numeric vector of inter‐event durations
#' @return Numeric scalar in [–1,1]
#' @details
#' Applies the displayed coefficient-of-variation transformation using the
#' sample standard deviation from stats::sd() and n = length(ied) + 1.
#' The input is a vector of durations rather than event times. A vector with
#' zero mean and zero standard deviation returns 1 by convention; other
#' zero-mean input is rejected. Missing values and insufficient observations
#' are not explicitly handled. The finite-size formula and sample-size
#' convention should be considered when interpreting results.
#' @examples
#' ied <- c(2, 4, 5, 8, 3)
#' burstiness_eq22(ied)
#' burstiness_eq22(c(3, 3, 3, 3)) # -1: equal durations
#' @export
burstiness_eq22 <- function(ied) {
  m <- mean(ied)
  s <- stats::sd(ied)
  if (m == 0 && s == 0) return(1)
  stopifnot(m != 0)
  cv <- s / m
  n  <- length(ied) + 1
  a <- sqrt(n + 1) * cv - sqrt(n - 1)
  b <- cv * (sqrt(n + 1) - 2) + sqrt(n - 1)
  a / b
}


#' Memory metric: lag-1 correlation of inter-event durations
#'
#' @param times Numeric vector of event times
#' @return Numeric scalar in [-1, 1], or NA if too few events
#' @details
#' Calls interevent_durations() to sort event times and form successive
#' durations, then computes the Pearson correlation between adjacent durations
#' with stats::cor(). Returns NA when there are fewer than two durations or
#' when a lagged vector has zero standard deviation. With exactly two
#' durations, lagged vectors each contain one value and the current
#' implementation can error. This statistic describes event-time ordering,
#' not network reachability.
#' @examples
#' times <- c(1, 3, 7, 12, 20, 23)
#' interevent_durations(times)
#' memory(times)
#' @export
memory <- function(times) {
  ied <- interevent_durations(times)

  if (length(ied) < 2) {
    return(NA_real_)
  }

  x <- ied[-length(ied)]
  y <- ied[-1]

  if (stats::sd(x) == 0 || stats::sd(y) == 0) {
    return(NA_real_)
  }

  stats::cor(x, y)
}


#' Build co-occurrence table by day
#'
#' @param species_data A data.frame of nodes
#' @return Day matrix
#' @details
#' Accepts a nonempty data frame of integer-day intervals or date intervals within
#' one calendar year. The shared helper orders species by start_day, end_day, and
#' species. Each interval is expanded into daily presence including both endpoints;
#' flowering is assumed continuous between those dates.
#'
#' Rows contain the union of represented days, with day-of-year labels for date
#' input. Days with no species present are omitted. Row order follows first
#' occurrence in expanded intervals and can be nonchronological when intervals
#' are nested; explicitly sort row labels if a chronological display is needed.
#' This matrix is suitable for interval-based coflowering summaries. It is not a
#' representation of observation effort and has no unknown states. Use
#' prepare_phenology() for survey data with omitted records, unknown days, or an
#' explicit observation window. The interval reconstruction option in that workflow
#' calls this function only after the continuity assumption has been chosen.
#' @examples
#' species_data <- data.frame(
#'   species = c("A", "B", "C"),
#'   start_day = c(1, 4, 7),
#'   end_day = c(5, 8, 10)
#' )
#' day_matrix <- get_daywise_overlap(species_data)
#' day_matrix
#' # This interval expansion has no unknown survey days.
#' calculate_prop_edge_persistence(day_matrix)
#' @export
get_daywise_overlap <- function(species_data) {

  species_data <- prepare_species_data(species_data)
  # Create a list of days for each species
  species_days <- lapply(1:nrow(species_data), function(i) {
    seq(species_data$start_day[i], species_data$end_day[i])
  })
  names(species_days) <- species_data$species

  # Build a day × species presence matrix
  all_days <- unique(unlist(species_days))
  day_matrix <- matrix(0, nrow = length(all_days), ncol = nrow(species_data),
                       dimnames = list(all_days, species_data$species))

  for (i in seq_along(species_days)) {
    days <- as.character(species_days[[i]])
    day_matrix[days, i] <- 1
  }

  return(day_matrix)
}

#' For each species, calculate the proportion of flowering days spent coflowering
#'
#' @param day_matrix A day x species presence matrix from get_daywise_overlap()
#' @return A data.frame with species and proportion of flowering days with >=1 neighbor
#' @details
#' Expects a binary day-by-species presence matrix with unique species column
#' names and no missing entries. For each species, divides the number of its
#' present days with at least one other species present by its total number
#' of present days. Each day is counted once regardless of partner number.
#' Species with no present days receive NA. This is a species-level coflowering
#' proportion, not a mean of pairwise edge durations.
#' @examples
#' species_data <- data.frame(
#'   species = c("A", "B", "C"),
#'   start_day = c(1, 4, 7),
#'   end_day = c(5, 8, 10)
#' )
#' day_matrix <- get_daywise_overlap(species_data)
#' calculate_prop_edge_persistence(day_matrix)
#' # Do not substitute a survey matrix containing NA for this binary input.
#' @export
calculate_prop_edge_persistence <- function(day_matrix) {
  if (!is.matrix(day_matrix)) {
    stop("day_matrix must be a matrix.")
  }

  species <- colnames(day_matrix)
  if (is.null(species)) {
    stop("day_matrix must have species names as column names.")
  }

  result <- data.frame(
    species = species,
    prop_with_neighbors = NA_real_,
    stringsAsFactors = FALSE
  )

  for (sp in species) {
    flowering_days <- which(day_matrix[, sp] == 1)

    if (length(flowering_days) == 0) {
      result[result$species == sp, "prop_with_neighbors"] <- NA_real_
      next
    }

    coflower_days <- vapply(
      flowering_days,
      function(day_idx) sum(day_matrix[day_idx, ]) > 1,
      logical(1)
    )

    prop <- mean(coflower_days)
    result[result$species == sp, "prop_with_neighbors"] <- prop
  }

  result
}

#' Compute average node and edge persistence from the network
#'
#' @param g An igraph object created from `create_temporal_network`
#' @param species_data The original data.frame with species, start_day, and end_day,
#'   or NULL to calculate edge persistence only (node persistence is then NA).
#' @return A data.frame with two metrics
#' @details
#' With species_data supplied, node persistence is the inclusive interval duration
#' averaged over its rows whose species occur in the graph. All species in an
#' interval graph created by create_temporal_network(), including isolates, now
#' contribute. Dates are converted through the shared preparation helper.
#'
#' With species_data = NULL, node persistence is NA and only edge persistence is
#' calculated. This graph-only mode is used by the survey workflow so it does not
#' infer a continuous species interval from incomplete or repeated-episode records.
#' It does not estimate node duration from graph edges.
#'
#' Edge persistence retains the original calculation: convert to undirected with
#' igraph::as_undirected(), sum weights of collapsed edges, then average edge
#' weights. In graphs produced by the shared constructor there is one edge per
#' overlapping pair, with weight equal to total shared presence days. A graph with
#' no vertices returns NA for both metrics. A nonempty edgeless graph retains its
#' node-duration summary when species_data is supplied, with edge persistence NA.
#' @examples
#' species_data <- data.frame(
#'   species = c("A", "B", "C"),
#'   start_day = c(1, 4, 7),
#'   end_day = c(5, 8, 10)
#' )
#' g <- create_temporal_network(species_data)
#' # Interval input supplies both node and edge persistence.
#' calculate_persistence_metrics(g, species_data)
#'
#' # Graph-only mode supplies edge persistence; node persistence is NA.
#' calculate_persistence_metrics(g)
#'
#' # The same function handles a survey-derived graph.
#' presence <- cbind(A = c(1, NA, 1), B = c(0, 1, 1))
#' g_survey <- create_temporal_network(presence_matrix = presence)
#' calculate_persistence_metrics(g_survey)
#'
#' # With no overlaps, species durations are still available.
#' isolated <- data.frame(
#'   species = c("X", "Y"), start_day = c(1, 10), end_day = c(3, 12)
#' )
#' calculate_persistence_metrics(create_temporal_network(isolated), isolated)
#' @export
calculate_persistence_metrics <- function(g, species_data = NULL) {

  # Handle empty networks
  if (igraph::vcount(g) == 0) {
    return(data.frame(
      avg_node_persistence = NA,
      avg_edge_persistence = NA
    ))
  }

  avg_node_persistence <- NA_real_
  if (!is.null(species_data)) {
    species_data <- prepare_species_data(species_data)

    # Ensure species names match between graph and data
    nodes <- igraph::V(g)$name
    node_durations <- species_data |>
      dplyr::filter(species %in% nodes) |>
      dplyr::mutate(duration = end_day - start_day + 1)

    avg_node_persistence <- mean(node_durations$duration)
  }

  # Convert to undirected to reflect mutual overlap
  g_undirected <- igraph::as_undirected(g, mode = "collapse", edge.attr.comb = "sum")

  avg_edge_persistence <- if (igraph::ecount(g_undirected) == 0) NA else mean(igraph::E(g_undirected)$weight)

  return(data.frame(
    avg_node_persistence = avg_node_persistence,
    avg_edge_persistence = avg_edge_persistence
  ))
}

#' Compute average temporal degree undirected (mean number of co-flowering partners, symmetric overlaps)
#'
#' @param g An igraph object from create_temporal_network()
#' @return Numeric: average degree across all nodes
#' @section Interpretation:
#' \describe{
#'   \item{0}{No coflowering — fully isolated species}
#'   \item{1–2}{Species overlap with 1–2 others on average}
#'   \item{Higher values}{More coflowering partners on average; depends on richness and duration}
#' }
#' @details
#' Wraps igraph::as_undirected() and igraph::degree(), then averages degree
#' across graph vertices. Direction is collapsed and edge weights do not
#' contribute to degree. For a simple overlap graph, degree counts distinct
#' coflowering partners. The average includes only vertices present in the
#' supplied graph; an empty graph returns NA. Values depend on community
#' richness and flowering duration as well as relative timing.
#' @examples
#' species_data <- data.frame(
#'   species = c("A", "B", "C"),
#'   start_day = c(1, 4, 7),
#'   end_day = c(5, 8, 10)
#' )
#' g <- create_temporal_network(species_data)
#' calculate_avg_temporal_degree(g)
#' # Including an isolated species changes the mean degree.
#' with_isolate <- rbind(species_data,
#'   data.frame(species = "D", start_day = 20, end_day = 22))
#' calculate_avg_temporal_degree(create_temporal_network(with_isolate))
#' @export
calculate_avg_temporal_degree <- function(g) {
  if (igraph::vcount(g) == 0) return(NA)

  # Treat the graph as undirected since coflowering is symmetric
  g_undirected <- igraph::as_undirected(g, mode = "collapse", edge.attr.comb = "sum")

  # Compute degree for all nodes
  deg <- igraph::degree(g_undirected)

  return(mean(deg))
}

#' Calculate average total degree (in + out) in a directed network
#'
#' @param g A directed igraph object
#' @return Numeric: average number of edges (in + out) per node
#' @details
#' Wraps igraph::degree() separately for incoming and outgoing edges and
#' averages their sum across graph vertices. Weights are ignored. Each
#' directed edge contributes to both its source's out-degree and its target's
#' in-degree, so the mean total degree is twice the number of edges divided
#' by the number of vertices. A graph with no vertices returns NA.
#' @examples
#' species_data <- data.frame(
#'   species = c("A", "B", "C"),
#'   start_day = c(1, 4, 7),
#'   end_day = c(5, 8, 10)
#' )
#' g <- create_temporal_network(species_data)
#' calculate_avg_temporal_network_degree(g)
#' # The survey community summary reuses this function for onset-directed graphs.
#' surveys <- data.frame(
#'   species = c("A", "A", "B", "B"),
#'   date = c("2026-04-01", "2026-04-05", "2026-04-03", "2026-04-07"),
#'   presence = 1
#' )
#' analysis <- prepare_phenology(surveys, method = "interval")
#' summarise_phenology_networks(analysis)$mean_degree
#' @export
calculate_avg_temporal_network_degree <- function(g) {
  if (igraph::vcount(g) == 0) return(NA)

  in_deg <- igraph::degree(g, mode = "in")
  out_deg <- igraph::degree(g, mode = "out")

  total_deg <- in_deg + out_deg

  mean(total_deg)
}

#' Calculate temporal degree of each node (species)
#'
#' Computes the number of overlapping flowering partners per species in a phenological network.
#' In an undirected network, this corresponds to the total number of species a node co-flowers with.
#' In a directed network, in-degree and out-degree can reflect upstream/downstream relationships based on flowering order.
#'
#' @param g An igraph object created with `create_temporal_network()`
#' @param mode Character. One of "total" (default), "in", or "out".
#'   - "total": treats network as undirected (collapses direction)
#'   - "in": counts incoming edges from earlier-onset species (subject to ties)
#'   - "out": counts outgoing edges to later-onset species (subject to ties)
#'
#' @return A data.frame with:
#'   - species: node name
#'   - degree: number of connected overlapping edges
#'
#' @section Interpretation:
#' \describe{
#'   \item{0}{Species does not coflower with any other species}
#'   \item{1–2}{Species overlaps with a few others during its phenophase}
#'   \item{Higher values}{More coflowering partners; interpretation depends on richness and duration}
#'   \item{"in" vs "out"}{May indicate if a species tends to flower earlier or later relative to its neighbors}
#' }
#'
#' @examples
#' species_data <- data.frame(
#'   species = c("A", "B", "C"),
#'   start_day = c(1, 4, 7),
#'   end_day = c(5, 8, 10)
#' )
#' g <- create_temporal_network(species_data)
#' calculate_node_temporal_degree(g, mode = "total")
#' calculate_node_temporal_degree(g, mode = "in")
#' calculate_node_temporal_degree(g, mode = "out")
#' @details
#' Wraps igraph::degree(). For mode = "total", first collapses direction
#' using igraph::as_undirected(); other modes are passed directly to igraph.
#' Weights do not contribute. With earlier-onset-to-later-onset edges,
#' incoming edges originate from earlier-starting neighbours and outgoing
#' edges point to later-starting neighbours; equal-onset edges follow the
#' construction convention. Only species present in the graph are returned.
#' An empty graph returns an empty data frame.
#' @export
calculate_node_temporal_degree <- function(g, mode = "total") {
  if (igraph::vcount(g) == 0) {
    return(data.frame(species = character(), degree = numeric()))
  }

  if (mode == "total") {
    g2 <- igraph::as_undirected(g, mode = "collapse", edge.attr.comb = "sum")
    deg <- igraph::degree(g2)
  } else {
    deg <- igraph::degree(g, mode = mode)
  }

  data.frame(species = names(deg), degree = deg)
}


#' Calculate average betweenness in the aggregated overlap network
#'
#' Computes the mean betweenness centrality across all nodes in a co-flowering network.
#' Betweenness centrality measures how often a node lies on the shortest paths between other nodes,
#' indicating its role as a temporal bridge or connector between species that otherwise do not co-flower directly.
#'
#' The function assumes that species with longer overlap (higher edge weight) are more strongly connected.
#' To prioritize such overlaps, the function inverts weights (i.e., 1/weight) for shortest path calculations.
#'
#' @param g An igraph object created using `create_temporal_network()`
#'
#' @return A numeric value: average betweenness centrality across all nodes in the (undirected) network
#'
#' @section Interpretation:
#' \describe{
#'   \item{0}{No node acts as a bridge — all paths are direct or disconnected}
#'   \item{Higher values}{More shortest-path mediation on average; values depend on graph size, structure, and weights}
#' }
#'
#' @examples
#' species_data <- data.frame(
#'   species = c("A", "B", "C"),
#'   start_day = c(1, 4, 7),
#'   end_day = c(5, 8, 10)
#' )
#' g <- create_temporal_network(species_data)
#' calculate_avg_betweenness(g) # averages raw undirected betweenness
#' @details
#' Collapses direction using igraph::as.undirected(), summing weights when
#' edges are merged, and calls igraph::betweenness() with inverse weights and
#' normalized = FALSE. Returns the arithmetic mean of raw node betweenness.
#' Positive finite overlap weights are expected. This is a static centrality
#' of the aggregated overlap graph, not an explicit time-dependent path
#' calculation. Scores depend on graph size and structure; a graph with no
#' vertices returns NA.
#' @export
calculate_avg_betweenness <- function(g) {
  if (igraph::vcount(g) == 0) return(NA)

  g_undirected <- igraph::as.undirected(g, mode = "collapse", edge.attr.comb = "sum")

  # Invert weights: high overlap = shorter path
  btwn <- igraph::betweenness(g_undirected,
                              weights = 1 / igraph::E(g_undirected)$weight,
                              normalized = FALSE)

  mean(btwn)
}

#' Calculate average closeness in the aggregated overlap network
#'
#' Computes the mean closeness centrality across all nodes in a co-flowering network.
#' Closeness centrality reflects how easily a species can reach (or be reached by) others
#' through overlapping flowering periods.
#'
#' The function inverts edge weights (1/weight), treating stronger overlaps as shorter paths.
#' The graph is treated as undirected to reflect mutual co-flowering.
#'
#' @param g An igraph object from `create_temporal_network()`
#'
#' @return A numeric value: average closeness centrality across all nodes
#'
#' @section Interpretation:
#' \describe{
#'   \item{NaN}{An isolated vertex has undefined closeness and propagates to the mean}
#'   \item{Low}{Species is far from many others — few or weak overlaps}
#'   \item{High}{Species is close to many others — central in phenological timing}
#' }
#'
#' @examples
#' species_data <- data.frame(
#'   species = c("A", "B", "C"),
#'   start_day = c(1, 4, 7),
#'   end_day = c(5, 8, 10)
#' )
#' g <- create_temporal_network(species_data)
#' calculate_avg_closeness(g)
#'
#' # An isolated species makes the current arithmetic mean NaN.
#' with_isolate <- rbind(species_data,
#'   data.frame(species = "D", start_day = 20, end_day = 22))
#' calculate_avg_closeness(create_temporal_network(with_isolate))
#' @details
#' Collapses direction using igraph::as.undirected(), summing weights when
#' edges are merged, then calls igraph::closeness() with inverse weights and
#' normalized = FALSE. Node scores are inverse sums of distances to reachable
#' other vertices. Unreachable vertices are omitted by igraph; a vertex with
#' no reachable neighbours has NaN closeness. The arithmetic mean does not
#' remove missing or NaN values. A graph with no vertices returns NA.
#' This is centrality on the aggregated graph, not elapsed-time accessibility.
#' @export
calculate_avg_closeness <- function(g) {
  if (igraph::vcount(g) == 0) return(NA)

  g_undirected <- igraph::as.undirected(g, mode = "collapse", edge.attr.comb = "sum")

  closeness_vals <- igraph::closeness(g_undirected,
                                      weights = 1 / igraph::E(g_undirected)$weight,
                                      normalized = FALSE)

  mean(closeness_vals)
}


#' Calculate upstream and downstream reachability sets for each node
#'
#' For each species (node), computes the number of other species it can reach
#' (downstream set) and the number of species that can reach it (upstream set)
#' via directed paths in the aggregated onset-ordered co-flowering network.
#'
#' @param g A directed igraph object from `create_temporal_network()`.
#'
#' @return A data.frame with species name, upstream size, and downstream size
#'
#' @examples
#' species_data <- data.frame(
#'   species = c("A", "B", "C"),
#'   start_day = c(1, 4, 7),
#'   end_day = c(5, 8, 10)
#' )
#' g <- create_temporal_network(species_data)
#' calculate_temporal_reachability(g)
#' # A reaches B and C; C has no downstream species.
#' @details
#' Wraps igraph::subcomponent() with outgoing and incoming modes and excludes
#' the focal vertex from each count. Weights are ignored. Downstream counts
#' vertices reachable along outgoing edges; upstream counts vertices with a
#' path to the focal species. Interpretation follows the supplied graph's
#' edge-orientation convention. The calculation uses aggregate graph paths,
#' without departure times or traversal durations. An empty graph returns
#' an empty data frame.
#' @export
calculate_temporal_reachability <- function(g) {
  if (igraph::vcount(g) == 0) {
    return(data.frame(species = character(), upstream = integer(), downstream = integer()))
  }

  species <- igraph::V(g)$name

  downstream_sizes <- sapply(species, function(sp) {
    length(igraph::subcomponent(g, sp, mode = "out")) - 1  # exclude self
  })

  upstream_sizes <- sapply(species, function(sp) {
    length(igraph::subcomponent(g, sp, mode = "in")) - 1  # exclude self
  })

  data.frame(
    species = species,
    upstream = upstream_sizes,
    downstream = downstream_sizes
  )
}
