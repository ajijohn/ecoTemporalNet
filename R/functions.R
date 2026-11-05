
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

    parse_date <- function(x) {
      if (inherits(x, "Date")) {
        return(x)
      }

      if (!is.character(x)) {
        stop("Date columns must contain Date values or YYYY-MM-DD strings.")
      }

      parsed <- tryCatch(
        as.Date(x, format = "%Y-%m-%d"),
        error = function(e) {
          stop("Dates must be non-missing and valid YYYY-MM-DD dates.")
        }
      )

      valid <- !is.na(x) & !is.na(parsed)
      valid[valid] <- format(parsed[valid], "%Y-%m-%d") == x[valid]

      if (!all(valid)) {
        stop("Dates must be non-missing and valid YYYY-MM-DD dates.")
      }

      parsed
    }

    start_date <- parse_date(species_data$start_date)
    end_date <- parse_date(species_data$end_date)

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

  species_data
}

#' Create the time-ordered network by checking temporal overlap
#' @details
#' Accepts integer start_day and end_day columns, or start_date and end_date
#' columns containing Date values or YYYY-MM-DD strings. Dates must belong to
#' one calendar year and take precedence over day columns. Each interval is
#' assumed to represent continuous presence of the selected phenological stage.
#' Both endpoints are included when counting overlap days.
#'
#' Input must be ordered by nondecreasing start_day. Each pair is examined
#' once in input order; an edge is retained only when the first species starts
#' no later than the second. Equal-onset edges follow input row order.
#' Returns an edge matrix and corresponding overlap durations; when no pairs
#' are retained, edges is NULL.
#' @examples
#' species_data <- data.frame(
#'   species = c("A", "B", "C"),
#'   start_day = c(1, 4, 10),
#'   end_day = c(5, 8, 15)
#' )
#' check_temporal_overlap(species_data)
#'
#' # Calendar dates within a single year are also accepted.
#' date_data <- data.frame(
#'   species = c("A", "B"),
#'   start_date = c("2026-04-01", "2026-04-04"),
#'   end_date = c("2026-04-05", "2026-04-08")
#' )
#' check_temporal_overlap(date_data)
#' @export
check_temporal_overlap <- function(species_data) {
  edges <- list()
  weights <- c()  # Store the edge weights (duration of overlap)

  #https://github.com/ajijohn/ecoTemporalNet/issues/5
  # Support for survey dates in calendar format

  species_data <- prepare_species_data(species_data)


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
#' @param species A data.frame of nodes
#' @return A networkDynamic object
#' @details
#' Uses check_temporal_overlap() to obtain directed edges and constructs an
#' igraph graph with igraph::graph_from_edgelist(). Edge weights are inclusive
#' pairwise overlap durations in days. Input conventions and ordering
#' requirements are those of check_temporal_overlap().
#'
#' The implementation returns an igraph object. Only species occurring in the
#' edge list become vertices; species without retained overlaps are omitted.
#' When there are no edges, an empty directed graph with no vertices is returned.
#' @examples
#' species_data <- data.frame(
#'   species = c("A", "B", "C"),
#'   start_day = c(1, 4, 10),
#'   end_day = c(5, 8, 15)
#' )
#' g <- create_temporal_network(species_data)
#' g
#' @export
create_temporal_network <- function(species_data) {

  species_data <- prepare_species_data(species_data)
  # Get co-occurrence edges and create the graph (time-ordered network)
  overlap_result <- check_temporal_overlap(species_data)
  edges <- overlap_result$edges
  weights <- overlap_result$weights

  # if there are no edges, return an empty directed graph
  if (is.null(edges) || nrow(edges) == 0) {
    # create an empty graph with no vertices or edges
    return(igraph::make_empty_graph(directed = TRUE))
  }

  # Create the graph (time-ordered network) with weights as edge attributes
  g <- igraph::graph_from_edgelist(edges, directed = TRUE)
  igraph::E(g)$weight <- weights  # Add the weights (overlap duration) to the edges

  return(g)
}

#' Plot a snapshot of the temporal network
#' @param g temporal graph
#' @details
#' Uses igraph graph conversion and plotting with a Fruchterman-Reingold
#' layout. Edge labels display overlap durations, and widths are scaled to a
#' maximum of five relative to the largest edge weight in the supplied graph.
#' The display represents an aggregated overlap network rather than a sequence
#' of daily snapshots. Nonempty graphs with positive edge weights are expected.
#' @examples
#' library(igraph)
#' species_data <- data.frame(
#'   species = c("A", "B", "C"),
#'   start_day = c(1, 4, 10),
#'   end_day = c(5, 8, 15)
#' )
#' g <- create_temporal_network(species_data)
#' plot_temporal_network(g)
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
#'   start_day = c(1, 4, 10),
#'   end_day = c(5, 8, 15)
#' )
#' g <- create_temporal_network(species_data)
#' calculate_weighted_overlap_distance(g)
#' @export
calculate_weighted_overlap_distance <- function(g) {
  distance_matrix <- shortest.paths(g, weights = E(g)$weight)  # Shortest paths with overlap duration as weights
  return(distance_matrix)
}

#' Function to calculate distance (smallest number of nodes, hops, between species)
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
#'   start_day = c(1, 4, 10),
#'   end_day = c(5, 8, 15)
#' )
#' g <- create_temporal_network(species_data)
#' calculate_overlap_hop_distance(g)
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
#'   start_day = c(1, 4, 10),
#'   end_day = c(5, 8, 15)
#' )
#' g <- create_temporal_network(species_data)
#' calculate_latency(g)
#' calculate_latency(g, use_overlap_strength = TRUE)
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
#'   start_day = c(1, 4, 10),
#'   end_day = c(5, 8, 15)
#' )
#' g <- create_temporal_network(species_data)
#' calculate_overlap_shortest_paths(g)
#' @export
calculate_overlap_shortest_paths <- function(g) {
  # Calculate all shortest paths between every pair of nodes
  shortest_paths_matrix <- shortest.paths(g, weights = NULL)
  return(shortest_paths_matrix)
}

#' Create a function to calculate edge persistence (pairwise overlap duration)
#' @param species Species dataframe
#' @details
#' Examines each unordered pair of species and counts days in the intersection
#' of their continuous phenological intervals, including both endpoints.
#' Accepts day or single-calendar-year date columns through the internal input
#' helper. Pair labels follow input order and do not establish ecological
#' direction. Only overlapping pairs are returned; with no overlaps, edges is
#' NULL and persistence is empty.
#' @examples
#' species_data <- data.frame(
#'   species = c("A", "B", "C"),
#'   start_day = c(1, 4, 10),
#'   end_day = c(5, 8, 15)
#' )
#' calculate_edge_persistence(species_data)
#' @export
calculate_edge_persistence <- function(species_data) {
  edges <- list()
  persistence_values <- c()  # Store edge persistence (duration of overlap)

  species_data <- prepare_species_data(species_data)

  for (i in 1:(nrow(species_data) - 1)) {
    for (j in (i + 1):nrow(species_data)) {
      species_i <- species_data[i, ]
      species_j <- species_data[j, ]

      # Check if there is temporal overlap between species i and j
      if (species_i$end_day >= species_j$start_day && species_i$start_day <= species_j$end_day) {
        # Calculate the overlap period
        overlap_start <- max(species_i$start_day, species_j$start_day)
        overlap_end <- min(species_i$end_day, species_j$end_day)
        overlap_duration <- overlap_end - overlap_start + 1  # Duration of overlap in days
        edges <- append(edges, list(c(species_i$species, species_j$species)))  # Store edge
        persistence_values <- c(persistence_values, overlap_duration)  # Store overlap duration as persistence
      }
    }
  }

  # Return edges and their corresponding persistence values
  return(list(edges = do.call(rbind, edges), persistence = persistence_values))
}

#' Calculate node betweenness centrality in the directed overlap network
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
#'   start_day = c(1, 4, 10),
#'   end_day = c(5, 8, 15)
#' )
#' g <- create_temporal_network(species_data)
#' calculate_betweenness(g)
#' calculate_betweenness(g, normalized = FALSE)
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
#' Zero means intervals touch.
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
#' inter_event_times(species_data)
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
#'   start_day = c(1, 4, 10),
#'   end_day = c(5, 8, 15)
#' )
#' # NA is returned when usable gap pairs have insufficient variance.
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
#'   species = c("A", "B", "C"),
#'   start_day = c(1, 4, 10),
#'   end_day = c(5, 8, 15)
#' )
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

#’ Compute inter‐event durations from sorted event times
#’ @param times Numeric vector of event times (will be sorted)
#’ @return Numeric vector of inter‐event durations
#' @details
#' Coerces times to numeric, sorts them, and returns successive differences
#' using diff(). Tied times produce zero durations. Units follow the numeric
#' representation of the input; there is no inclusive-day adjustment. This
#' function operates on event times rather than flowering intervals.
#' @examples
#' times <- c(12, 1, 7, 3, 20)
#' interevent_durations(times)
#' @export
interevent_durations <- function(times) {
  t <- sort(as.numeric(times))
  diff(t)
}


#’ Simple burstiness (Goh & Barabási, 2008)
#’
#’ B = (σ – μ) / (σ + μ)
#’ @param ied Numeric vector of inter‐event durations
#’ @return Numeric scalar in [–1,1]
#' @details
#' Computes (sd - mean) / (sd + mean) from the supplied durations using
#' stats::sd(). Durations are expected to be finite and nonnegative; no sorting
#' or missing-value removal is performed. When mean and standard deviation
#' are both zero, the implementation returns 1 by convention. Insufficient
#' observations or missing values are not explicitly guarded against.
#' @examples
#' ied <- c(2, 4, 5, 8, 3)
#' burstiness_simple(ied)
#' @export
burstiness_simple <- function(ied) {
  m <- mean(ied)
  s <- stats::sd(ied)
  # degenerate: all intervals zero
  if (m + s == 0) return(1)
  (s - m) / (s + m)
}



#’ Finite‐size burstiness, unbounded case (Eq. 22, Kim & Jo 2016)
#’
#’ Aₙ(r) = [√(n+1)·r − √(n−1)] / [r·(√(n+1)−2) + √(n−1)]
#’ where r = CV = σ/μ
#’ @param ied Numeric vector of inter‐event durations
#’ @return Numeric scalar in [–1,1]
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


#’ Build co-occurrence table by day
#’
#’ @param species A data.frame of nodes
#’ @return Day matrix
#' @details
#' Accepts integer day intervals or date intervals within one calendar year.
#' Each interval is expanded into daily presence, including both endpoints.
#' Rows contain the union of represented days and columns correspond to input
#' species; days with no species present are omitted. Row order follows first
#' occurrence in the expanded intervals and is not explicitly sorted. Presence
#' is assumed continuous between each species' start and end.
#' @examples
#' species_data <- data.frame(
#'   species = c("A", "B", "C"),
#'   start_day = c(1, 4, 10),
#'   end_day = c(5, 8, 15)
#' )
#' get_daywise_overlap(species_data)
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
#'   start_day = c(1, 4, 10),
#'   end_day = c(5, 8, 15)
#' )
#' day_matrix <- get_daywise_overlap(species_data)
#' calculate_prop_edge_persistence(day_matrix)
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
#' @param species_data The original data.frame with species, start_day, and end_day
#' @return A data.frame with two metrics
#' @details
#' Node persistence is the inclusive interval duration averaged over input
#' rows whose species occur in the supplied graph. Edge persistence is the
#' mean weight after igraph::as_undirected() collapses directed edges by
#' summing their attributes. Species absent from the graph do not contribute
#' to the node mean. Date inputs are converted to day of year by the internal
#' helper. A graph with no vertices returns NA for both summaries; with
#' vertices but no edges, the edge summary is NA.
#' @examples
#' species_data <- data.frame(
#'   species = c("A", "B", "C"),
#'   start_day = c(1, 4, 10),
#'   end_day = c(5, 8, 15)
#' )
#' g <- create_temporal_network(species_data)
#' calculate_persistence_metrics(g, species_data)
#' @export
calculate_persistence_metrics <- function(g, species_data) {

  # Handle empty networks
  if (igraph::vcount(g) == 0) {
    return(data.frame(
      avg_node_persistence = NA,
      avg_edge_persistence = NA
    ))
  }

  species_data <- prepare_species_data(species_data)

  # Ensure species names match between graph and data
  nodes <- igraph::V(g)$name
  node_durations <- species_data |>
    dplyr::filter(species %in% nodes) |>
    dplyr::mutate(duration = end_day - start_day + 1)

  avg_node_persistence <- mean(node_durations$duration)

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
#'   \item{High}{Dense synchrony — many co-flowering partners}
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
#'   start_day = c(1, 4, 10),
#'   end_day = c(5, 8, 15)
#' )
#' g <- create_temporal_network(species_data)
#' calculate_avg_temporal_degree(g)
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
#'   start_day = c(1, 4, 10),
#'   end_day = c(5, 8, 15)
#' )
#' g <- create_temporal_network(species_data)
#' calculate_avg_temporal_network_degree(g)
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
#'   - "in": counts incoming edges only (e.g., overlapping with species that flower later)
#'   - "out": counts outgoing edges only (e.g., overlapping with species that flower earlier)
#'
#' @return A data.frame with:
#'   - species: node name
#'   - degree: number of connected overlapping edges
#'
#' @section Interpretation:
#' \describe{
#'   \item{0}{Species does not coflower with any other species}
#'   \item{1–2}{Species overlaps with a few others during its phenophase}
#'   \item{High (>5)}{Species overlaps with many others — strong synchrony}
#'   \item{"in" vs "out"}{May indicate if a species tends to flower earlier or later relative to its neighbors}
#' }
#'
#' @examples
#' species_data <- data.frame(
#'   species = c("A", "B", "C"),
#'   start_day = c(1, 3, 6),
#'   end_day = c(5, 7, 9)
#' )
#' g <- create_temporal_network(species_data)
#' calculate_node_temporal_degree(g, mode = "total")
#'
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


#' Calculate average time-dependent betweenness centrality
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
#'   \item{Low (~0.1–0.5)}{Some species act as connectors between clusters, but many are peripheral}
#'   \item{High (>1)}{One or more species serve as important bridges — linking phenological modules or stages}
#' }
#'
#' @examples
#' species_data <- data.frame(
#'   species = c("A", "B", "C"),
#'   start_day = c(1, 3, 6),
#'   end_day = c(5, 7, 9)
#' )
#' g <- create_temporal_network(species_data)
#' calculate_avg_betweenness(g)
#'
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

#' Calculate average time-dependent closeness centrality
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
#'   \item{0}{Node is isolated — no access to other species}
#'   \item{Low}{Species is far from many others — few or weak overlaps}
#'   \item{High}{Species is close to many others — central in phenological timing}
#' }
#'
#' @examples
#' species_data <- data.frame(
#'   species = c("A", "B", "C"),
#'   start_day = c(1, 3, 6),
#'   end_day = c(5, 7, 9)
#' )
#' g <- create_temporal_network(species_data)
#' calculate_avg_closeness(g)
#'
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
#' via time-respecting paths in a directed co-flowering network.
#'
#' @param g A directed igraph object from `create_temporal_network()`, with time-respecting edge direction
#'
#' @return A data.frame with species name, upstream size, and downstream size
#'
#' @examples
#' species_data <- data.frame(
#'   species = c("A", "B", "C"),
#'   start_day = c(1, 3, 6),
#'   end_day = c(5, 7, 9)
#' )
#' g <- create_temporal_network(species_data)  # assuming edge direction is time-respecting
#' calculate_temporal_reachability(g)
#'
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
