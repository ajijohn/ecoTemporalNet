#' Create the time-ordered network by checking temporal overlap
#' @export
check_temporal_overlap <- function(species_data) {
  edges <- list()
  weights <- c()  # Store the edge weights (duration of overlap)

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
#' @export
create_temporal_network <- function(species_data) {
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
#' @export
calculate_weighted_overlap_distance <- function(g) {
  distance_matrix <- shortest.paths(g, weights = E(g)$weight)  # Shortest paths with overlap duration as weights
  return(distance_matrix)
}

#' Function to calculate distance (smallest number of nodes, hops, between species)
#'
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
#' @export
calculate_overlap_shortest_paths <- function(g) {
  # Calculate all shortest paths between every pair of nodes
  shortest_paths_matrix <- shortest.paths(g, weights = NULL)
  return(shortest_paths_matrix)
}

#' Create a function to calculate edge persistence (pairwise overlap duration)
#' @param species Species dataframe
#' @export
calculate_edge_persistence <- function(species_data) {
  edges <- list()
  persistence_values <- c()  # Store edge persistence (duration of overlap)

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
#' @export
inter_event_times <- function(species_data) {
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
      overlap_duration  = pmax(0L, end_day - next_start_day + 1L)
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


#' Calculate burstiness (B) from non-negative interval gaps
#'
#' Uses gap_positive from inter_event_times():
#' - overlap => 0 waiting time
#' - gap => positive waiting time
#'
#' @param species_data Data frame with species, start_day, end_day
#' @return Numeric scalar in [-1, 1], or NA if too few usable gaps
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
#' @export
get_daywise_overlap <- function(species_data) {
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
#' @export
calculate_persistence_metrics <- function(g, species_data) {
  # Handle empty networks
  if (igraph::vcount(g) == 0) {
    return(data.frame(
      avg_node_persistence = NA,
      avg_edge_persistence = NA
    ))
  }

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
