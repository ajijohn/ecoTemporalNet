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
      if (species_i$end_day >= species_j$start_day && species_i$start_day <= species_j$end_day) {
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
  g <- graph_from_edgelist(edges, directed = TRUE)
  E(g)$weight <- weights  # Add the weights (overlap duration) to the edges

  return(g)
}

#' Plot a snapshot of the temporal network
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


#' Function to calculate Distance (shortest paths with weights representing overlap duration)
#' @export
calculate_distance_weights <- function(g) {
  distance_matrix <- shortest.paths(g, weights = E(g)$weight)  # Shortest paths with overlap duration as weights
  return(distance_matrix)
}

#' Function to calculate Distance (smallest number of nodes, hops, between species)
#' @export
calculate_distance <- function(g) {
  distance_matrix <- shortest.paths(g, v = V(g), mode = "all", weights = NULL)  # Shortest paths (unweighted)
  return(distance_matrix)
}

#' Function to calculate Latency (time duration, shortest time between species in time-ordered network)
#' @export
calculate_latency <- function(g) {
  # Calculate the shortest path distances with weights (overlap duration)
  latency_matrix <- shortest.paths(g, weights = E(g)$weight)
  return(latency_matrix)
}

#' Function to compute shortest paths between all species pairs
#' @export
calculate_shortest_paths <- function(g) {
  # Calculate all shortest paths between every pair of nodes
  shortest_paths_matrix <- shortest.paths(g, weights = NULL)
  return(shortest_paths_matrix)
}

#' Create a function to calculate edge persistence (pairwise overlap duration)
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

#' Function to calculate time-dependent betweenness centrality
#' @export
calculate_betweenness <- function(g, shortest_paths_matrix) {
  N <- vcount(g)  # Number of nodes
  betweenness_values <- numeric(N)  # Initialize betweenness values

  for (i in 1:N) {
    for (j in 1:N) {
      if (i != j) {
        # Get the shortest path lengths between nodes i and j
        shortest_path_length <- shortest_paths_matrix[i, j]

        if (is.finite(shortest_path_length)) {
          # For each path, calculate the contribution to betweenness centrality
          betweenness_values[i] <- betweenness_values[i] + 1 / shortest_path_length
        }
      }
    }
  }

  # Normalize by the number of nodes minus 1
  betweenness_values <- betweenness_values / (N - 1)

  return(betweenness_values)
}


#' Calculate inter-event times (duration between end of one species and start of the next)
#' @export
inter_event_times <- function(species_data) {
  species_data_iet <- species_data %>%
  dplyr::arrange(start_day) %>%  # Ensure species are ordered by start day
  dplyr::mutate(
    inter_event_time = abs(lead(start_day) - end_day)  # Calculate the time difference (assumin days)
  )
return(species_data_iet)
}

#' The formula uses latency to adjust the memory metric, using correlation between inter-event times
#' @export
calculate_memory_with_latency <- function(species_data) {

  #if no overlaps,short-ciruit
  if (is.null(check_temporal_overlap(species_data)$edges))
  {
    return(NA_real_)
  }

  species_data_iet<-  inter_event_times(species_data)
  n <- nrow(species_data_iet)

  # Build the temporal network
  tn <- create_temporal_network(species_data)
  latency_matrix <- calculate_latency(tn)

  # Calculate inter-event times
  inter_event_times <- species_data_iet$inter_event_time

  # Memory calculation using latency-adjusted formula
  m1 <- mean(inter_event_times,na.rm = TRUE)
  m2 <- mean(inter_event_times[2:n],na.rm = TRUE)

  sigma1 <- sd(inter_event_times,na.rm = TRUE)
  sigma2 <- sd(inter_event_times[2:n],na.rm = TRUE)

  # Apply the memory formula with latency consideration (adjust the memory formula by incorporating latency)
  memory_value <- mean((inter_event_times - m1) *
                         (lead(inter_event_times) - m2) *
                         latency_matrix[1:(n - 1), 2:n], na.rm = TRUE) / (sigma1 * sigma2)

  return(memory_value)
}

#' Calculate burstiness (B)
#' @export
calculate_burstiness <- function(species_data) {

  species_data_iet<-  inter_event_times(species_data)

  # Calculate mean and standard deviation of inter-event times
  m_tau <- mean(species_data_iet$inter_event_time,na.rm = TRUE)
  sigma_tau <- sd(species_data_iet$inter_event_time,na.rm = TRUE)

  # Calculate Burstiness (B) using the formula
  burstiness <- (sigma_tau / m_tau - 1) / (sigma_tau / m_tau + 1)

  return(burstiness)
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


#’ Memory metric (lag‐1 correlation of inter‐event times, scaled)
#’
#’ m = r₁ / (n−2), where r₁ = corr(ied₁…n−2, ied₂…n−1)
#’ @param times Numeric vector of event times
#’ @return Numeric scalar in [–1,1] or NA if too few events
#' @export
memory <- function(times) {
  ied <- interevent_durations(times)
  if (length(ied) < 2) return(NA_real_)
  r1 <- stats::cor(ied[-length(ied)], ied[-1])
  r1 / (length(ied) - 1)
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

#’ For each species, calculate % of its flowering days spent coflowering
#’
#’ @param Day matrix
#’ @return dataframe of species and prop. of persistence
#' @export
calculate_prop_edge_persistence <- function(day_matrix) {
  species <- colnames(day_matrix)
  result <- data.frame(species = species, prop_with_neighbors = NA)

  for (sp in species) {
    flowering_days <- which(day_matrix[, sp] == 1)
    coflower_days <- sapply(flowering_days, function(day_idx) {
      coflowering <- sum(day_matrix[day_idx, ]) > 1  # at least one neighbor
      return(coflowering)
    })
    prop <- sum(coflower_days) / length(flowering_days)
    result[result$species == sp, "prop_edge_persistency"] <- prop
  }

  return(result)
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

#' Compute average temporal degree (mean number of co-flowering partners)
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
