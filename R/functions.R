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
  arrange(start_day) %>%  # Ensure species are ordered by start day
  mutate(
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
