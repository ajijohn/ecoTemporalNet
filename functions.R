#' Create a temporal ecological network
#' @export
create_temporal_network <- function(nodes, edges) {
  library(networkDynamic)
  net <- network.initialize(n = nrow(nodes), directed = TRUE)
  network::set.vertex.attribute(net, "name", nodes$id)
  add.edge.activity(net,
                    tail = match(edges$from, nodes$id),
                    head = match(edges$to, nodes$id),
                    onset = edges$onset,
                    terminus = edges$terminus)
  return(net)
}

#' Plot a snapshot of the temporal network
#' @export
plot_temporal_network <- function(net, time) {
  library(igraph)
  net_static <- network.extract(net, at = time)
  g <- igraph::graph_from_adjacency_matrix(as.matrix(as.sociomatrix(net_static)))
  plot(g, vertex.label = V(g)$name, main = paste("Network at time", time))
}
