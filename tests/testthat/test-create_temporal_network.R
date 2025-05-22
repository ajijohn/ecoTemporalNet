library(testthat)
library(ecoTemporalNet)
library(igraph)

test_that("create_temporal_network returns correct igraph with weights", {
  species_data <- data.frame(
    species   = c("Species_A", "Species_B", "Species_C", "Species_D", "Species_E"),
    start_day = c(10, 40, 50, 60, 70),
    end_day   = c(50, 60, 70, 80, 100),
    stringsAsFactors = FALSE
  )

  g <- create_temporal_network(species_data)

  # it should be an igraph graph
  expect_s3_class(g, "igraph")

  # expected directed edges and weights (same as in the overlap test)
  expected_edges <- matrix(
    c(
      "Species_A","Species_B",
      "Species_A","Species_C",
      "Species_B","Species_C",
      "Species_B","Species_D",
      "Species_C","Species_D",
      "Species_C","Species_E",
      "Species_D","Species_E"
    ),
    ncol = 2,
    byrow = TRUE
  )
  expected_weights <- c(11, 1, 11, 1, 11, 1, 11)

  # compare edge list (character matrix)
  actual_edges <- igraph::as_edgelist(g, names = TRUE)
  expect_equal(actual_edges, expected_edges)

  # compare weights
  actual_weights <- igraph::E(g)$weight
  expect_equal(actual_weights, expected_weights)
})
