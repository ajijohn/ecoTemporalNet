library(testthat)
library(ecoTemporalNet)
library(igraph)


test_that("calculate_memory_with_latency returns NA when there are too few species", {
  df <- data.frame(
    species = c("X", "Y", "Z"),
    start_day = c(1, 4, 9),
    end_day = c(2, 6, 12),
    stringsAsFactors = FALSE
  )

  expect_true(is.na(calculate_memory_with_latency(df)))
})


test_that("calculate_memory_with_latency returns weighted lag-1 correlation", {
  df <- data.frame(
    species = c("A", "B", "C", "D"),
    start_day = c(1, 4, 8, 13),
    end_day = c(2, 5, 9, 14),
    stringsAsFactors = FALSE
  )

  fake_graph <- igraph::make_empty_graph(n = 4, directed = TRUE) |>
    igraph::set_vertex_attr("name", value = c("A", "B", "C", "D"))

  fake_latency <- matrix(
    c(0,   1,   1,   2,
      Inf, 0,   1,   3,
      Inf, Inf, 0,   1,
      Inf, Inf, Inf, 0),
    nrow = 4,
    byrow = TRUE,
    dimnames = list(c("A", "B", "C", "D"), c("A", "B", "C", "D"))
  )

  testthat::local_mocked_bindings(
    create_temporal_network = function(species_data) fake_graph,
    calculate_latency = function(g, use_overlap_strength = FALSE) fake_latency,
    .package = "ecoTemporalNet"
  )

  mem <- calculate_memory_with_latency(df)

  expect_equal(mem, 1)
})
