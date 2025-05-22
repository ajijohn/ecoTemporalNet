library(testthat)
library(ecoTemporalNet)

test_that("calculate_latency uses inclusive overlap durations for shortest paths", {
  # Species windows:
  #   A: days 1–5, B: days 3–7, C: days 6–9
  # Inclusive overlaps:
  #   A→B: days {3,4,5} = 3
  #   B→C: days {6,7}   = 2
  species_data <- data.frame(
    species   = c("A", "B", "C"),
    start_day = c(1,   3,   6),
    end_day   = c(5,   7,   9),
    stringsAsFactors = FALSE
  )

  g   <- create_temporal_network(species_data)
  lat <- calculate_latency(g)

  nodes <- c("A", "B", "C")
  expected <- matrix(Inf, nrow = 3, ncol = 3,
                     dimnames = list(nodes, nodes))
  diag(expected) <- 0
  expected["A", "B"] <- 3       # (5 - 3 + 1)
  expected["B", "C"] <- 2       # (7 - 6 + 1)
  expected["A", "C"] <- 3 + 2   # via A→B→C = 5
  expected["B", "A"] <- 3
  expected["C", "A"] <- 5
  expected["C", "B"] <- 2


  expect_equal(lat, expected)
})
