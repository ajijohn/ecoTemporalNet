library(testthat)
library(ecoTemporalNet)


test_that("calculate_latency returns directed hop distances by default", {
  species_data <- data.frame(
    species = c("A", "B", "C"),
    start_day = c(1, 3, 6),
    end_day = c(5, 7, 9),
    stringsAsFactors = FALSE
  )

  g <- create_temporal_network(species_data)
  lat <- calculate_latency(g)

  expected <- matrix(
    c(0, 1, 2,
      Inf, 0, 1,
      Inf, Inf, 0),
    nrow = 3,
    byrow = TRUE,
    dimnames = list(c("A", "B", "C"), c("A", "B", "C"))
  )

  expect_equal(lat, expected)
})


test_that("calculate_latency can use inverse overlap duration as directed cost", {
  species_data <- data.frame(
    species = c("A", "B", "C"),
    start_day = c(1, 3, 6),
    end_day = c(5, 7, 9),
    stringsAsFactors = FALSE
  )

  g <- create_temporal_network(species_data)
  lat <- calculate_latency(g, use_overlap_strength = TRUE)

  expected <- matrix(
    c(0, 1 / 3, 1 / 3 + 1 / 2,
      Inf, 0, 1 / 2,
      Inf, Inf, 0),
    nrow = 3,
    byrow = TRUE,
    dimnames = list(c("A", "B", "C"), c("A", "B", "C"))
  )

  expect_equal(lat, expected)
})
