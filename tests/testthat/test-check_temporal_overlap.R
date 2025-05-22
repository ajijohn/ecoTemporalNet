library(testthat)
library(ecoTemporalNet)
test_that("check_temporal_overlap returns correct edges and weights", {
  species_data <- data.frame(
    species   = c("Species_A", "Species_B", "Species_C", "Species_D", "Species_E"),
    start_day = c(10, 40, 50, 60, 70),
    end_day   = c(50, 60, 70, 80, 100),
    stringsAsFactors = FALSE
  )

  result <- check_temporal_overlap(species_data)

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

  expect_equal(result$edges,   expected_edges)
  expect_equal(result$weights, expected_weights)
})
