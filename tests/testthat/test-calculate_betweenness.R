library(testthat)
library(ecoTemporalNet)


test_that("calculate_betweenness returns directed betweenness", {
  species_data <- data.frame(
    species = c("A", "B", "C"),
    start_day = c(1, 3, 6),
    end_day = c(5, 7, 9),
    stringsAsFactors = FALSE
  )

  g <- create_temporal_network(species_data)
  bt <- calculate_betweenness(g, use_overlap_strength = FALSE, normalized = FALSE)

  expect_equal(unname(bt[c("A", "B", "C")]), c(0, 1, 0))
})
