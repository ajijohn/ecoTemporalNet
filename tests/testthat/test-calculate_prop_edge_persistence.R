library(testthat)
library(ecoTemporalNet)


test_that("calculate_prop_edge_persistence returns coflowering proportions", {
  day_matrix <- matrix(
    c(1, 0, 0,
      1, 0, 0,
      1, 1, 0,
      0, 1, 0),
    nrow = 4,
    byrow = TRUE,
    dimnames = list(NULL, c("A", "B", "C"))
  )

  res <- calculate_prop_edge_persistence(day_matrix)

  expect_equal(res$species, c("A", "B", "C"))
  expect_equal(res$prop_with_neighbors[res$species == "A"], 1 / 3)
  expect_equal(res$prop_with_neighbors[res$species == "B"], 1 / 2)
  expect_true(is.na(res$prop_with_neighbors[res$species == "C"]))
})
