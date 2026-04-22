library(testthat)
library(dplyr)
library(ecoTemporalNet)


test_that("inter_event_times keeps signed gaps and overlap duration", {
  df <- data.frame(
    species = c("A", "B", "C"),
    start_day = c(1, 4, 10),
    end_day = c(5, 8, 15),
    stringsAsFactors = FALSE
  )

  res <- inter_event_times(df)

  expect_equal(res$species, c("A", "B", "C"))
  expect_equal(res$gap_to_next, c(-2, 1, NA))
  expect_equal(res$gap_positive, c(0, 1, NA))
  expect_equal(res$overlap_duration, c(2, 0, NA))
})
