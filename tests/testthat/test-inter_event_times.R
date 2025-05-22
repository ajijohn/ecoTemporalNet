library(testthat)
library(dplyr)
library(ecoTemporalNet)

test_that("inter_event_times computes correct inter-event time", {
  df <- data.frame(
    species   = c("A","B","C"),
    start_day = c(1, 5, 10),
    end_day   = c(3, 8, 15),
    stringsAsFactors = FALSE
  )

  res <- inter_event_times(df)

  # After ordering by start_day, rows are A, B, C
  # inter_event_time for A = lead(start_B=5) - end_A=3 = 2
  # for B = lead(start_C=10) - end_B=8 = 2
  # for C = NA
  expect_equal(res$inter_event_time, c(2, 2, NA))
  expect_equal(res$species, df$species)
})
