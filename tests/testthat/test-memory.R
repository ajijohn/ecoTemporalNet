library(testthat)
library(ecoTemporalNet)


test_that("memory returns lag-1 correlation of interevent durations", {
  times <- c(1, 3, 6, 10)

  expect_equal(memory(times), 1)
})


test_that("memory returns NA when too few durations are available", {
  expect_true(is.na(memory(c(1, 2))))
})
