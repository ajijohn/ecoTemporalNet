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

test_that("nested intervals use the earlier end date", {
  df <- data.frame(
    species = c("A", "B", "C"),
    start_day = c(1, 4, 12),
    end_day = c(10, 5, 15)
  )

  res <- inter_event_times(df)

  expect_equal(res$species, c("A", "B", "C"))
  expect_equal(res$gap_to_next, c(-7, 6, NA))
  expect_equal(res$gap_positive, c(0, 6, NA))
  expect_equal(res$overlap_duration, c(2, 0, NA))
})

test_that("inclusive days distinguish shared endpoints from adjacent intervals", {
  df <- data.frame(
    species = c("A", "B", "C", "D"),
    start_day = c(1, 3, 6, 10),
    end_day = c(3, 5, 8, 12)
  )

  res <- inter_event_times(df)

  # A–B share day 3; B–C are adjacent; C–D have day 9 between them.
  expect_equal(res$species, c("A", "B", "C", "D"))
  expect_equal(res$gap_to_next, c(-1, 0, 1, NA))
  expect_equal(res$gap_positive, c(0, 0, 1, NA))
  expect_equal(res$overlap_duration, c(1, 0, 0, NA))
})

test_that("results do not depend on input row order", {
  df <- data.frame(
    species = c("A", "B", "C"),
    start_day = c(1, 4, 10),
    end_day = c(5, 8, 15)
  )

  expected <- inter_event_times(df)
  actual <- inter_event_times(df[c(3, 1, 2), ])

  rownames(expected) <- NULL
  rownames(actual) <- NULL

  expect_equal(actual, expected)
})

test_that("tied starts are ordered by end day and then species", {
  df <- data.frame(
    species = c("C", "B", "A"),
    start_day = c(4, 4, 4),
    end_day = c(8, 5, 5)
  )

  res <- inter_event_times(df)

  expect_equal(res$species, c("A", "B", "C"))
  expect_equal(res$gap_to_next, c(-2, -2, NA))
  expect_equal(res$gap_positive, c(0, 0, NA))
  expect_equal(res$overlap_duration, c(2, 2, NA))
})

test_that("one-day flowering intervals have one day of overlap", {
  df <- data.frame(
    species = c("A", "B"),
    start_day = c(5, 5),
    end_day = c(5, 5)
  )

  res <- inter_event_times(df)

  expect_equal(res$species, c("A", "B"))
  expect_equal(res$gap_to_next, c(-1, NA))
  expect_equal(res$gap_positive, c(0, NA))
  expect_equal(res$overlap_duration, c(1, NA))
})

test_that("next-interval columns identify the correct species and dates", {
  df <- data.frame(
    species = c("A", "B", "C"),
    start_day = c(1, 4, 10),
    end_day = c(5, 8, 15)
  )

  res <- inter_event_times(df)

  expect_equal(res$next_species, c("B", "C", NA_character_))
  expect_equal(res$next_start_day, c(4, 10, NA))
  expect_equal(res$next_end_day, c(8, 15, NA))
})

test_that("a single species has no next interval", {
  df <- data.frame(
    species = "A",
    start_day = 1,
    end_day = 5
  )

  res <- inter_event_times(df)

  expect_equal(nrow(res), 1L)
  expect_equal(res$species, "A")

  next_columns <- c(
    "next_species", "next_start_day", "next_end_day",
    "gap_to_next", "gap_positive", "overlap_duration"
  )

  expect_true(all(vapply(
    res[next_columns],
    function(x) all(is.na(x)),
    logical(1)
  )))
})

test_that("empty input returns an empty result with expected columns", {
  df <- data.frame(
    species = character(),
    start_day = integer(),
    end_day = integer()
  )

  res <- inter_event_times(df)

  expect_equal(nrow(res), 0L)
  expect_true(all(c(
    "species", "start_day", "end_day",
    "next_species", "next_start_day", "next_end_day",
    "gap_to_next", "gap_positive", "overlap_duration"
  ) %in% names(res)))
})

test_that("missing required columns produce informative errors", {
  df <- data.frame(
    species = "A",
    start_day = 1,
    end_day = 5
  )

  for (column in names(df)) {
    incomplete <- df[setdiff(names(df), column)]

    expect_error(
      inter_event_times(incomplete),
      paste0("missing required columns: ", column)
    )
  }
})
