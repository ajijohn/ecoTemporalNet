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
      "Species_A", "Species_B",
      "Species_A", "Species_C",
      "Species_B", "Species_C",
      "Species_B", "Species_D",
      "Species_C", "Species_D",
      "Species_C", "Species_E",
      "Species_D", "Species_E"
    ),
    ncol = 2,
    byrow = TRUE
  )

  expected_weights <- c(11, 1, 11, 1, 11, 1, 11)

  expect_equal(result$edges, expected_edges)
  expect_equal(result$weights, expected_weights)
})

test_that("date columns produce the same results as day columns", {
  day_data <- data.frame(
    species   = c("Species_A", "Species_B", "Species_C", "Species_D", "Species_E"),
    start_day = c(10, 40, 50, 60, 70),
    end_day   = c(50, 60, 70, 80, 100)
  )

  date_data <- data.frame(
    species = day_data$species,
    start_date = as.character(
      as.Date("2026-01-01") + day_data$start_day - 1
    ),
    end_date = as.character(
      as.Date("2026-01-01") + day_data$end_day - 1
    )
  )

  expected <- check_temporal_overlap(day_data)

  # YYYY-MM-DD character columns
  expect_equal(check_temporal_overlap(date_data), expected)

  # R Date columns
  date_data$start_date <- as.Date(date_data$start_date)
  date_data$end_date <- as.Date(date_data$end_date)

  expect_equal(check_temporal_overlap(date_data), expected)
})

test_that("date columns take precedence over existing day columns", {
  species_data <- data.frame(
    species = c("A", "B"),
    start_date = c("2026-04-01", "2026-04-04"),
    end_date = c("2026-04-05", "2026-04-08"),
    start_day = c(1, 20),
    end_day = c(2, 21)
  )

  result <- check_temporal_overlap(species_data)

  expect_equal(result$edges, matrix(c("A", "B"), ncol = 2))
  expect_equal(result$weights, 2)
})

test_that("date conversion handles leap day", {
  species_data <- data.frame(
    species = c("A", "B"),
    start_date = c("2024-02-28", "2024-02-29"),
    end_date = c("2024-03-01", "2024-03-02")
  )

  result <- check_temporal_overlap(species_data)

  expect_equal(result$edges, matrix(c("A", "B"), ncol = 2))
  expect_equal(result$weights, 2)
})

test_that("both date columns are required when either is supplied", {
  species_data <- data.frame(
    species = c("A", "B"),
    start_date = c("2026-04-01", "2026-04-04"),
    end_date = c("2026-04-05", "2026-04-08")
  )

  expect_error(
    check_temporal_overlap(species_data[c("species", "start_date")]),
    "Provide both start_date and end_date.",
    fixed = TRUE
  )

  expect_error(
    check_temporal_overlap(species_data[c("species", "end_date")]),
    "Provide both start_date and end_date.",
    fixed = TRUE
  )
})

test_that("missing or invalid dates are rejected", {
  species_data <- data.frame(
    species = c("A", "B"),
    start_date = c("2026-04-01", NA_character_),
    end_date = c("2026-04-05", "2026-04-08")
  )

  expect_error(
    check_temporal_overlap(species_data),
    "Dates must be non-missing",
    fixed = TRUE
  )

  species_data$start_date[2] <- "2026-13-01"

  expect_error(check_temporal_overlap(species_data))
})

test_that("dates with reversed endpoints are rejected", {
  species_data <- data.frame(
    species = c("A", "B"),
    start_date = c("2026-04-06", "2026-04-04"),
    end_date = c("2026-04-05", "2026-04-08")
  )

  expect_error(
    check_temporal_overlap(species_data),
    "start_date must be on or before end_date.",
    fixed = TRUE
  )
})

test_that("dates spanning multiple calendar years are rejected", {
  species_data <- data.frame(
    species = c("A", "B"),
    start_date = c("2026-12-28", "2027-01-01"),
    end_date = c("2027-01-03", "2027-01-05")
  )

  expect_error(
    check_temporal_overlap(species_data),
    "All dates must belong to the same calendar year.",
    fixed = TRUE
  )
})
