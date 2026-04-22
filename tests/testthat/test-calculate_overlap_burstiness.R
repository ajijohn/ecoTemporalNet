test_that("calculate_overlap_burstiness returns expected value for variable overlaps", {
  species_data <- data.frame(
    species = c("A", "B", "C"),
    start_day = c(1, 3, 8),
    end_day = c(10, 6, 12),
    stringsAsFactors = FALSE
  )

  # overlaps:
  # A-B = 4 days (3:6)
  # A-C = 3 days (8:10)
  # B-C = 0 overlap
  # x = c(4, 3)
  expected <- (stats::sd(c(4, 3)) - mean(c(4, 3))) /
    (stats::sd(c(4, 3)) + mean(c(4, 3)))

  expect_equal(
    calculate_overlap_burstiness(species_data),
    expected
  )
})

test_that("calculate_overlap_burstiness returns NA with fewer than two overlaps", {
  species_data <- data.frame(
    species = c("A", "B", "C"),
    start_day = c(1, 5, 20),
    end_day = c(10, 7, 25),
    stringsAsFactors = FALSE
  )

  # only A-B overlaps
  expect_true(is.na(calculate_overlap_burstiness(species_data)))
})

test_that("calculate_overlap_burstiness returns -1 for perfectly regular positive overlaps", {
  species_data <- data.frame(
    species = c("A", "B", "C"),
    start_day = c(1, 3, 5),
    end_day = c(6, 8, 10),
    stringsAsFactors = FALSE
  )

  # overlaps:
  # A-B = 4
  # A-C = 2
  # B-C = 4
  # not regular, so use a mocked persistence object instead
  mock_ep <- list(
    edges = matrix(c("A", "B", "A", "C"), ncol = 2, byrow = TRUE),
    persistence = c(5, 5)
  )

  with_mocked_bindings(
    calculate_edge_persistence = function(species_data) mock_ep,
    {
      expect_equal(calculate_overlap_burstiness(species_data), -1)
    }
  )
})

test_that("calculate_overlap_burstiness returns NA when persistence object is NULL-like", {
  species_data <- data.frame(
    species = c("A", "B"),
    start_day = c(1, 2),
    end_day = c(3, 4),
    stringsAsFactors = FALSE
  )

  with_mocked_bindings(
    calculate_edge_persistence = function(species_data) NULL,
    {
      expect_true(is.na(calculate_overlap_burstiness(species_data)))
    }
  )
})
