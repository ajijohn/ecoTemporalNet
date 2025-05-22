library(testthat)
library(ecoTemporalNet)

test_that("calculate_memory_with_latency returns correct memory value", {
  # Simple species_data with known inter-event times 2 and 3
  df <- data.frame(
    species   = c("X","Y","Z"),
    start_day = c(1, 4, 9),
    end_day   = c(2, 6, 12),
    stringsAsFactors = FALSE
  )
  # inter_event_times = c(lead(4)-2, lead(9)-6) = c(2, 3), last NA
  # so vector = c(2, 3, NA), drop last for latency matrix dims 2x2
  iet <- c(2, 3)
  # simple latency matrix 2×2; only [1,2] used
  latency <- matrix(c(0, 0.5,
                      0, 0),
                    nrow = 2, byrow = TRUE)

  # m1 = mean(c(2,3), na.rm=TRUE)=2.5
  # m2 = mean(c(3), na.rm=TRUE)=3
  # sigma1 = sd(c(2,3)) = 0.7071
  # sigma2 = sd(c(3)) = NA (sd of single value is NA), but we only have one term
  # memory = mean((2-2.5)*(3-3)*0.5)/ (sigma1 * sigma2) = 0 / (anything * NA) = NA
  # so expect NA
  mem <- calculate_memory_with_latency(df)
  expect_true(is.na(mem))

  # If we make latency matrix = 1 & ensure non-zero correlation:
  latency2 <- matrix(c(0, 1,
                       0, 0), nrow = 2, byrow = TRUE)
  # Now memory = mean((2-2.5)*(3-3)*1)/(...)=0 -> 0
  mem2 <- calculate_memory_with_latency(df)
  expect_equal(mem2, NA_real_)
})
