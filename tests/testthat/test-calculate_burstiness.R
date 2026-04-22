library(testthat)
library(ecoTemporalNet)


test_that("calculate_burstiness uses non-negative inclusive gaps", {
  df <- data.frame(
    species = c("A", "B", "C", "D"),
    start_day = c(1, 4, 10, 15),
    end_day = c(2, 6, 12, 20),
    stringsAsFactors = FALSE
  )

  gaps <- c(1, 3, 2)
  m_tau <- mean(gaps)
  sigma_tau <- stats::sd(gaps)
  expected_B <- (sigma_tau - m_tau) / (sigma_tau + m_tau)

  B <- calculate_burstiness(df)

  expect_equal(B, expected_B)
})
