library(testthat)
library(ecoTemporalNet)

test_that("calculate_burstiness computes correct B", {
  df <- data.frame(
    species   = c("A","B","C","D"),
    start_day = c(1, 4, 9, 15),
    end_day   = c(2, 6, 12, 20),
    stringsAsFactors = FALSE
  )
  # inter_event_times: c(lead(4)-2=2, lead(9)-6=3, lead(15)-12=3, NA)
  iet <- c(2,3,3, NA)
  m_tau <- mean(iet, na.rm = TRUE)    # (2+3+3)/3 = 8/3 ≈ 2.6667
  sigma_tau <- sd(iet, na.rm = TRUE)  # sd of c(2,3,3) = ~0.57735
  expected_B <- (sigma_tau / m_tau - 1) / (sigma_tau / m_tau + 1)

  B <- calculate_burstiness(df)
  expect_equal(B, expected_B)
})
