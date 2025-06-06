library(testthat)
library(ecoTemporalNet)

test_that("Burstiness eq 22", {


  # Species windows:
  #   A: days 1–5, B: days 3–7, C: days 6–9
  # Inclusive overlaps:
  #   A→B: days {3,4,5} = 3
  #   B→C: days {6,7}   = 2
  species_data <- data.frame(
    species   = c("A", "B", "C"),
    start_day = c(1,   3,   6),
    end_day   = c(5,   7,   9),
    stringsAsFactors = FALSE
  )

  # 1) Compute the inter‐event durations (gap between end of one and start of the next)
  durations <- with(species_data, c(
    species_data$start_day[2] - species_data$end_day[1],
    species_data$start_day[3] - species_data$end_day[2]
  ))

  # 2) Simple burstiness (Goh & Barabási 2008)
  m  <- mean(durations)     # 2.5
  s  <- sd(durations)       # ≈0.7071068

  # 3) Finite‐size burstiness (Eq 22, Kim & Jo 2016)
  cv <- s/m                  # ≈0.28284
  n  <- length(durations)+1  # = 3 events
  num <- sqrt(n+1)*cv - sqrt(n-1)            # √4*cv − √2
  den <- cv*(sqrt(n+1)-2) + sqrt(n-1)         # cv*(2−2) + √2
  expected_B_finite <- num/den

  # Generic calculation
  species_data_ordered <- species_data[order(species_data$start_day), ]
  durations_m2 <- species_data_ordered$start_day[-1] - species_data_ordered$end_day[-nrow(species_data_ordered)]
  #compare it with function
  B_finite <- burstiness_eq22(durations_m2)

  expect_equal(B_finite, expected_B_finite)
})
