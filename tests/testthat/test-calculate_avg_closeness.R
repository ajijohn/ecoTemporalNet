test_that("calculate_avg_closeness returns correct value", {
  # Network structure (A-B-C):
  # A: 1–5
  # B: 3–7
  # C: 6–9
  #
  # Edges:
  # A—B (3 days), B—C (2 days)
  # Distances (1/weight):
  # A—B = 1/3
  # B—C = 1/2
  #
  # Shortest path distances:
  # A–B = 1/3
  # A–C = (1/3 + 1/2) = 5/6
  # B–C = 1/2
  #
  # Closeness:
  # A = 1 / (1/3 + 5/6) = 1 / (7/6) ≈ 0.857
  # B = 1 / (1/3 + 1/2) = 1 / (5/6) ≈ 1.2
  # C = 1 / (1/2 + 5/6) = 1 / (4/3) ≈ 0.75
  # Mean ≈ (0.857 + 1.2 + 0.75) / 3 ≈ 0.936

  species_data <- data.frame(
    species = c("A", "B", "C"),
    start_day = c(1, 3, 6),
    end_day = c(5, 7, 9),
    stringsAsFactors = FALSE
  )

  g <- create_temporal_network(species_data)

  avg_closeness <- calculate_avg_closeness(g)

  expect_type(avg_closeness, "double")
  expect_equal(avg_closeness, mean(c(1 / (7/6), 1 / (5/6), 1 / (4/3))), tolerance = 1e-6)
})
