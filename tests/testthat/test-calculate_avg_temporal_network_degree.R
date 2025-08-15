test_that("calculate_avg_temporal_network_degree returns correct average degree in directed network", {
  # Example:
  # A: 1–5
  # B: 3–7
  # C: 6–9
  #
  # Overlaps:
  # A→B (A starts earlier)
  # B→C (B starts earlier)
  #
  # Degrees:
  # A: in = 0, out = 1 → total = 1
  # B: in = 1, out = 1 → total = 2
  # C: in = 1, out = 0 → total = 1
  # Mean = (1 + 2 + 1)/3 = 1.33

  species_data <- data.frame(
    species = c("A", "B", "C"),
    start_day = c(1, 3, 6),
    end_day = c(5, 7, 9),
    stringsAsFactors = FALSE
  )

  g <- create_temporal_network(species_data)

  avg_degree <- calculate_avg_temporal_network_degree(g)

  expect_type(avg_degree, "double")
  expect_equal(avg_degree, mean(c(1, 2, 1)), tolerance = 1e-6)
})
