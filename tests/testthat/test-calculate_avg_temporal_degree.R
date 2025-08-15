test_that("calculate_avg_temporal_degree returns correct value", {
  # Minimal test example
  # A: 1–5, B: 3–7, C: 6–9
  # Overlaps:
  # A–B: 3 days → edge
  # B–C: 2 days → edge
  # A–C: 0 days → no edge

  species_data <- data.frame(
    species = c("A", "B", "C"),
    start_day = c(1, 3, 6),
    end_day = c(5, 7, 9),
    stringsAsFactors = FALSE
  )

  # Create network
  g <- create_temporal_network(species_data)

  # Degree (undirected):
  # A = 1 (A–B)
  # B = 2 (A–B, B–C)
  # C = 1 (B–C)
  # Average = (1 + 2 + 1)/3 = 1.33

  avg_deg <- calculate_avg_temporal_degree(g)
  expect_equal(avg_deg, mean(c(1, 2, 1)))
})
