test_that("calculate_avg_betweenness returns correct average betweenness", {
  # Example:
  # A: 1–5
  # B: 3–7
  # C: 6–9
  # Overlaps:
  # A–B (3 days), B–C (2 days), A–C = no overlap
  #
  # Network (undirected):
  # A—B—C
  # Betweenness:
  # A = 0
  # B = 1
  # C = 0
  # Average = (0 + 1 + 0) / 3 = 0.333...

  species_data <- data.frame(
    species = c("A", "B", "C"),
    start_day = c(1, 3, 6),
    end_day = c(5, 7, 9),
    stringsAsFactors = FALSE
  )

  g <- create_temporal_network(species_data)
  avg_btwn <- calculate_avg_betweenness(g)

  expect_type(avg_btwn, "double")
  expect_equal(avg_btwn, 1 / 3, tolerance = 1e-6)
})
