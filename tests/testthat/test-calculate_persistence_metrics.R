test_that("calculate_persistence_metrics returns correct values", {
  # Minimal example: A = 1–5, B = 3–7 → overlap = 3 days
  species_data <- data.frame(
    species = c("A", "B"),
    start_day = c(1, 3),
    end_day = c(5, 7),
    stringsAsFactors = FALSE
  )

  # Build network
  g <- create_temporal_network(species_data)

  # Run persistence metric function
  result <- calculate_persistence_metrics(g, species_data)

  # Check output is a data.frame
  expect_s3_class(result, "data.frame")

  # Node persistence = (5 + 5)/2 = 5
  expect_equal(result$avg_node_persistence, 5)

  # Edge persistence = 3
  expect_equal(result$avg_edge_persistence, 3)
})
