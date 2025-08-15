test_that("calculate_node_temporal_degree returns correct values in all modes", {
  # Example:
  # A: 1–5, B: 3–7, C: 6–9
  # Overlaps:
  # A–B: 3 days
  # B–C: 2 days
  # A–C: no overlap

  species_data <- data.frame(
    species = c("A", "B", "C"),
    start_day = c(1, 3, 6),
    end_day = c(5, 7, 9),
    stringsAsFactors = FALSE
  )

  g <- create_temporal_network(species_data)

  # Expected degrees:
  # Undirected (total): A=1, B=2, C=1
  total_deg <- calculate_node_temporal_degree(g, mode = "total")
  expect_equal(total_deg$degree[total_deg$species == "A"], 1)
  expect_equal(total_deg$degree[total_deg$species == "B"], 2)
  expect_equal(total_deg$degree[total_deg$species == "C"], 1)

  # Directed (in): A=0, B=1, C=1 (A → B, B → C)
  in_deg <- calculate_node_temporal_degree(g, mode = "in")
  expect_equal(in_deg$degree[match(c("A", "B", "C"), in_deg$species)], c(0, 1, 1))

  # Directed (out): A=1, B=1, C=0
  out_deg <- calculate_node_temporal_degree(g, mode = "out")
  expect_equal(out_deg$degree[match(c("A", "B", "C"), out_deg$species)], c(1, 1, 0))
})
