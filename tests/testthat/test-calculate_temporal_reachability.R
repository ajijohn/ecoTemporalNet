test_that("calculate_temporal_reachability returns correct upstream/downstream sizes", {
  # Example:
  # A: 1–5
  # B: 3–7
  # C: 6–9
  #
  # Overlaps:
  # A→B (3 days) because A starts earlier
  # B→C (2 days) because B starts earlier
  # A→C is not valid (no overlap)
  #
  # So:
  # A → B → C
  # Reachability:
  # A: downstream = 2, upstream = 0
  # B: downstream = 1, upstream = 1
  # C: downstream = 0, upstream = 2

  species_data <- data.frame(
    species = c("A", "B", "C"),
    start_day = c(1, 3, 6),
    end_day = c(5, 7, 9),
    stringsAsFactors = FALSE
  )

  # Use a modified version of create_temporal_network() if needed to ensure time-respecting direction
  g <- create_temporal_network(species_data)

  reach_df <- calculate_temporal_reachability(g)

  expect_s3_class(reach_df, "data.frame")
  expect_named(reach_df, c("species", "upstream", "downstream"))

  expect_equal(reach_df$upstream[reach_df$species == "A"], 0)
  expect_equal(reach_df$downstream[reach_df$species == "A"], 2)

  expect_equal(reach_df$upstream[reach_df$species == "B"], 1)
  expect_equal(reach_df$downstream[reach_df$species == "B"], 1)

  expect_equal(reach_df$upstream[reach_df$species == "C"], 2)
  expect_equal(reach_df$downstream[reach_df$species == "C"], 0)
})
