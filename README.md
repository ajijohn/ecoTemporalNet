# ecoTemporalNet

**ecoTemporalNet** builds and summarizes phenological overlap networks from
species intervals or community survey observations. Flowering is one example:
the same workflow can describe another explicitly selected phenological stage.

The package connects input preparation, weighted network construction, species
and community summaries, and duration-preserving reference comparisons. Survey
and interval workflows share the existing overlap, graph, degree, and persistence
functions rather than maintaining separate implementations.

- **Intervals:** one continuous presence interval per species, in days or dates.
- **Surveys:** long-format presence/absence/unknown records, grouped by site and year.
- **Daily matrices:** repeated presence episodes without filling the gaps between them.
- **Networks:** onset-directed by default, with an undirected option and retained isolates.

Edges represent temporal co-occurrence opportunity, **not measured ecological
interactions**. Presence refers to the selected stage, not organismal occurrence.

## Installation

Install the development version from GitHub:

```r
# install.packages("devtools")
devtools::install_github("ajijohn/ecoTemporalNet")
```

From a local package checkout:

```r
# Run from the package root.
devtools::document()
devtools::install()
```

The survey workflow described below requires the refactored functions to be
included in the installed package. After integrating them locally, reinstall the
package and restart R before trying the examples.

## Quick start: species intervals

Supply one row per species with `species`, `start_day`, and `end_day`. Day values
must be finite integers, with start on or before end. Intervals include both
endpoints. All rows should refer to one community and annual observation period.

```r
library(ecoTemporalNet)

species_data <- data.frame(
  species = c("A", "B", "C"),
  start_day = c(1, 3, 6),
  end_day = c(5, 7, 9)
)

check_temporal_overlap(species_data)
g <- create_temporal_network(species_data)

calculate_node_temporal_degree(g, mode = "total")
calculate_avg_temporal_network_degree(g)
calculate_persistence_metrics(g, species_data)
calculate_latency(g)
```

Input is sorted by onset, end day, then species name. Edges point from earlier to
later onset. Equal-onset direction uses earlier end, then species name; this is
a reproducible convention, not evidence of biological precedence. Species without
overlaps remain in the graph, including in wholly edgeless communities.

For symmetric co-occurrence:

```r
g_undirected <- create_temporal_network(species_data, direction = "undirected")
```

### Calendar dates

Alternatively supply `start_date` and `end_date` as R `Date` values or
`YYYY-MM-DD` strings. All dates must belong to one calendar year. Date columns
are converted to day of year and take precedence if day columns are also present.

```r
date_data <- data.frame(
  species = c("A", "B"),
  start_date = c("2026-04-01", "2026-04-04"),
  end_date = c("2026-04-05", "2026-04-08")
)

g_dates <- create_temporal_network(date_data)
igraph::as_data_frame(g_dates, what = "edges") # Two shared days.
```

## Quick start: community surveys

Survey records have one row per species/date/site/year. Required columns are
`species`, `date`, and `presence`:

| Value | Meaning |
|---|---|
| `1` or `TRUE` | Observed present in the selected phenological stage |
| `0` or `FALSE` | Surveyed and observed absent from that stage |
| `NA`, or an omitted species/date record | Unknown status |

`site` is optional and defaults to `"community"`. Year is derived from dates;
a supplied `year` must agree. Aggregate individual observations explicitly before
analysis: duplicate species/date rows within a site/year are rejected. Filter to
one phenological stage at a time.

This synthetic example has complete daily observations at two sites:

```r
observations <- expand.grid(
  species = c("A", "B", "C"),
  date = as.Date("2026-04-01") + 0:9,
  site = c("north", "south"),
  stringsAsFactors = FALSE
)
day <- as.integer(observations$date - as.Date("2026-04-01")) + 1L
observations$presence <- as.integer(
  (observations$species == "A" & day <= 4L) |
  (observations$species == "B" & day >= 3L & day <= 7L) |
  (observations$species == "C" & day >= 8L)
)
idx <- observations$site == "south" & observations$species == "A"
observations$presence[idx] <- as.integer(day[idx] >= 6L & day[idx] <= 9L)

analysis <- prepare_phenology(observations)
community <- summarise_phenology_networks(analysis)
species <- summarise_phenology_networks(analysis, level = "species")

community[c("site", "n_edges", "mean_degree", "days_with_presence")]
species[c("site", "species", "degree", "in_degree", "out_degree")]
```

The northern site has one edge and ten flowering days; the southern site has
two edges but eight flowering days. More overlap does not necessarily imply
longer seasonal coverage. These are descriptive site/year comparisons, not
statistical tests or assumptions that groups are independent replicates.

### One analysis object

`prepare_phenology()` returns an `eco_phenology` object containing:

- Original observations and calculation settings.
- One group per site/year, with identifying metadata and observation window.
- Observed and analysis presence matrices, dates, positive episodes, and graph.
- Community summaries calculated through the shared metric functions.

Access a graph and apply existing functions directly:

```r
survey_graph <- analysis$groups[[1]]$graph
calculate_node_temporal_degree(survey_graph, mode = "out")
calculate_persistence_metrics(survey_graph)
```

Without interval data, node persistence is `NA`; edge persistence is still
available. This avoids inferring a continuous species duration from incomplete
surveys or separated presence episodes.

### Missing surveys and interval reconstruction

The default `method = "observed"` retains unknown days as `NA`; only confirmed
shared presence contributes to edges. No observation is not the same as absence.

```r
missing_dates <- as.Date(c("2026-04-02", "2026-04-04"))
sparse <- observations[!(observations$date %in% missing_dates), ]
sparse_analysis <- prepare_phenology(sparse)
summarise_phenology_networks(sparse_analysis)[
  c("site", "coverage_lower", "coverage_upper", "unknown_fraction")
]
```

Coverage bounds distinguish confirmed coverage from what could be present on
unresolved days; they are not confidence intervals. With incomplete data, absent
edges do not establish biological non-overlap.

Where continuous presence is justified, choose reconstruction explicitly:

```r
reconstructed <- prepare_phenology(sparse, method = "interval")
```

For species with positive records, this fills presence from first to last
observed presence and assumes absence outside that span within the window.
An observed zero inside the proposed span raises an error. Species with no
positive records keep their original zeros and unknowns. Reconstruction is an
assumption, not a detection model or an estimate of endpoint uncertainty.

By default, each group spans its first-to-last survey date. Set, for example,
`observation_window = c(91L, 120L)` to use a shared day-of-year window; all
observations must lie inside it. Check window length, richness, effort, missingness,
and reconstruction settings before interpreting community differences.

## Repeated intervals and daily matrices

Do not force repeated species episodes into one continuous interval or rename
them as different species. Use a matrix with one column per species and one row
per consecutive day. Combine overlapping interval records as a union, so a shared
day is counted once; preserve intervening gaps.

```r
presence <- cbind(
  A = c(1L, NA_integer_, 0L, 1L, 1L),
  B = c(0L, 1L, 1L, 1L, 0L)
)
rownames(presence) <- as.character(as.Date("2026-04-01") + 0:4)

g_daily <- create_temporal_network(presence_matrix = presence)
igraph::as_data_frame(g_daily, what = "edges")
```

This graph has one shared presence day; the unknown day is not filled. For
repeated episodes, direction uses first and last presence across the whole
window, not separate directions for each episode. When converting interval data
to zeros outside each species' episodes, be explicit that the interval data
justify those absences. The frog vignette demonstrates that conversion.

## Edge weights and metrics

`weight` and `overlap_days` record inclusive shared presence days. Daily-matrix
input additionally provides `jointly_known_days` and `overlap_jaccard`, the ratio
of shared to either-species presence days restricted to jointly known days.

| Function | Behaviour |
|---|---|
| `check_temporal_overlap()` | Pairwise interval overlap and onset-directed edge labels |
| `create_temporal_network()` | Shared interval/daily constructor; onset-directed or undirected |
| `prepare_phenology()` | Survey preparation, grouping, shared graphs, settings, and summaries |
| `summarise_phenology_networks()` | Labelled community or species summaries |
| `test_phenological_structure()` | Duration-preserving randomized reference comparisons |
| `calculate_latency()` | Directed hop distances, or inverse-overlap costs when requested |
| `calculate_edge_persistence()` | Pairwise overlap for one interval per species; reuses overlap calculation |
| `calculate_persistence_metrics()` | Mean interval duration when supplied, and mean collapsed edge weight |
| `calculate_avg_temporal_degree()` | Mean undirected degree |
| `calculate_avg_temporal_network_degree()` | Mean directed total degree (incoming plus outgoing) |
| `calculate_node_temporal_degree()` | Per-species total, incoming, or outgoing degree |
| `calculate_betweenness()` | Directed betweenness, with inverse-overlap costs by default |
| `calculate_avg_betweenness()` | Mean raw undirected betweenness with inverse-overlap costs |
| `calculate_avg_closeness()` | Mean unnormalized undirected closeness with inverse-overlap costs |
| `calculate_temporal_reachability()` | Upstream and downstream counts along directed graph paths |
| `inter_event_times()` | Consecutive-onset signed separation, nonnegative gap, and actual overlap |
| `calculate_burstiness()` | Variability of nonnegative consecutive-interval gaps |
| `calculate_overlap_burstiness()` | Variability of pairwise overlap durations |
| `get_daywise_overlap()` | Expands continuous intervals into daily presence; omits wholly empty days |
| `calculate_prop_edge_persistence()` | Fraction of each species' present days with a neighbour; expects a binary matrix without NA |

### Interpretation and current limitations

- Directed in-degree counts edges from earlier-onset neighbours; out-degree points
  to later-onset neighbours, subject to the tie convention.
- Degree and centrality depend on richness, duration, graph structure, and weighting.
  They do not by themselves establish ecological importance or interaction strength.
- Onset-directed connectance uses `n * (n - 1)` possible pairs; undirected
  connectance uses `n * (n - 1) / 2`. The onset graph has one direction per pair,
  so its connectance cannot exceed 0.5. Direction does not change presence coverage.
- Latency is graph distance, not elapsed time. Reachability and centrality use
  aggregated graph paths rather than explicitly timed movement or transmission.
- `calculate_weighted_overlap_distance()` uses overlap duration directly as cost.
  `calculate_overlap_hop_distance()` and `calculate_overlap_shortest_paths()`
  also use existing weights because their implementation passes `weights = NULL`;
  despite their names, these are not unweighted on graphs with a weight attribute.
- `calculate_betweenness(use_overlap_strength = FALSE)` likewise uses an existing
  weight attribute; that option currently does not disable weighting.
- `calculate_avg_closeness()` can return `NaN` when an isolate is present. Undefined
  values should not automatically be replaced with zero.
- `plot_temporal_network()` reconstructs a directed edge-list graph and omits
  isolates from its display. Use `igraph::plot.igraph()` for an isolate-preserving
  plot or undirected representation.
- `calculate_memory_with_latency()` remains a legacy experimental function. Its
  reachability weighting can exclude the positive-gap variation it aims to measure,
  leaving the statistic undefined. It is not part of the recommended workflow.
- Calendar-date input is restricted to one calendar year per interval community
  or survey group. Cross-year episodes and multi-episode null models are not supported.

See individual function documentation for weighting choices and undefined cases.

## Duration-preserving reference comparisons

For complete daily histories with at most one positive episode per species:

```r
reference <- test_phenological_structure(analysis, nsim = 199, seed = 42)
reference$summary
```

The model preserves species durations, species pool, and each group's window,
then independently places intervals at uniformly sampled feasible onsets. Each
replicate reuses the same network constructor and metrics; direction is recalculated.
Species without presence stay absent. A supplied seed preserves the caller's
random-number state. Increase simulation count as needed for inferential precision.

Unknown histories and repeated episodes are rejected. A complete interval-mode
reconstruction can be used, but results then condition on its assumptions.
The model does not reproduce observation effort, detection, species-specific
seasonal constraints, climate, or ecological interaction mechanisms.

Outputs include simulations, reference quantiles, standardized effects and add-one
one-sided tail probabilities. Quantiles are not confidence intervals; probabilities
are not multiplicity-adjusted. Zero null variance leaves standardized effects
undefined. Mean overlap is undefined in edgeless replicates; these are counted,
and tail probabilities for that metric are withheld if any replicate is undefined.

## Tutorials

- [Survey workflow](vignettes/survey-workflow.Rmd): complete and missing surveys,
  site/year summaries, reconstruction, and reference comparisons.
- [Frog phenology](vignettes/frog_vignette.Rmd): repeated interval records combined
  into species-level daily presence.

For installed vignettes:

```r
browseVignettes("ecoTemporalNet")
```

Build them from a package checkout with:

```r
devtools::build_vignettes()
```

Include `knitr` and `rmarkdown` in `Suggests` and `VignetteBuilder: knitr` in
`DESCRIPTION`. To build vignettes during GitHub installation, use
`devtools::install_github("ajijohn/ecoTemporalNet", build_vignettes = TRUE)`.

## Testing and contributing

```r
# Run from the package root.
devtools::test()
devtools::check()
```

Contributions should include tests, roxygen2 documentation, and runnable examples.
Fork the repository, make changes on a feature branch, and submit a pull request.

## License

MIT — see [LICENSE](LICENSE) for details.
