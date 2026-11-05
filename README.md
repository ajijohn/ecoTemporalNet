# ecoTemporalNet

**ecoTemporalNet** provides tools for building and analyzing temporal ecological networks from species phenology data. It supports:

- Detection of flowering‐period overlaps between species  
- Construction of temporal networks where edge weights represent overlap durations  
- Computation of network metrics such as latency, memory, and burstiness  

Input data should be a data frame with one row per species and columns `species`, `start_day`, and `end_day`. Day values must be finite integers, and each start must be on or before its end. Alternatively, provide `start_date` and `end_date` as `Date` values or `YYYY-MM-DD` strings; all dates must be in the same calendar year. Intervals are inclusive.

---

## Installation

You can install the development version from GitHub with:

```r
# install.packages("devtools")
devtools::install_github("ajijohn/ecoTemporalNet")
```

Or install a local tarball:

```r
# From the package root
devtools::build()
devtools::install_local("ecoTemporalNet_0.1.0.tar.gz")
```

---

## Quick Start

```r
library(ecoTemporalNet)

# Define your species flowering windows
species_data <- data.frame(
  species   = c("A", "B", "C"),
  start_day = c(1,   3,   6),
  end_day   = c(5,   7,   9),
  stringsAsFactors = FALSE
)

# 1. Check pairwise temporal overlap
overlap_result <- check_temporal_overlap(species_data)
edges   <- overlap_result$edges
weights <- overlap_result$weights

# 2. Build the temporal network (igraph)
g <- create_temporal_network(species_data)

# 3. Compute directed hop latency (shortest-path matrix)
latency_mat <- calculate_latency(g)

# To use inverse overlap duration as the edge cost:
strength_latency_mat <- calculate_latency(g, use_overlap_strength = TRUE)

# 4. Calculate burstiness of non-negative gaps between consecutive intervals
burst_value <- calculate_burstiness(species_data)

# 5. Calculate lag-1 memory in interval gaps, discounted by network latency
memory_value <- calculate_memory_with_latency(species_data)
```

Some metrics return `NA` when there are too few usable observations or when the statistic is undefined.

---

## Key Functions

| Function | Description |
|----------|-------------|
| `check_temporal_overlap()` | Finds inclusive pairwise overlaps and their durations; edges point from the earlier-starting to the later-starting species |
| `create_temporal_network()` | Builds a directed `igraph` network with overlap duration as edge weight |
| `calculate_latency()` | Directed shortest-path matrix; counts hops by default or uses inverse overlap duration when `use_overlap_strength = TRUE` |
| `calculate_memory_with_latency()` | Computes lag-1 correlation in non-negative consecutive-interval gaps, weighted by directed network latency |
| `calculate_burstiness()` | Calculates burstiness of non-negative gaps between consecutive intervals |
| `calculate_overlap_burstiness()` | Calculates burstiness of pairwise overlap durations |
| `inter_event_times()` | Orders intervals and returns their signed separation and non-negative gap |
| `calculate_edge_persistence()` | Returns pairwise overlap durations |
| `calculate_persistence_metrics()` | Returns average node flowering duration and average edge overlap duration |
| `calculate_avg_temporal_degree()` | Average degree after treating the network as undirected |
| `calculate_avg_temporal_network_degree()` | Average directed total degree (in-degree plus out-degree) |
| `calculate_node_temporal_degree()` | Per-species degree in total, incoming, or outgoing mode |
| `calculate_avg_betweenness()` | Average undirected betweenness, using inverse overlap duration as distance |
| `calculate_avg_closeness()` | Average undirected closeness, using inverse overlap duration as distance |
| `calculate_temporal_reachability()` | Returns the number of upstream and downstream reachable nodes per species |
| `calculate_weighted_overlap_distance()` | Shortest paths using overlap duration as edge weight |
| `calculate_overlap_hop_distance()` | Undirected shortest-path distances counted in hops |
| `calculate_overlap_shortest_paths()` | Unweighted shortest-path matrix |
| `calculate_prop_edge_persistence()` | Per-species proportion of flowering days with at least one coflowering species |
| `plot_temporal_network()` | Plots a network with edge widths and labels based on overlap weights |

---

## Metric Definitions

### Temporal Degree
- **Undirected**: mean number of coflowering partners
- **Directed**: mean (in + out) degree across all nodes

### Centrality
- **Betweenness**: how often species bridge between others
- **Closeness**: how easily a species can reach or be reached

### Reachability
- **Upstream count**: number of nodes that can reach a given node
- **Downstream count**: number of nodes a node can reach by following directed edges

### Persistence
- **Node persistence**: average flowering duration per species
- **Edge persistence**: average coflowering duration across all links

---

## Testing

We use **testthat** for unit tests. To run all tests:

```r
devtools::test()
```

---

## Vignette

A step‐by‐step demo is available:

```r
browseVignettes("ecoTemporalNet")
```

or build it via:

```r
devtools::build_vignettes()
```

---

## Contributing

1. Fork the repository  
2. Create a feature branch  
3. Write code & tests  
4. Document with roxygen2  
5. Submit a pull request  

---

## License

MIT &mdash; see the [LICENSE](LICENSE) file for details.

---

## About

**ecoTemporalNet** helps ecologists describe phenological overlap and temporal structure in ecological networks. It provides functions for constructing directed overlap networks and summarizing their connectivity, reachability, persistence, and timing patterns.

---
