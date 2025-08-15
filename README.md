# ecoTemporalNet

**ecoTemporalNet** provides a simple, lightweight toolkit for building and analyzing temporal ecological networks from species phenology data. It automates:

- Detection of flowering‐period overlaps between species  
- Construction of temporal networks where edge weights represent overlap durations  
- Computation of network metrics such as latency, memory, and burstiness  

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
library(igraph)

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

# 3. Compute latency (shortest‐path matrix)
latency_mat <-g calculate_latency(g)

# 4. Calculate memory adjusted by a latency matrix
#    (here: simple toy latency for demonstration)
toy_latency <- matrix(c(0, 1, 0,
                        0, 0, 1,
                        0, 0, 0),
                      nrow = 3, byrow = TRUE)
memory_value <- calculate_memory_with_latency(species_data, toy_latency)

# 5. Compute burstiness of inter‐event times
burst_value <- calculate_burstiness(species_data)
```

---

## Key Functions

| Function | Description |
|----------|-------------|
| `check_temporal_overlap()` | Identifies overlap between flowering windows (inclusive) |
| `create_temporal_network()` | Builds a **directed** temporal network where edges represent overlap |
| `calculate_latency()` | Shortest path latency matrix (weighted by 1/overlap duration) |
| `calculate_memory_with_latency()` | Memory index using weighted correlation by path latency |
| `calculate_burstiness()` | Calculates burstiness of phenological durations |
| `calculate_avg_temporal_degree()` | Average **undirected** node degree (coflowering richness) |
| `calculate_avg_temporal_network_degree()` | Average **directed** degree (in + out edges per node) |
| `calculate_avg_betweenness()` | Average betweenness centrality weighted by overlap |
| `calculate_avg_closeness()` | Average closeness centrality weighted by overlap |
| `calculate_temporal_reachability()` | Returns upstream and downstream sets for each node |
| `calculate_persistence_metrics()` | Returns node and edge persistence (in days) |

---

## Metric Definitions

### Temporal Degree
- **Undirected**: mean number of coflowering partners
- **Directed**: mean (in + out) degree across all nodes

### Centrality
- **Betweenness**: how often species bridge between others
- **Closeness**: how easily a species can reach or be reached

### Reachability
- **Upstream set**: species that can reach a given node
- **Downstream set**: species that a node can reach forward in time

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

**ecoTemporalNet** enables ecologists to quantify phenological synchrony and fragmentation using time-respecting network analysis. Designed for applications in climate change ecology, alpine phenology, and pollinator dynamics.

---
