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
latency_mat <- calculate_latency(g)

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

## Core Functions

- **`check_temporal_overlap(species_data)`**  
  Detects all pairwise flowering overlaps and returns edges and inclusive overlap durations.

- **`inter_event_times(species_data)`**  
  Calculates the gap (in days) between the end of one species’ event and the start of the next.

- **`create_temporal_network(species_data)`**  
  Builds an **igraph** network with edge weights as overlap durations.

- **`calculate_latency(g)`**  
  Computes a latency matrix of weighted shortest‐path distances using overlap durations.

- **`calculate_memory_with_latency(species_data, latency_matrix)`**  
  Computes a memory metric (lagged correlation) weighted by a latency matrix.

- **`calculate_burstiness(species_data)`**  
  Returns the burstiness index of inter‐event times.

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
