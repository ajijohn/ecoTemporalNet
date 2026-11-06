library(testthat)

survey_fixture <- function() {
  d <- expand.grid(species = c("A", "B", "C"),
                   date = as.Date("2026-04-01") + 0:4,
                   stringsAsFactors = FALSE)
  day <- as.integer(d$date - as.Date("2026-04-01")) + 1L
  d$presence <- as.integer((d$species == "A" & day <= 3L) |
                          (d$species == "B" & day >= 3L))
  d
}

test_that("graphs and daily histories match a hand-calculated community", {
  x <- prepare_phenology(survey_fixture())
  a <- x$groups[[1]]
  expect_s3_class(x, "eco_phenology")
  expect_equal(unname(a$presence), cbind(c(1L,1L,1L,0L,0L), c(0L,0L,1L,1L,1L), rep(0L,5)))
  expect_equal(igraph::vcount(a$graph), 3)
  expect_true(igraph::is_directed(a$graph))
  expect_equal(igraph::ecount(a$graph), 1)
  expect_equal(igraph::E(a$graph)$overlap_days, 1)
  expect_equal(igraph::E(a$graph)$weight, 1)
  expect_equal(igraph::E(a$graph)$overlap_jaccard, 1/5)
  expect_equal(igraph::E(a$graph)$jointly_known_days, 5L)
  tab <- summarise_phenology_networks(x)
  expect_equal(tab$mean_degree, 2/3)
  expect_equal(tab$connectance, 1/6)
  expect_equal(tab$coverage_lower, 1)
  expect_equal(tab$coverage_upper, 1)
  species <- summarise_phenology_networks(x, "species")
  expect_equal(species$degree, c(1, 1, 0))
  expect_equal(species$exclusive_presence_days, c(2, 2, 0))
  expect_equal(a$intervals$duration_days, c(3L, 3L))
})

test_that("absent, omitted, and explicitly missing records remain distinct", {
  d <- survey_fixture()
  d$presence[d$species == "A" & d$date == as.Date("2026-04-02")] <- NA_integer_
  d <- d[!(d$species == "B" & d$date == as.Date("2026-04-02")), ]
  a <- prepare_phenology(d)$groups[[1]]
  expect_true(is.na(a$presence[2, "A"]))
  expect_true(is.na(a$presence[2, "B"]))
  expect_equal(a$presence[2, "C"], 0L)
  expect_equal(a$summary$unresolved_days, 1L)
  expect_equal(a$summary$coverage_lower, 4/5)
  expect_equal(a$summary$coverage_upper, 1)
  expect_error(test_phenological_structure(prepare_phenology(d), 9), "unknown days")
})

test_that("interval reconstruction is explicit and preserves observations", {
  d <- data.frame(species = c("A","A","B","B"),
    date = c("2026-04-01","2026-04-05","2026-04-03","2026-04-07"), presence = 1)
  original <- d
  observed <- prepare_phenology(d)
  x <- prepare_phenology(d, method = "interval")
  expect_identical(d, original)
  expect_true(anyNA(observed$groups[[1]]$presence))
  expect_false(anyNA(x$groups[[1]]$presence))
  expect_equal(colSums(x$groups[[1]]$presence), c(A=5L, B=5L))
  expect_equal(igraph::E(x$groups[[1]]$graph)$overlap_days, 3)
  expect_identical(x$groups[[1]]$observed_presence, observed$groups[[1]]$presence)
  contradiction <- rbind(d, data.frame(species="A", date="2026-04-02", presence=0))
  expect_error(prepare_phenology(contradiction, "interval"), "Observed absence")
})

test_that("species without positive records are not reconstructed as absent", {
  d <- data.frame(species=c("A","A","B"), date=c("2026-04-01","2026-04-03","2026-04-02"), presence=c(1,1,0))
  x <- prepare_phenology(d, "interval")
  expect_equal(unname(x$groups[[1]]$presence[,"B"]), c(NA_integer_,0L,NA_integer_))
  expect_error(test_phenological_structure(x, 9), "unknown days")
})

test_that("row order does not affect analysis", {
  d <- survey_fixture()
  a <- prepare_phenology(d)$groups[[1]]
  b <- prepare_phenology(d[rev(seq_len(nrow(d))), ])$groups[[1]]
  expect_identical(a$presence, b$presence)
  expect_equal(igraph::as_data_frame(a$graph), igraph::as_data_frame(b$graph))
  expect_identical(a$summary, b$summary)
})

test_that("site/year grouping prevents cross-community edges", {
  d <- survey_fixture()
  d$site <- "north"
  other <- d; other$site <- "south"; other$date <- other$date + 365
  other$presence <- 0L
  x <- prepare_phenology(rbind(d,other))
  tab <- summarise_phenology_networks(x)
  expect_equal(nrow(tab), 2L)
  expect_equal(tab$site, c("north","south"))
  expect_equal(tab$year, c(2026L,2027L))
  expect_equal(tab$n_edges, c(1L,0L))
  expect_equal(nrow(summarise_phenology_networks(x,"species")), 6L)
  null <- test_phenological_structure(x, 5, seed=1)
  expect_equal(nrow(null$summary), 8L)
  expect_equal(nrow(null$simulations), 10L)
})

test_that("observation windows include unknown days and validate year boundaries", {
  d <- survey_fixture()
  x <- prepare_phenology(d, observation_window=c(90,96))
  expect_equal(nrow(x$groups[[1]]$presence), 7L)
  expect_equal(x$groups[[1]]$summary$unresolved_days, 2L)
  expect_error(prepare_phenology(d, observation_window=c(1,3)), "outside")
  expect_error(prepare_phenology(d, observation_window=c(1,366)), "exceeds")
  leap <- data.frame(species="A", date=c("2024-02-28","2024-03-01"), presence=1)
  x <- prepare_phenology(leap,"interval")
  expect_equal(nrow(x$groups[[1]]$presence), 3L)
  expect_equal(x$groups[[1]]$presence["2024-02-29","A"], 1L)
})

test_that("repeated episodes are retained and rejected by the interval null", {
  d <- data.frame(species="A", date=as.Date("2026-04-01")+0:2, presence=c(1,0,1))
  x <- prepare_phenology(d)
  expect_equal(nrow(x$groups[[1]]$intervals), 2L)
  expect_error(test_phenological_structure(x, 9), "continuous")
})

test_that("null model preserves fixed cases and handles undefined metrics", {
  d <- survey_fixture(); d$presence <- as.integer(d$species != "C")
  x <- prepare_phenology(d)
  result <- test_phenological_structure(x, 9, seed=1)
  expect_true(all(result$simulations$mean_degree == 2/3))
  expect_true(all(result$simulations$mean_overlap_days == 5))
  expect_true(all(is.na(result$summary$standardized_effect)))
  expect_true(all(result$summary$p_upper == 1))
  d$presence <- 0L
  result <- test_phenological_structure(prepare_phenology(d), 9, seed=1)
  overlap <- result$summary[result$summary$metric=="mean_overlap_days",]
  expect_equal(overlap$n_undefined, 9L)
  expect_true(is.na(overlap$p_upper))
  expect_equal(result$simulations$coverage_lower, rep(0,9))
})

test_that("random interval placement agrees with an enumerable null", {
  # Two one-day species, each independently placed on one of two days.
  # Of four equally likely placements, two overlap.
  d <- expand.grid(species=c("A","B"), date=as.Date("2026-04-01")+0:1)
  d$presence <- as.integer(d$date==as.Date("2026-04-01"))
  r <- test_phenological_structure(prepare_phenology(d), 2000, seed=18)
  expect_equal(mean(r$simulations$mean_degree), .5, tolerance=.05)
  expect_equal(mean(r$simulations$coverage_lower), .75, tolerance=.025)
  expect_true(all(r$simulations$mean_degree %in% c(0,1)))
  expect_true(all(r$simulations$coverage_lower %in% c(.5,1)))
  # Independent enumeration verifies every possible value, not just a mean.
  exact <- expand.grid(a=1:2,b=1:2)
  expect_equal(sort(unique(r$simulations$mean_degree)), sort(unique(as.numeric(exact$a==exact$b))))
})

test_that("seeded results reproduce and preserve the caller's RNG state", {
  x <- prepare_phenology(survey_fixture())
  set.seed(109); before <- .Random.seed
  a <- test_phenological_structure(x, 19, seed=42)
  expect_identical(.Random.seed, before)
  b <- test_phenological_structure(x, 19, seed=42)
  expect_identical(a,b)
  expect_error(test_phenological_structure(x,1), "at least two")
  expect_error(test_phenological_structure(x,2,seed=-1), "seed")
})

test_that("invalid survey data receive informative errors", {
  d <- survey_fixture()
  expect_error(prepare_phenology(d[FALSE,]), "nonempty")
  expect_error(prepare_phenology(d[c("species","date")]), "presence")
  bad <- d; bad$presence[1] <- 2
  expect_error(prepare_phenology(bad), "presence")
  bad <- d; bad$species[1] <- NA_character_
  expect_error(prepare_phenology(bad), "species")
  bad <- d; bad$date <- as.character(bad$date); bad$date[1] <- "2026-02-30"
  expect_error(prepare_phenology(bad), "valid")
  bad <- d; bad$year <- 2025
  expect_error(prepare_phenology(bad), "year must match")
  expect_error(prepare_phenology(rbind(d,d[1,])), "Duplicate")
})


test_that("direction defaults to onset and can be explicitly undirected", {
  d <- survey_fixture()
  onset <- prepare_phenology(d)
  undirected <- prepare_phenology(d, direction = "undirected")
  expect_identical(onset$settings$direction, "onset")
  expect_true(onset$settings$directed)
  expect_false(undirected$settings$directed)
  a <- onset$groups[[1]]; b <- undirected$groups[[1]]
  expect_true(igraph::is_directed(a$graph))
  expect_false(igraph::is_directed(b$graph))
  expect_equal(igraph::as_edgelist(a$graph), matrix(c("A","B"), ncol=2))
  expect_equal(a$summary$connectance, 1/6)
  expect_equal(b$summary$connectance, 1/3)
  expect_equal(a$summary$mean_degree, b$summary$mean_degree)
  expect_equal(a$presence, b$presence)
  expect_equal(a$summary$coverage_lower, b$summary$coverage_lower)
  species <- summarise_phenology_networks(onset, "species")
  expect_equal(species$in_degree, c(0,1,0))
  expect_equal(species$out_degree, c(1,0,0))
  species <- summarise_phenology_networks(undirected, "species")
  expect_true(all(is.na(species$in_degree)))
  expect_true(all(is.na(species$out_degree)))
  expect_error(prepare_phenology(d, direction="backwards"), "arg")
})

test_that("onset orientation overrides alphabetic species order", {
  d <- expand.grid(species=c("A","Z"), date=as.Date("2026-04-01")+0:3)
  day <- as.integer(d$date-as.Date("2026-04-01"))+1L
  d$presence <- as.integer((d$species=="Z" & day<=3) | (d$species=="A" & day>=2))
  g <- prepare_phenology(d)$groups[[1]]$graph
  expect_equal(igraph::as_edgelist(g), matrix(c("Z","A"), ncol=2))
  expect_equal(igraph::E(g)$overlap_days, 2)
})

test_that("equal onsets use last presence then species name", {
  d <- expand.grid(species=c("A","B","Z"), date=as.Date("2026-04-01")+0:2)
  day <- as.integer(d$date-as.Date("2026-04-01"))+1L
  d$presence <- as.integer(d$species=="A" | day<=2)
  a <- prepare_phenology(d)$groups[[1]]$graph
  b <- prepare_phenology(d[rev(seq_len(nrow(d))), ])$groups[[1]]$graph
  edges <- igraph::as_edgelist(a)
  expect_equal(edges, matrix(c("B","A","Z","A","B","Z"), ncol=2, byrow=TRUE))
  expect_equal(igraph::as_edgelist(b), edges)
  expect_true(igraph::is_dag(a))
  expect_equal(igraph::ecount(a), 3)
})

test_that("null models inherit direction for every simulation", {
  d <- survey_fixture()
  onset <- test_phenological_structure(prepare_phenology(d), 29, seed=41)
  undirected <- test_phenological_structure(
    prepare_phenology(d, direction="undirected"), 29, seed=41)
  expect_identical(onset$settings$preparation$direction, "onset")
  expect_identical(undirected$settings$preparation$direction, "undirected")
  expect_equal(onset$simulations$connectance * 2, undirected$simulations$connectance)
  for (metric in c("mean_degree","mean_overlap_days","coverage_lower"))
    expect_equal(onset$simulations[[metric]], undirected$simulations[[metric]])
  expect_true(all(onset$simulations$connectance <= .5))
})

test_that("direction is recomputed rather than copied during randomization", {
  p <- cbind(A=c(1L,1L,0L), B=c(0L,1L,1L))
  d <- expand.grid(date=as.Date("2026-04-01")+0:2, species=c("A","B"))
  d$presence <- as.vector(p)
  original <- prepare_phenology(d)$groups[[1]]$graph
  d$presence <- as.vector(p[3:1,,drop=FALSE])
  reversed <- prepare_phenology(d)$groups[[1]]$graph
  expect_equal(igraph::as_edgelist(original), matrix(c("A","B"), ncol=2))
  expect_equal(igraph::as_edgelist(reversed), matrix(c("B","A"), ncol=2))
})

test_that("null comparisons reject stale or inconsistent direction settings", {
  x <- prepare_phenology(survey_fixture())
  x$settings$direction <- NULL
  expect_error(test_phenological_structure(x, 3), "Rebuild x")
  x <- prepare_phenology(survey_fixture())
  x$settings$direction <- "undirected"
  expect_error(test_phenological_structure(x, 3), "does not match")
})
