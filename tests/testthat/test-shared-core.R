library(testthat)

test_that("the original five-species example keeps all seven weights", {
  d <- data.frame(species=paste0("Species_",LETTERS[1:5]),
                  start_day=c(10,40,50,60,70), end_day=c(50,60,70,80,100))
  expected <- matrix(c("Species_A","Species_B", "Species_A","Species_C",
    "Species_B","Species_C", "Species_B","Species_D", "Species_C","Species_D",
    "Species_C","Species_E", "Species_D","Species_E"), ncol=2,byrow=TRUE)
  o <- check_temporal_overlap(d)
  expect_equal(o$edges,expected)
  expect_equal(o$weights,c(11,1,11,1,11,1,11))
  expect_equal(calculate_edge_persistence(d),list(edges=expected,persistence=o$weights))
  dates <- data.frame(species=d$species,
    start_date=as.Date("2026-01-01")+d$start_day-1L,
    end_date=as.Date("2026-01-01")+d$end_day-1L)
  expect_equal(check_temporal_overlap(dates),o)
  expect_equal(check_temporal_overlap(d[5:1,]),o)
})

test_that("interval, survey and matrix interfaces share construction", {
  intervals <- data.frame(species=c("A","B","C"), start_day=c(1,3,8), end_day=c(4,6,9))
  surveys <- expand.grid(species=intervals$species, date=as.Date("2026-01-01")+0:8)
  day <- as.integer(format(surveys$date,"%j"))
  i <- match(surveys$species,intervals$species)
  surveys$presence <- as.integer(day>=intervals$start_day[i] & day<=intervals$end_day[i])
  for (direction in c("onset","undirected")) {
    legacy <- create_temporal_network(intervals,direction=direction)
    a <- prepare_phenology(surveys,direction=direction)$groups[[1]]
    matrix_graph <- create_temporal_network(presence_matrix=a$presence,direction=direction)
    expect_equal(igraph::as_edgelist(a$graph),igraph::as_edgelist(legacy))
    expect_equal(igraph::E(a$graph)$weight,igraph::E(legacy)$weight)
    expect_equal(igraph::V(a$graph)$name,igraph::V(legacy)$name)
    expect_equal(igraph::as_data_frame(matrix_graph),igraph::as_data_frame(a$graph))
    expect_equal(a$summary$mean_overlap_days,
                 calculate_persistence_metrics(legacy,intervals)$avg_edge_persistence)
    expected_degree <- if (direction=="onset") calculate_avg_temporal_network_degree(legacy)
                       else calculate_avg_temporal_degree(legacy)
    expect_equal(a$summary$mean_degree,expected_degree)
  }
})

test_that("survey summaries reuse the original node degree results", {
  d <- data.frame(species=c("A","A","B","B"),
    date=c("2026-04-01","2026-04-05","2026-04-03","2026-04-07"), presence=1)
  x <- prepare_phenology(d,"interval")
  g <- x$groups[[1]]$graph
  tab <- summarise_phenology_networks(x,"species")
  for (mode in c("total","in","out")) {
    expected <- calculate_node_temporal_degree(g,mode=mode)
    field <- switch(mode,total="degree","in"="in_degree",out="out_degree")
    expect_equal(tab[[field]],expected$degree[match(tab$species,expected$species)])
  }
})

test_that("unknown and absent days break episodes instead of extending spans", {
  d <- expand.grid(date=as.Date("2026-04-01")+0:4,species=c("A","B"))
  d$presence <- c(1,NA,0,NA,1, 0,1,1,1,0)
  x <- prepare_phenology(d)
  expect_equal(igraph::ecount(x$groups[[1]]$graph),0)
  expect_equal(igraph::vcount(x$groups[[1]]$graph),2)
  expect_equal(x$groups[[1]]$intervals$duration_days,c(1L,1L,3L))
  expect_true(is.na(x$groups[[1]]$presence[2,"A"]))
  expect_equal(x$groups[[1]]$presence[3,"A"],0L)
})

test_that("disjoint episode intersections sum once per species pair", {
  d <- expand.grid(date=as.Date("2026-04-01")+0:5,species=c("A","B"))
  d$presence <- c(1,1,0,0,1,1, 1,1,1,1,1,1)
  a <- prepare_phenology(d)$groups[[1]]
  expect_equal(igraph::ecount(a$graph),1)
  expect_equal(igraph::E(a$graph)$weight,4)
  expect_equal(igraph::E(a$graph)$overlap_jaccard,4/6)
  # Same onset, A ends when B does; species name settles the tie.
  expect_equal(igraph::as_edgelist(a$graph),matrix(c("A","B"),ncol=2))
})

test_that("empty, single and edgeless interval inputs retain their species", {
  empty <- data.frame(species=character(),start_day=integer(),end_day=integer())
  expect_null(check_temporal_overlap(empty)$edges)
  expect_equal(igraph::vcount(create_temporal_network(empty)),0)
  one <- data.frame(species="A",start_day=1,end_day=4)
  expect_equal(igraph::vcount(create_temporal_network(one)),1)
  expect_equal(igraph::ecount(create_temporal_network(one)),0)
  d <- data.frame(species=c("A","B"),start_day=c(1,6),end_day=c(4,9))
  g <- create_temporal_network(d)
  expect_equal(igraph::vcount(g),2)
  expect_equal(calculate_avg_temporal_network_degree(g),0)
  expect_equal(calculate_persistence_metrics(g,d)$avg_node_persistence,4)
  expect_true(is.na(calculate_persistence_metrics(g)$avg_node_persistence))
})

test_that("nested-interval correction and missing-column messages remain", {
  d <- data.frame(species=c("A","B"),start_day=c(1,4),end_day=c(10,5))
  expect_equal(inter_event_times(d)$overlap_duration,c(2,NA))
  for (column in names(d))
    expect_error(inter_event_times(d[setdiff(names(d),column)]),
                 paste0("missing required columns: ",column))
})

test_that("daily presence input validates its explicit calendar contract", {
  p <- cbind(A=c(1L,NA_integer_,0L),B=c(1L,0L,0L))
  expect_equal(igraph::E(create_temporal_network(presence_matrix=p))$weight,1)
  bad <- p; colnames(bad) <- c("A","A")
  expect_error(create_temporal_network(presence_matrix=bad),"unique")
  bad <- p; bad[1,1] <- 2
  expect_error(create_temporal_network(presence_matrix=bad),"0, 1, or NA")
  bad <- p; rownames(bad) <- c("2026-04-01","2026-04-03","2026-04-04")
  expect_error(create_temporal_network(presence_matrix=bad),"consecutive")
  expect_error(create_temporal_network(data.frame(),presence_matrix=p),"not both")
})

test_that("survey and null paths actually delegate to legacy construction and metrics", {
  env <- environment(prepare_phenology)
  skip_if_not(isNamespace(env), "Delegation mocks require an installed package namespace.")
  package <- getNamespaceName(env)
  calls <- new.env(parent=emptyenv())
  calls$build <- 0L; calls$overlap <- 0L; calls$degree <- 0L; calls$persistence <- 0L
  build <- create_temporal_network; overlap <- check_temporal_overlap
  degree <- calculate_avg_temporal_network_degree; persistence <- calculate_persistence_metrics
  local_mocked_bindings(
    create_temporal_network=function(...) { calls$build <- calls$build+1L; build(...) },
    check_temporal_overlap=function(...) { calls$overlap <- calls$overlap+1L; overlap(...) },
    calculate_avg_temporal_network_degree=function(...) { calls$degree <- calls$degree+1L; degree(...) },
    calculate_persistence_metrics=function(...) { calls$persistence <- calls$persistence+1L; persistence(...) },
    .package=package
  )
  d <- data.frame(species=c("A","A","B","B"),
    date=c("2026-04-01","2026-04-05","2026-04-03","2026-04-07"),presence=1)
  x <- prepare_phenology(d,"interval")
  test_phenological_structure(x, nsim=3, seed=42)
  expect_equal(calls$build,4L)
  expect_equal(calls$overlap,4L)
  expect_equal(calls$degree,4L)
  expect_equal(calls$persistence,4L)
})

test_that("changes to legacy metrics flow through survey summaries", {
  env <- environment(prepare_phenology)
  skip_if_not(isNamespace(env), "Delegation mocks require an installed package namespace.")
  original_node <- calculate_node_temporal_degree
  local_mocked_bindings(
    calculate_avg_temporal_network_degree=function(g) 123,
    calculate_persistence_metrics=function(g, species_data=NULL)
      data.frame(avg_node_persistence=NA_real_,avg_edge_persistence=456),
    calculate_node_temporal_degree=function(g,mode="total") {
      tab <- original_node(g,mode); tab$degree <- tab$degree+10; tab
    },
    .package=getNamespaceName(env)
  )
  d <- data.frame(species=c("A","B"),date="2026-04-01",presence=1)
  x <- prepare_phenology(d)
  expect_equal(summarise_phenology_networks(x)$mean_degree,123)
  expect_equal(summarise_phenology_networks(x)$mean_overlap_days,456)
  expect_equal(summarise_phenology_networks(x,"species")$degree,c(11,11))
})
