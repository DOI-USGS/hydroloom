# Layer 7 — contained-basin policy.
#
# No real contained-basin example exists in the package's small test
# data, so the fixture is built inline here. The fixture is one trunk
# of 7 features plus a 3-feature endorheic subgraph; a hand-built
# domain_decomposition wraps both as separate domains and links them
# via a "contained" relation row in domain_graph.
#
# These tests pin the include_contained policy flag semantics: with
# the flag off, contained basins do not contribute; with the flag on,
# they do.

# ---- inline fixture builders ------------------------------------------

#' Build the contained-basin fixture: trunk (7 rows) + endorheic
#' subgraph (3 rows). Returns the source hy_leveled.
make_contained_source <- function() {

  trunk <- data.frame(
    id = 1:7,
    toid = c(2L, 3L, 4L, 5L, 6L, 7L, 0L),
    topo_sort = 7:1,
    levelpath = rep(1L, 7),
    levelpath_outlet_id = rep(7L, 7),
    da_sqkm = rep(1.0, 7))

  endo <- data.frame(
    id = 8:10,
    toid = c(9L, 10L, 0L),
    topo_sort = 3:1,
    levelpath = rep(2L, 3),
    levelpath_outlet_id = rep(10L, 3),
    da_sqkm = rep(1.0, 3))

  hydroloom::hy(rbind(trunk, endo))

}

#' Build the contained-basin decomposition: 1 trunk + 1 contained
#' compact + a "contained" edge in domain_graph.
make_contained_decomposition <- function() {

  decomposition_pending(c("hy_domain"))

  src <- make_contained_source()

  trunk <- make_minimal_hy_domain(
    hydroloom::hy(src[1:7, ]),
    domain_type = "trunk",
    outlet_nexus_id = "n_outlet")

  endo <- make_minimal_hy_domain(
    hydroloom::hy(src[8:10, ]),
    domain_type = "compact",
    domain_id = "E1",
    outlet_nexus_id = "n_endo",
    containing_domain_id = "T1")

  make_minimal_decomposition(
    domains = list(T1 = trunk, E1 = endo),
    domain_graph = data.frame(
      id = "E1", toid = "T1",
      nexus_id = "n_endo", nexus_position = NA_real_,
      relation_type = "contained"),
    nexus_registry = data.frame(nexus_id = c("n_outlet", "n_endo")),
    source_network = src)

}

# ---- assertions on the fixture ----------------------------------------

test_that("hand-built contained decomposition validates", {

  decomposition_pending(c("hy_domain", "validate_decomposition"))

  d <- make_contained_decomposition()

  expect_true(hydroloom::validate_decomposition(d)$valid)

})

test_that("accumulate_domains excludes contained by default", {

  decomposition_pending(c("hy_domain", "accumulate_domains"))

  d <- make_contained_decomposition()

  values <- setNames(rep(1, length(d$domains)), names(d$domains))

  res <- hydroloom::accumulate_domains(d, values, fun = sum,
    include_contained = FALSE)

  # at the trunk outlet, only the trunk itself contributes -> 1
  expect_equal(max(res, na.rm = TRUE), 1)

})

test_that("accumulate_domains includes contained when requested", {

  decomposition_pending(c("hy_domain", "accumulate_domains"))

  d <- make_contained_decomposition()

  values <- setNames(rep(1, length(d$domains)), names(d$domains))

  res <- hydroloom::accumulate_domains(d, values, fun = sum,
    include_contained = TRUE)

  # trunk + endorheic -> 2
  expect_equal(max(res, na.rm = TRUE), 2)

})

test_that("get_domain_graph filters by relation_type", {

  decomposition_pending(c("hy_domain", "get_domain_graph"))

  d <- make_contained_decomposition()

  flow_only <- hydroloom::get_domain_graph(d, relations = "flow")
  expect_equal(nrow(flow_only), 0L)

  with_contained <- hydroloom::get_domain_graph(d,
    relations = c("flow", "contained"))
  expect_equal(nrow(with_contained), 1L)

})

test_that("navigate_domain_graph respects include_contained", {

  decomposition_pending(c("hy_domain", "navigate_domain_graph"))

  d <- make_contained_decomposition()

  up_strict <- hydroloom::navigate_domain_graph(d, "T1", "up",
    include_contained = FALSE)

  expect_setequal(as.character(up_strict), "T1")

  up_inclusive <- hydroloom::navigate_domain_graph(d, "T1", "up",
    include_contained = TRUE)

  expect_setequal(as.character(up_inclusive), c("T1", "E1"))

})
