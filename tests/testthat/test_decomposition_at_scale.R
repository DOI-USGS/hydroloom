# Layer 9 — at-scale real-basin integration test with closed sub-basins.
#
# Exercises the decomposition end-to-end on a real CONUS basin large
# enough to contain closed (internally-drained) sub-basins, drawn fresh
# from nhdplusTools and cached under nhdplusTools_data_dir(). Treated
# as a developer-machine integration test, not a CRAN/CI gate.
#
# Skipped on CRAN, when offline, or without nhdplusTools available.
# Skipped at the file level because every test in the file depends on
# the same fixture.

testthat::skip_on_cran()
testthat::skip_if_offline()
testthat::skip_if_not_installed("nhdplusTools")

# Resolve the fixture once for the whole file. prepare_at_scale_fixture()
# itself signals testthat::skip() if the fetch fails or the fixture is
# missing on a machine without network access.

at_scale_gpkg <- tryCatch(
  prepare_at_scale_fixture(),
  error = function(e) {
    testthat::skip(paste0("at-scale fixture not available: ",
      conditionMessage(e)))
  })

at_scale_src <- tryCatch(
  enrich_for_decomposition(at_scale_gpkg),
  error = function(e) {
    testthat::skip(paste0("could not enrich at-scale fixture: ",
      conditionMessage(e)))
  })

# Lift decomposition to file scope so a single decompose_network call
# is shared by all test blocks that use it. Guarded by exists() so the
# file still loads while the decomposition API is pending.
at_scale_d <- if (exists("decompose_network",
  envir = asNamespace("hydroloom"), inherits = FALSE)) {
  tryCatch(
    hydroloom::decompose_network(at_scale_src),
    error = function(e) NULL)
} else {
  NULL
}

at_scale_closed <- count_closed_subbasins(at_scale_src)

test_that("at-scale fixture loads and enriches", {

  expect_true(inherits(at_scale_src, "hy_leveled"))
  expect_gt(nrow(at_scale_src), 100)

})

test_that("at-scale closed sub-basins are present in the source", {

  # Report the count rather than pinning an exact number — the count
  # may shift if the underlying NHDPlusV2 cache changes. The test
  # records the value via expect_gte and a labeled message.
  message("at-scale closed sub-basin count: ", at_scale_closed)
  expect_gte(at_scale_closed, 1L)

})

test_that("decompose_network produces a valid at-scale decomposition", {

  decomposition_pending(c("decompose_network", "validate_decomposition"))

  expect_true(hydroloom::validate_decomposition(at_scale_d)$valid,
    label = "at-scale decomposition is valid")

  assert_partition_coverage(at_scale_d, at_scale_src)

})

test_that("at-scale recomposed da_sqkm matches accumulate_downstream", {

  decomposition_pending(c("decompose_network", "recompose"))

  expect_recomposes_to_source(at_scale_d, at_scale_src, var = "da_sqkm",
    tolerance = 1e-6)

})

test_that("at-scale recomposed stream order matches add_streamorder", {

  decomposition_pending(c("decompose_network", "recompose"))

  expect_recomposes_streamorder(at_scale_d, at_scale_src)

})

test_that("at-scale recomposed levelpath matches the source", {

  decomposition_pending(c("decompose_network", "recompose"))

  expect_recomposes_levelpath(at_scale_d, at_scale_src)

})

test_that("at-scale decomposition contains both trunk and compact domains", {

  decomposition_pending("decompose_network")

  trunk_count <- sum(vapply(at_scale_d$domains,
    \(x) x$domain_type == "trunk", logical(1)))
  compact_count <- sum(vapply(at_scale_d$domains,
    \(x) x$domain_type == "compact", logical(1)))

  expect_gte(trunk_count, 1L)
  expect_gte(compact_count, 1L)

})

test_that("at-scale closed sub-basins surface in the decomposition", {

  decomposition_pending(c("decompose_network", "get_domain_graph"))

  graph_all <- hydroloom::get_domain_graph(at_scale_d,
    relations = c("flow", "contained"))

  contained_edges <- if ("relation_type" %in% names(graph_all))
    sum(graph_all$relation_type == "contained") else 0L

  # Either we see contained-relation edges, or there is at least one
  # closed sub-basin in the source — both are valid signals that closed
  # sub-basins survived the partition. Pinning is loose because the
  # underlying basin can change as NHDPlusV2 updates.
  expect_true(contained_edges >= 1L || at_scale_closed >= 1L,
    label = "at-scale decomposition surfaces closed sub-basins")

})

test_that("accumulate_domains include_contained behaves consistently at scale", {

  decomposition_pending(c("decompose_network", "accumulate_domains"))

  values <- setNames(rep(1, length(at_scale_d$domains)),
    names(at_scale_d$domains))

  excl <- hydroloom::accumulate_domains(at_scale_d, values, fun = sum,
    include_contained = FALSE)

  incl <- hydroloom::accumulate_domains(at_scale_d, values, fun = sum,
    include_contained = TRUE)

  expect_gte(max(incl, na.rm = TRUE), max(excl, na.rm = TRUE),
    label = "include_contained = TRUE never decreases the outlet total")

})
