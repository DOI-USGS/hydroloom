# Layer 8 — robustness on adversarial and pathological inputs.
#
# Pins decompose_network's behavior on cycles, single-flowline
# networks, disconnected pairs, and the larger hr.rds smoke fixture.
# Tests beyond the cycle path are skipped on CRAN to avoid runtime
# bloat.

test_that("decompose_network errors cleanly on loop.rds", {

  decomposition_pending("decompose_network")

  lo <- load_test_rds("loop")

  expect_error(hydroloom::decompose_network(hydroloom::hy(lo)),
    "loop|cycle|graph", ignore.case = TRUE)

})

test_that("decompose_network errors cleanly on loop2.rds", {

  decomposition_pending("decompose_network")

  l2 <- load_test_rds("loop2")

  expect_error(hydroloom::decompose_network(hydroloom::hy(l2)),
    "loop|cycle|graph", ignore.case = TRUE)

})

test_that("decompose_network handles a single-flowline network", {

  decomposition_pending(c("decompose_network", "validate_decomposition"))

  one <- data.frame(
    id = 1L, toid = 0L,
    topo_sort = 1L, levelpath = 1L,
    levelpath_outlet_id = 1L,
    da_sqkm = 1.0)

  src <- hydroloom::hy(one)

  d <- hydroloom::decompose_network(src)

  expect_true(hydroloom::validate_decomposition(d)$valid)
  expect_length(d$domains, 1L)

})

test_that("decompose_network handles a disconnected pair", {

  decomposition_pending(c("decompose_network", "validate_decomposition"))

  pair <- data.frame(
    id = 1:4,
    toid = c(2L, 0L, 4L, 0L),
    topo_sort = c(2L, 1L, 2L, 1L),
    levelpath = c(1L, 1L, 2L, 2L),
    levelpath_outlet_id = c(2L, 2L, 4L, 4L),
    da_sqkm = rep(1.0, 4))

  src <- hydroloom::hy(pair)

  res <- try(hydroloom::decompose_network(src), silent = TRUE)

  if (inherits(res, "try-error")) {

    # acceptable: implementation rejects multi-outlet input outright
    expect_match(attr(res, "condition")$message,
      "outlet|disconnected|multiple", ignore.case = TRUE)

  } else {

    # acceptable: produces two top-level domains
    expect_true(hydroloom::validate_decomposition(res)$valid)
    expect_gte(length(res$domains), 2L)

  }

})

test_that("hr.rds is a scale smoke for decompose_network", {

  testthat::skip_on_cran()

  decomposition_pending(c("decompose_network", "validate_decomposition",
    "recompose"))

  hr <- load_test_rds("hr")

  src <- enrich_for_decomposition(hr,
    name_attribute = "GNIS_ID", weight_attribute = "ArbolateSu")

  d <- hydroloom::decompose_network(src)

  expect_true(hydroloom::validate_decomposition(d)$valid,
    label = "hr.rds decomposition is valid")

  assert_partition_coverage(d, src)

  expect_recomposes_to_source(d, src, var = "da_sqkm",
    tolerance = 1e-9)

})
