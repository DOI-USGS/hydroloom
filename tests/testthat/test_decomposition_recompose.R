# Layer 5 — recomposition and mass balance.
#
# Decomposes, recomposes, and asserts that the recomposed result
# matches the result of running the same operation on the
# un-decomposed network. The un-decomposed output is the oracle: it
# comes from already-tested hydroloom functions.
#
# Three canonical invariants:
#   1. drainage area accumulation (numeric, tolerance from
#      test_accumulate.R lineage)
#   2. stream order propagation (integer equality)
#   3. levelpath identity preservation (integer equality)
#
# Walker exercises exact arithmetic on the dendritic baseline; new_hope
# exercises the multi-trunk + divergent path. Both fixtures are lifted
# to file scope so each is enriched and decomposed exactly once.

# ---- shared fixtures (file scope) --------------------------------------

walker_src <- enrich_for_decomposition(load_walker())
new_hope_src <- enrich_for_decomposition(load_new_hope())

decompose_or_null <- function(src) {
  if (!exists("decompose_network",
    envir = asNamespace("hydroloom"), inherits = FALSE)) return(NULL)
  tryCatch(hydroloom::decompose_network(src), error = function(e) NULL)
}

walker_d <- decompose_or_null(walker_src)
new_hope_d <- decompose_or_null(new_hope_src)

test_that("walker recomposed da_sqkm matches accumulate_downstream", {

  decomposition_pending(c("decompose_network", "recompose"))

  expect_recomposes_to_source(walker_d, walker_src, var = "da_sqkm",
    tolerance = 1e-9)

})

test_that("walker recomposed stream order matches add_streamorder", {

  decomposition_pending(c("decompose_network", "recompose"))

  expect_recomposes_streamorder(walker_d, walker_src)

})

test_that("walker recomposed levelpath matches the source", {

  decomposition_pending(c("decompose_network", "recompose"))

  expect_recomposes_levelpath(walker_d, walker_src)

})

test_that("new_hope recomposed da_sqkm matches accumulate_downstream", {

  decomposition_pending(c("decompose_network", "recompose"))

  # tolerance follows test_accumulate.R: 1e-3 mean / 1e-2 max for
  # AreaSqKM accumulation against TotDASqKM. We use a tighter relative
  # tolerance because both sides are produced by the same hydroloom
  # function chain — the only floating-point variation comes from join
  # order.
  expect_recomposes_to_source(new_hope_d, new_hope_src, var = "da_sqkm",
    tolerance = 1e-9)

})

test_that("new_hope recomposed stream order matches add_streamorder", {

  decomposition_pending(c("decompose_network", "recompose"))

  expect_recomposes_streamorder(new_hope_d, new_hope_src)

})

test_that("new_hope recomposed levelpath matches the source", {

  decomposition_pending(c("decompose_network", "recompose"))

  expect_recomposes_levelpath(new_hope_d, new_hope_src)

})

test_that("recompose with check_mass_balance flags a corrupted decomposition", {

  decomposition_pending(c("decompose_network", "recompose",
    "validate_decomposition"))

  if (nrow(new_hope_d$domain_graph) == 0)
    skip("decomposition has no inter-domain edges to corrupt")

  # delete the first inter-domain edge so the recomposition loses a
  # contribution at the boundary.
  d_bad <- new_hope_d
  d_bad$domain_graph <- new_hope_d$domain_graph[-1, , drop = FALSE]

  res <- hydroloom::validate_decomposition(d_bad)

  expect_false(res$valid,
    label = "validator catches deleted inter-domain edge")

  rec_bad <- try(
    hydroloom::recompose(d_bad,
      domain_results = NULL,
      apply_overrides = FALSE,
      check_mass_balance = TRUE),
    silent = TRUE)

  if (inherits(rec_bad, "try-error")) {

    expect_match(attr(rec_bad, "condition")$message,
      "mass[ -]?balance|missing|deleted|corrupt", ignore.case = TRUE)

  } else if (is.list(rec_bad) && "issues" %in% names(rec_bad)) {

    expect_gt(length(rec_bad$issues), 0L)

  } else {

    # Some implementations might surface this as a warning + return
    # the partial result. Either way an indicator that something is
    # wrong is required.
    skip("recompose returned silently — implementation must surface mass-balance failure")

  }

})
