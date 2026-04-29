# Layer 5 -- recomposition.
#
# Decomposes, recomposes, and asserts that the recomposed result
# matches the result of running accumulate_downstream() on the
# un-decomposed network. The un-decomposed output is the reference: it
# comes from already-tested hydroloom functions.
#
# Walker exercises exact arithmetic on the dendritic baseline; new_hope
# exercises the multi-stem + divergent path. Both fixtures are lifted
# to file scope so each is enriched and decomposed exactly once.

# ---- shared fixtures (file scope) --------------------------------------

walker_src <- enrich_for_decomposition(load_walker())
new_hope_src <- enrich_for_decomposition(load_new_hope())

decompose_or_null <- function(src, ...) {
  if (!exists("decompose_network",
    envir = asNamespace("hydroloom"), inherits = FALSE)) return(NULL)
  tryCatch(hydroloom::decompose_network(src, ...), error = function(e) NULL)
}

walker_d <- decompose_or_null(walker_src)
new_hope_d <- decompose_or_null(new_hope_src)

test_that("walker recomposed da_sqkm matches accumulate_downstream", {

  decomposition_pending(c("decompose_network", "recompose"))

  expect_recomposes_to_source(walker_d, walker_src, var = "da_sqkm",
    tolerance = 1e-9)

})

test_that("new_hope recomposed da_sqkm matches accumulate_downstream", {

  decomposition_pending(c("decompose_network", "recompose"))

  expect_recomposes_to_source(new_hope_d, new_hope_src, var = "da_sqkm",
    tolerance = 1e-9)

})

test_that("recompose on a sub-threshold basin equals accumulate_downstream", {

  decomposition_pending(c("decompose_network", "recompose"))

  # A stem_threshold larger than any basin metric falls back to the
  # one-stem-per-basin default (the basin's outlet levelpath). Walker
  # has a single basin, so this exercises the path where the basin
  # has only one domain and a single connectivity overlay covering
  # the whole network.
  src <- walker_src

  d <- hydroloom::decompose_network(src,
    stem_threshold = max(src$total_da_sqkm, na.rm = TRUE) + 1)

  expect_recomposes_to_source(d, src, var = "da_sqkm",
    tolerance = 1e-9)

})
