# Layer 6 — late-bound non-dendritic overrides.
#
# Override entries are synthetic — no real fixture in the package
# carries inter-basin transfers. Tests assert that an empty override
# table is a no-op, that a single fraction transfer moves the expected
# amount arithmetically, and that the validator rejects ill-formed
# overrides.
#
# transfer_type = "conditional" / "scheduled" are deferred until the
# spec firms up.

test_that("empty overrides leave recompose output unchanged", {

  decomposition_pending(c("decompose_network", "recompose"))

  src <- enrich_for_decomposition(load_new_hope())
  d <- hydroloom::decompose_network(src)

  base <- hydroloom::recompose(d,
    domain_results = NULL,
    apply_overrides = FALSE,
    check_mass_balance = FALSE)

  with_empty <- hydroloom::recompose(d,
    domain_results = NULL,
    apply_overrides = TRUE,
    check_mass_balance = FALSE)

  expect_equal(with_empty, base,
    label = "empty override table is a no-op")

})

test_that("a 50% fraction override moves exactly 50% from source to sink", {

  decomposition_pending(c("decompose_network", "recompose"))

  src <- enrich_for_decomposition(load_new_hope())
  d <- hydroloom::decompose_network(src)

  compacts <- names(d$domains)[
    vapply(d$domains, \(x) x$domain_type == "compact", logical(1))]

  if (length(compacts) < 2)
    skip("need at least two compact domains for an override")

  source_id <- compacts[1]
  sink_id <- compacts[2]

  override <- data.frame(
    id = source_id,
    toid = sink_id,
    source_nexus_id = d$domains[[source_id]]$outlet_nexus_id,
    sink_nexus_id = d$domains[[sink_id]]$outlet_nexus_id,
    transfer_type = "fraction",
    transfer_spec = I(list(list(fraction = 0.5))))

  d_with <- d
  d_with$overrides <- override

  base <- hydroloom::recompose(d,
    domain_results = NULL,
    apply_overrides = FALSE,
    check_mass_balance = FALSE)

  applied <- hydroloom::recompose(d_with,
    domain_results = NULL,
    apply_overrides = TRUE,
    check_mass_balance = FALSE)

  # The source domain's outlet contribution should be reduced by 50%
  # and that 50% should appear at the sink. We compare on da_sqkm at
  # the outlet ids of source and sink domains.
  src_outlet_id <- d$domains[[source_id]]$catchments$id[
    nrow(d$domains[[source_id]]$catchments)]
  sink_outlet_id <- d$domains[[sink_id]]$catchments$id[
    nrow(d$domains[[sink_id]]$catchments)]

  base_src <- base$da_sqkm[base$id == src_outlet_id]
  app_src <- applied$da_sqkm[applied$id == src_outlet_id]

  base_sink <- base$da_sqkm[base$id == sink_outlet_id]
  app_sink <- applied$da_sqkm[applied$id == sink_outlet_id]

  expect_equal(app_src, base_src * 0.5, tolerance = 1e-9,
    label = "source loses 50%")

  expect_equal(app_sink - base_sink, base_src * 0.5, tolerance = 1e-9,
    label = "sink gains 50% of source")

})

test_that("validator rejects an override referencing an unknown domain", {

  decomposition_pending(c("decompose_network", "validate_decomposition"))

  src <- enrich_for_decomposition(load_new_hope())
  d <- hydroloom::decompose_network(src)

  bad <- data.frame(
    id = "phantom_source",
    toid = names(d$domains)[1],
    source_nexus_id = "phantom_nexus",
    sink_nexus_id = d$domains[[1]]$outlet_nexus_id,
    transfer_type = "fraction",
    transfer_spec = I(list(list(fraction = 0.5))))

  d_bad <- d
  d_bad$overrides <- bad

  res <- hydroloom::validate_decomposition(d_bad)

  expect_false(res$valid)
  expect_true(any(grepl("override|phantom|unknown",
    res$issues, ignore.case = TRUE)))

})

test_that("validator accepts a decomposition carrying a valid override table", {

  decomposition_pending(c("decompose_network", "validate_decomposition"))

  src <- enrich_for_decomposition(load_new_hope())
  d <- hydroloom::decompose_network(src)

  compacts <- names(d$domains)[
    vapply(d$domains, \(x) x$domain_type == "compact", logical(1))]

  if (length(compacts) < 2)
    skip("need at least two compact domains for a valid override")

  override <- data.frame(
    id = compacts[1],
    toid = compacts[2],
    source_nexus_id = d$domains[[compacts[1]]]$outlet_nexus_id,
    sink_nexus_id = d$domains[[compacts[2]]]$outlet_nexus_id,
    transfer_type = "fraction",
    transfer_spec = I(list(list(fraction = 0.25))))

  d_ok <- d
  d_ok$overrides <- override

  res <- hydroloom::validate_decomposition(d_ok)

  expect_true(res$valid,
    label = "valid override table is accepted")

})
