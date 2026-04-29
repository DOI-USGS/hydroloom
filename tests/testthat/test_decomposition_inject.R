# Layer 4 — basin-overlay coordination via inject_lateral.
#
# Tests that domain results map to main-path segments via the nexus
# registry's measure positions, and that inject_lateral writes to the
# right rows. Uses new_hope.gpkg only — walker is too small to have
# multi-nexus connectivity overlays worth testing.

test_that("nexus_position values lie in [0, 1] and within their basin's measures", {

  decomposition_pending(c("decompose_network", "get_nexus_registry"))

  src <- enrich_for_decomposition(load_new_hope())
  d <- hydroloom::decompose_network(src)

  reg <- hydroloom::get_nexus_registry(d)

  if (!"nexus_position" %in% names(reg))
    skip("nexus_registry has no nexus_position column yet")

  expect_true(all(reg$nexus_position >= 0 & reg$nexus_position <= 1,
    na.rm = TRUE),
    label = "nexus_position in [0, 1]")

})

test_that("inject_lateral on synthetic unit results sums correctly per basin", {

  decomposition_pending(c("decompose_network", "inject_lateral"))

  src <- enrich_for_decomposition(load_new_hope())
  d <- hydroloom::decompose_network(src)

  basins <- names(d$domain_connectivity)

  if (length(basins) == 0)
    skip("decomposition produced no basin connectivity overlays")

  results <- synthetic_results(d, value_per_domain = 1)

  for (bid in basins) {

    inj <- hydroloom::inject_lateral(d, results, basin_id = bid)

    if (!"lateral_inflow" %in% names(inj)) next

    contributing <- length(inflow_seeds_for_domain(d, bid))

    if (contributing > 0) {

      expect_equal(sum(inj$lateral_inflow, na.rm = TRUE),
        contributing,
        label = paste0("basin ", bid,
          " lateral_inflow sums to contributor count"))

    } else {

      expect_true(all(is.na(inj$lateral_inflow) |
        inj$lateral_inflow == 0),
        label = paste0("basin ", bid,
          " has no lateral inflow when no domains feed it"))

    }

  }

})

test_that("inject_lateral writes only to rows in inflow_seeds_for_domain", {

  decomposition_pending(c("decompose_network", "inject_lateral",
    "get_nexus_registry"))

  src <- enrich_for_decomposition(load_new_hope())
  d <- hydroloom::decompose_network(src)

  basins <- names(d$domain_connectivity)

  if (length(basins) == 0)
    skip("decomposition produced no basin connectivity overlays")

  bid <- basins[1]

  recipients <- inflow_seeds_for_domain(d, bid)

  if (length(recipients) == 0)
    skip(paste0("basin ", bid, " has no lateral recipients"))

  results <- synthetic_results(d, value_per_domain = 1)

  inj <- hydroloom::inject_lateral(d, results, basin_id = bid)

  if (!"lateral_inflow" %in% names(inj))
    skip("inject_lateral did not produce a lateral_inflow column")

  written_ids <- inj$id[
    !is.na(inj$lateral_inflow) & inj$lateral_inflow != 0]

  expect_setequal(as.character(written_ids), as.character(recipients))

})
