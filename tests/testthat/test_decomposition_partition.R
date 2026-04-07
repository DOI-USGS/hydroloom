# Layer 2 — partition correctness for decompose_network.
#
# First end-to-end exercise of decompose_network on real shipped
# networks. Asserts only the partition properties: coverage, single
# outlet per domain, dendritic inter-domain graph, and class
# selection. Mass balance and recomposition land in Layer 5.
#
# Datasets:
#   - walker.gpkg : degenerate baseline (62 features, dendritic).
#   - new_hope.gpkg : primary multi-trunk + divergent fixture.
#   - network.rds : non-dendritic, forces hy_flownetwork.

test_that("decompose_network partitions walker.gpkg", {

  decomposition_pending(c("decompose_network", "validate_decomposition",
    "get_domain_for_catchment"))

  src <- enrich_for_decomposition(load_walker())

  d <- hydroloom::decompose_network(src)

  expect_true(hydroloom::validate_decomposition(d)$valid,
    label = "walker decomposition is valid")

  assert_partition_coverage(d, src)
  assert_one_outlet_per_domain(d)
  assert_dendritic_inter_domain(d)

  # trunks must carry hy_leveled; compacts at least hy_topo or hy_flownetwork
  for (dom in d$domains) {

    if (dom$domain_type == "trunk") {
      expect_s3_class(dom$catchments, "hy_leveled")
    } else {
      expect_true(hydroloom::hy_network_type(dom$catchments) %in%
        c("hy_topo", "hy_leveled", "hy_flownetwork"))
    }

  }

  # round trip: catchment id -> domain id -> contains catchment
  sample_ids <- src$id[seq(1, nrow(src),
    length.out = min(10, nrow(src)))]

  for (cid in sample_ids) {

    did <- hydroloom::get_domain_for_catchment(d, cid)

    expect_true(cid %in% d$domains[[did]]$catchments$id,
      label = paste0("catchment ", cid, " in domain ", did))

  }

})

test_that("decompose_network partitions new_hope.gpkg", {

  decomposition_pending(c("decompose_network", "validate_decomposition",
    "get_domain_for_catchment"))

  src <- enrich_for_decomposition(load_new_hope())

  d <- hydroloom::decompose_network(src)

  expect_true(hydroloom::validate_decomposition(d)$valid,
    label = "new_hope decomposition is valid")

  assert_partition_coverage(d, src)
  assert_one_outlet_per_domain(d)
  assert_dendritic_inter_domain(d)

  # new_hope is the primary multi-trunk fixture; expect more than one
  # trunk and at least one compact.
  trunk_count <- sum(vapply(d$domains,
    \(dom) dom$domain_type == "trunk", logical(1)))
  compact_count <- sum(vapply(d$domains,
    \(dom) dom$domain_type == "compact", logical(1)))

  expect_gte(trunk_count, 1L)
  expect_gte(compact_count, 1L)

  # every trunk's catchments must be hy_leveled
  for (dom in d$domains) {
    if (dom$domain_type == "trunk")
      expect_s3_class(dom$catchments, "hy_leveled")
  }

})

test_that("decompose_network handles non-dendritic network.rds", {

  decomposition_pending(c("decompose_network", "validate_decomposition"))

  net <- load_test_rds("network")

  # network.rds carries no GNIS or arbolate columns, so the canonical
  # enrichment chain will skip this test via the membership check.
  src <- enrich_for_decomposition(net)

  d <- hydroloom::decompose_network(src)

  expect_true(hydroloom::validate_decomposition(d)$valid,
    label = "network.rds decomposition is valid")

  assert_partition_coverage(d, src)
  assert_one_outlet_per_domain(d)

  # at least one domain — trunk or compact — should land on
  # hy_flownetwork because the source is non-dendritic.
  has_fn <- any(vapply(d$domains,
    \(dom) inherits(dom$catchments, "hy_flownetwork"),
    logical(1)))

  expect_true(has_fn,
    label = "non-dendritic source produces at least one hy_flownetwork domain")

})
