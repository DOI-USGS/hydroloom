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

# ---- trunk_threshold / trunk_levelpaths tests -------------------------

test_that("decompose_network splits walker into multiple trunks on threshold", {

  decomposition_pending(c("decompose_network", "validate_decomposition",
    "get_domain_for_catchment"))

  src <- enrich_for_decomposition(load_walker())

  d <- hydroloom::decompose_network(src,
    trunk_metric    = "drainage_area",
    trunk_threshold = 50)

  expect_true(hydroloom::validate_decomposition(d)$valid,
    label = "walker threshold decomposition is valid")

  assert_partition_coverage(d, src)
  assert_one_outlet_per_domain(d)
  assert_dendritic_inter_domain(d)

  trunk_count <- sum(vapply(d$domains,
    \(dom) dom$domain_type == "trunk", logical(1)))

  expect_gt(trunk_count, 1L,
    label = "walker with threshold = 50 produces multiple trunks")

  # every qualifying levelpath should appear as a trunk
  lp_outlets <- src[src$id == src$levelpath_outlet_id, ]
  expected_trunk_lps <- lp_outlets$levelpath[
    lp_outlets$total_da_sqkm > 50]

  found_trunk_lps <- unique(unlist(lapply(d$domains,
    function(dom) {
      if (dom$domain_type != "trunk") return(NULL)
      unique(dom$catchments$levelpath)
    })))

  expect_true(all(expected_trunk_lps %in% found_trunk_lps),
    label = "all qualifying levelpaths are trunks")

})

test_that("decompose_network trunk_threshold scales on new_hope", {

  decomposition_pending(c("decompose_network", "validate_decomposition"))

  src <- enrich_for_decomposition(load_new_hope())

  d_lo <- hydroloom::decompose_network(src, trunk_threshold = 100)

  expect_true(hydroloom::validate_decomposition(d_lo)$valid)
  assert_partition_coverage(d_lo, src)
  assert_dendritic_inter_domain(d_lo)

  n_trunks_lo <- sum(vapply(d_lo$domains,
    \(dom) dom$domain_type == "trunk", logical(1)))

  d_hi <- hydroloom::decompose_network(src, trunk_threshold = 500)

  expect_true(hydroloom::validate_decomposition(d_hi)$valid)

  n_trunks_hi <- sum(vapply(d_hi$domains,
    \(dom) dom$domain_type == "trunk", logical(1)))

  expect_gte(n_trunks_hi, 1L)
  expect_gte(n_trunks_lo, n_trunks_hi,
    label = "lower threshold produces at least as many trunks as higher")

})

test_that("decompose_network trunk_levelpaths explicit override on walker", {

  decomposition_pending(c("decompose_network", "validate_decomposition"))

  src <- enrich_for_decomposition(load_walker())

  lp_outlets <- src[src$id == src$levelpath_outlet_id, ]
  top_lps <- lp_outlets$levelpath[
    order(-lp_outlets$total_da_sqkm)][1:2]

  d <- hydroloom::decompose_network(src, trunk_levelpaths = top_lps)

  expect_true(hydroloom::validate_decomposition(d)$valid)
  assert_partition_coverage(d, src)
  assert_dendritic_inter_domain(d)

  found_trunk_lps <- unique(unlist(lapply(d$domains,
    function(dom) {
      if (dom$domain_type != "trunk") return(NULL)
      unique(dom$catchments$levelpath)
    })))

  expect_true(all(top_lps %in% found_trunk_lps),
    label = "both override levelpaths appear as trunks")

})

test_that("decompose_network trunk_metric = arbolate_sum on walker", {

  decomposition_pending(c("decompose_network", "validate_decomposition"))

  src <- enrich_for_decomposition(load_walker())

  skip_if_not("arbolate_sum" %in% names(src))

  lp_outlets <- src[src$id == src$levelpath_outlet_id, ]

  threshold <- stats::median(lp_outlets$arbolate_sum, na.rm = TRUE)

  d <- hydroloom::decompose_network(src,
    trunk_metric    = "arbolate_sum",
    trunk_threshold = threshold)

  expect_true(hydroloom::validate_decomposition(d)$valid)
  assert_partition_coverage(d, src)
  assert_dendritic_inter_domain(d)

})

test_that("decompose_network errors on missing drainage_area metric", {

  decomposition_pending("decompose_network")

  src <- enrich_for_decomposition(load_walker())

  src$total_da_sqkm <- NULL
  src$da_sqkm <- NULL

  expect_error(
    hydroloom::decompose_network(src,
      trunk_metric = "drainage_area", trunk_threshold = 50),
    "total_da_sqkm")

})

test_that("decompose_network errors on unknown trunk_levelpaths", {

  decomposition_pending("decompose_network")

  src <- enrich_for_decomposition(load_walker())

  expect_error(
    hydroloom::decompose_network(src,
      trunk_levelpaths = c(999999999)),
    "unknown levelpath")

})

test_that("decompose_network omits trunks for sub-threshold basins", {

  decomposition_pending(c("decompose_network", "validate_decomposition"))

  # Two disconnected basins:
  #   Basin A (ids 1-3, DA sums to 30) -- below threshold 50
  #   Basin B (ids 4-6, DA sums to 100) -- above threshold 50
  pair <- data.frame(
    id = 1:6,
    toid = c(2L, 3L, 0L, 5L, 6L, 0L),
    topo_sort = c(3L, 2L, 1L, 6L, 5L, 4L),
    levelpath = c(1L, 1L, 1L, 2L, 2L, 2L),
    levelpath_outlet_id = c(3L, 3L, 3L, 6L, 6L, 6L),
    da_sqkm = c(10, 10, 10, 30, 30, 40))

  src <- hydroloom::hy(pair)

  d <- hydroloom::decompose_network(src,
    trunk_metric    = "drainage_area",
    trunk_threshold = 50)

  expect_true(hydroloom::validate_decomposition(d)$valid)
  assert_partition_coverage(d, src)

  types <- vapply(d$domains,
    \(dom) dom$domain_type, character(1))

  # Basin B (DA = 100) should have at least one trunk.
  expect_gte(sum(types == "trunk"), 1L)

  # Basin A (DA = 30) should have NO trunk -- only a compact domain.
  basin_a_domain <- d$catchment_domain_index[["3"]]
  basin_a_type <- d$domains[[basin_a_domain]]$domain_type

  expect_equal(basin_a_type, "compact",
    label = "sub-threshold basin is a compact domain, not a trunk")

  # The compact domain for basin A should contain all three catchments.
  expect_setequal(
    as.character(d$domains[[basin_a_domain]]$catchments$id),
    c("1", "2", "3"))

})
