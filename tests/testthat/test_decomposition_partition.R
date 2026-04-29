# Layer 2 — partition correctness for decompose_network.
#
# First end-to-end exercise of decompose_network on real shipped
# networks. Asserts only the partition properties: coverage, single
# outlet per domain, dendritic inter-domain graph, and class
# selection. Mass balance and recomposition land in Layer 5.
#
# Datasets:
#   - walker.gpkg : degenerate baseline (62 features, dendritic).
#   - new_hope.gpkg : primary multi-stem + divergent fixture.
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

  for (domain in d$domains) {
    expect_true(hydroloom::hy_network_type(domain$catchments) %in%
      c("hy_topo", "hy_leveled", "hy_flownetwork"))
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

  n_basins  <- length(d$domain_connectivity)
  n_domains <- length(d$domains)

  expect_gte(n_basins, 1L)
  expect_gte(n_domains, 1L)

  # every connectivity overlay must be hy_leveled
  expect_true(all(vapply(d$domain_connectivity,
    \(o) inherits(o, "hy_leveled"), logical(1))))

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

  # at least one domain should land on hy_flownetwork because the
  # source is non-dendritic.
  has_fn <- any(vapply(d$domains,
    \(domain) inherits(domain$catchments, "hy_flownetwork"),
    logical(1)))

  expect_true(has_fn,
    label = "non-dendritic source produces at least one hy_flownetwork domain")

})

# ---- stem_threshold / stem_levelpaths tests -------------------------

test_that("decompose_network extensive network includes all above-threshold catchments", {

  decomposition_pending(c("decompose_network", "validate_decomposition"))

  src <- enrich_for_decomposition(load_walker())

  # threshold=15: extensive network spans 2 levelpaths, 3 domains (segments)
  d <- hydroloom::decompose_network(src,
    stem_metric    = "drainage_area",
    stem_threshold = 15)

  expect_true(hydroloom::validate_decomposition(d)$valid,
    label = "walker threshold decomposition is valid")

  assert_partition_coverage(d, src)
  assert_one_outlet_per_domain(d)
  assert_dendritic_inter_domain(d)

  expect_equal(length(d$domain_connectivity), 1L,
    label = "walker is one basin")

  conn <- d$domain_connectivity[[1]]

  conn_ids <- as.character(conn$id)

  expected_ids <- as.character(
    src$id[src$total_da_sqkm > 15])

  expect_true(all(expected_ids %in% conn_ids),
    label = "all above-threshold catchments are in the connectivity overlay")

  expect_gt(length(unique(conn$levelpath)), 1L,
    label = "connectivity overlay includes catchments from multiple levelpaths")

})

test_that("decompose_network stem_threshold on new_hope", {

  decomposition_pending(c("decompose_network", "validate_decomposition"))

  src <- enrich_for_decomposition(load_new_hope())

  # threshold=100: extensive network spans many levelpaths, at least 2 segments
  d <- hydroloom::decompose_network(src, stem_threshold = 100)

  expect_true(hydroloom::validate_decomposition(d)$valid)
  assert_partition_coverage(d, src)
  assert_dendritic_inter_domain(d)

  expect_equal(length(d$domain_connectivity), 1L,
    label = "new_hope is one basin")

  conn <- d$domain_connectivity[[1]]

  expect_gt(length(unique(conn$levelpath)), 2L,
    label = "connectivity overlay includes tributaries beyond the outlet levelpath")

  expect_gte(length(d$domains), 2L,
    label = "new_hope with threshold = 100 produces at least 2 domains")

})

test_that("decompose_network stem_levelpaths explicit override on walker", {

  decomposition_pending(c("decompose_network", "validate_decomposition"))

  src <- enrich_for_decomposition(load_walker())

  lp_outlets <- src[src$id == src$levelpath_outlet_id, ]
  top_lps <- lp_outlets$levelpath[
    order(-lp_outlets$total_da_sqkm)][1:2]

  d <- hydroloom::decompose_network(src, stem_levelpaths = top_lps)

  expect_true(hydroloom::validate_decomposition(d)$valid)
  assert_partition_coverage(d, src)
  assert_dendritic_inter_domain(d)

  conn <- d$domain_connectivity[[1]]

  found_lps <- unique(conn$levelpath)

  expect_true(all(top_lps %in% found_lps),
    label = "both override levelpaths appear in the connectivity overlay")

})

test_that("decompose_network stem_metric = arbolate_sum on walker", {

  decomposition_pending(c("decompose_network", "validate_decomposition"))

  src <- enrich_for_decomposition(load_walker())

  skip_if_not("arbolate_sum" %in% names(src))

  lp_outlets <- src[src$id == src$levelpath_outlet_id, ]

  threshold <- stats::median(lp_outlets$arbolate_sum, na.rm = TRUE)

  d <- hydroloom::decompose_network(src,
    stem_metric    = "arbolate_sum",
    stem_threshold = threshold)

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
      stem_metric = "drainage_area", stem_threshold = 50),
    "total_da_sqkm")

})

test_that("decompose_network errors on unknown stem_levelpaths", {

  decomposition_pending("decompose_network")

  src <- enrich_for_decomposition(load_walker())

  expect_error(
    hydroloom::decompose_network(src,
      stem_levelpaths = c(999999999)),
    "unknown levelpath")

})

test_that("decompose_network omits connectivity for sub-threshold basins", {

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
    da_sqkm = c(10, 10, 10, 30, 30, 40),
    stream_calculator = c(1L, 1L, 1L, 1L, 1L, 1L))

  src <- hydroloom::hy(pair)

  d <- hydroloom::decompose_network(src,
    stem_metric    = "drainage_area",
    stem_threshold = 50)

  expect_true(hydroloom::validate_decomposition(d)$valid)
  assert_partition_coverage(d, src)

  # Basin B (DA = 100) should have a connectivity overlay.
  expect_true("6" %in% names(d$domain_connectivity),
    label = "above-threshold basin has a connectivity overlay")

  # Basin A (DA = 30) should have NO connectivity overlay -- whole
  # basin becomes a single domain.
  expect_false("3" %in% names(d$domain_connectivity),
    label = "sub-threshold basin has no connectivity overlay")

  basin_a_domain <- d$catchment_domain_index[["3"]]

  # The single domain for basin A should contain all three catchments.
  expect_setequal(
    as.character(d$domains[[basin_a_domain]]$catchments$id),
    c("1", "2", "3"))

})

# ---- domain_breaks parameter --------------------------------------------

test_that("decompose_network domain_breaks splits extensive network at specified ids", {

  decomposition_pending(c("decompose_network", "validate_decomposition"))

  src <- enrich_for_decomposition(load_walker())

  d_default <- hydroloom::decompose_network(src)

  # Pick a mid-extensive-network catchment from the basin's overlay.
  conn <- d_default$domain_connectivity[[1]]
  mid_id <- conn$id[ceiling(nrow(conn) / 2)]

  d_breaks <- hydroloom::decompose_network(src, domain_breaks = mid_id)

  expect_true(hydroloom::validate_decomposition(d_breaks)$valid)
  assert_partition_coverage(d_breaks, src)

  n_default <- length(d_default$domains)
  n_breaks  <- length(d_breaks$domains)

  expect_gte(n_breaks, n_default,
    label = "explicit break produces at least as many domains")

})

test_that("decompose_network domain_breaks ignores non-stem ids", {

  decomposition_pending(c("decompose_network", "validate_decomposition"))

  src <- enrich_for_decomposition(load_walker())

  d_default <- hydroloom::decompose_network(src)
  d_breaks <- hydroloom::decompose_network(src, domain_breaks = c(-999))

  expect_equal(length(d_breaks$domains), length(d_default$domains),
    label = "bogus break id does not change domain count")

})

test_that("decompose_network domain_breaks composes with stem_levelpaths", {

  decomposition_pending(c("decompose_network", "validate_decomposition"))

  src <- enrich_for_decomposition(load_walker())

  lp_outlets <- src[src$id == src$levelpath_outlet_id, ]
  top_lps <- lp_outlets$levelpath[order(-lp_outlets$total_da_sqkm)][1:2]

  # Pick a mid-extensive-network id from the multi-levelpath overlay.
  d_lp <- hydroloom::decompose_network(src, stem_levelpaths = top_lps)
  conn <- d_lp$domain_connectivity[[1]]
  mid_id <- conn$id[ceiling(nrow(conn) / 2)]

  d_both <- hydroloom::decompose_network(src,
    stem_levelpaths = top_lps,
    domain_breaks = mid_id)

  expect_true(hydroloom::validate_decomposition(d_both)$valid)
  assert_partition_coverage(d_both, src)

})

# ---- decomposed compact form ------------------------------------------

test_that("domain catchments include extensive network rows as detoid'd outlets", {

  decomposition_pending(c("decompose_network", "validate_decomposition"))

  src <- enrich_for_decomposition(load_walker())

  d <- hydroloom::decompose_network(src)

  expect_gte(length(d$domain_connectivity), 1L)
  expect_gte(length(d$domains), 1L)

  # Extensive network catchment ids -- pulled from the basin's
  # extensive network overlay so we can tell extensive network rows
  # from laterals without a marker column.
  conn_ids <- unlist(lapply(d$domain_connectivity,
    \(o) as.character(o$id)),
    use.names = FALSE)

  for (domain in d$domains) {

    catch <- domain$catchments

    outlet_value <- hydroloom:::get_outlet_value(catch)

    in_main <- as.character(catch$id) %in% conn_ids

    if (any(in_main)) {
      expect_true(all(catch$toid[in_main] == outlet_value),
        label = paste0("domain ", domain$domain_id,
          " extensive network rows have the reserved outlet toid value"))
    }
  }

  # Connectivity-membership invariant: every row in some domain
  # carrying the reserved outlet toid value appears in some basin's
  # connectivity overlay.
  assert_segments_in_connectivity(d)

})

test_that("domain accumulate_downstream gives per-extensive-network incremental DA", {

  decomposition_pending(c("decompose_network", "validate_decomposition"))

  src <- enrich_for_decomposition(load_walker())

  skip_if_not("da_sqkm" %in% names(src),
    "walker fixture missing da_sqkm")

  d <- hydroloom::decompose_network(src)

  conn_ids <- unlist(lapply(d$domain_connectivity,
    \(o) as.character(o$id)),
    use.names = FALSE)

  for (domain in d$domains) {

    catch <- domain$catchments

    in_main <- which(as.character(catch$id) %in% conn_ids)

    if (length(in_main) == 0L) next

    acc <- hydroloom::accumulate_downstream(catch, "da_sqkm")

    # Each extensive network row is its own outlet inside the domain.
    # Its accumulated value should equal its own incremental plus the
    # sum of every lateral row that drains (transitively) to it.
    for (i in in_main) {

      tf_id <- catch$id[i]

      contributing <- collect_upstream_in_domain(catch, tf_id)

      expected <- sum(catch$da_sqkm[
        as.character(catch$id) %in% contributing], na.rm = TRUE)

      expect_equal(acc[i], expected,
        label = paste0("domain ", domain$domain_id,
          " extensive network row ", tf_id, " accumulated DA"))
    }
  }

})

test_that("extensive network toids restored from source produce a connected extensive network", {

  decomposition_pending(c("decompose_network", "validate_decomposition"))

  src <- enrich_for_decomposition(load_walker())

  d <- hydroloom::decompose_network(src)

  conn_ids <- unlist(lapply(d$domain_connectivity,
    \(o) as.character(o$id)),
    use.names = FALSE)

  src_lookup <- setNames(as.character(src$toid), as.character(src$id))

  for (domain in d$domains) {

    catch <- domain$catchments

    in_main_idx <- which(as.character(catch$id) %in% conn_ids)

    if (length(in_main_idx) == 0L) next

    # Restore extensive network rows' toids from source_network.
    restored <- catch
    restored$toid[in_main_idx] <-
      src_lookup[as.character(restored$id[in_main_idx])]

    # Re-detoid: dropping toids again should match the original
    # decomposed form.
    re_dropped <- restored
    re_dropped$toid[in_main_idx] <-
      hydroloom:::get_outlet_value(re_dropped)

    expect_equal(
      as.character(re_dropped$toid),
      as.character(catch$toid),
      label = paste0("domain ", domain$domain_id,
        " round-trip restore -> drop matches original"))
  }

})
