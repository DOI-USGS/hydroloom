# Layer 3 — domain graph operations.
#
# With a verified decomposition in hand, exercises domain_topo_sort(),
# navigate_domain_graph(), and accumulate_domains() on the inter-domain
# graph alone — no within-domain math. Wiring checks via unit
# accumulation.
#
# Five of the seven blocks use new_hope as the source, so the enriched
# network and its decomposition are lifted to file scope and shared.
# walker and network.rds get their own enriched/decomposed pair.

# ---- shared new_hope fixture (file scope) ------------------------------

new_hope_src <- enrich_for_decomposition(load_new_hope())

new_hope_d <- if (exists("decompose_network",
  envir = asNamespace("hydroloom"), inherits = FALSE)) {
  tryCatch(
    hydroloom::decompose_network(new_hope_src),
    error = function(e) NULL)
} else {
  NULL
}

test_that("domain_topo_sort agrees with sort_network on the domain graph", {

  decomposition_pending(c("decompose_network", "domain_topo_sort",
    "get_domain_graph"))

  ts <- hydroloom::domain_topo_sort(new_hope_d)

  g <- hydroloom::get_domain_graph(new_hope_d, relations = "flow")

  ref <- hydroloom::sort_network(g)$id

  expect_equal(as.character(ts), as.character(ref),
    label = "domain_topo_sort matches sort_network on domain graph")

})

test_that("every flow edge is honored by the topological order", {

  decomposition_pending(c("decompose_network", "domain_topo_sort",
    "get_domain_graph"))

  ts <- as.character(hydroloom::domain_topo_sort(new_hope_d))

  g <- hydroloom::get_domain_graph(new_hope_d, relations = "flow")

  pos <- setNames(seq_along(ts), ts)

  for (i in seq_len(nrow(g))) {

    u <- as.character(g$id[i])
    v <- as.character(g$toid[i])

    if (is.na(v) || v == "" || v == "0" || !v %in% names(pos)) next

    expect_lt(pos[[u]], pos[[v]],
      label = paste0("edge ", u, " -> ", v, " honored by topo order"))

  }

})

test_that("navigate_domain_graph down from a headwater reaches the global outlet", {

  decomposition_pending(c("decompose_network", "navigate_domain_graph",
    "get_domain_graph"))

  g <- hydroloom::get_domain_graph(new_hope_d, relations = "flow")

  ids <- as.character(g$id)
  toids <- as.character(g$toid)

  # headwater = a domain id that does not appear as anyone's toid
  headwaters <- setdiff(ids, toids)

  if (length(headwaters) == 0)
    skip("no headwater domain in flow graph")

  start <- headwaters[1]

  outlet <- setdiff(toids, ids)
  outlet <- outlet[!is.na(outlet) & outlet != "" & outlet != "0"]
  global_outlet <- if (length(outlet) > 0) outlet[1] else
    setdiff(ids, ids[ids %in% toids])

  path <- as.character(hydroloom::navigate_domain_graph(new_hope_d, start, "down"))

  expect_true(global_outlet %in% path || tail(path, 1) == global_outlet,
    label = paste0("downstream walk from ", start, " reaches outlet"))

})

test_that("navigate_domain_graph up superset of upmain", {

  decomposition_pending(c("decompose_network", "navigate_domain_graph"))

  start <- names(new_hope_d$domains)[1]

  up_all <- as.character(hydroloom::navigate_domain_graph(new_hope_d, start, "up"))
  up_main <- as.character(hydroloom::navigate_domain_graph(new_hope_d, start, "upmain"))

  expect_true(all(up_main %in% up_all),
    label = "upmain is a subset of up")

})

test_that("unit accumulate_domains at the outlet equals domain count", {

  decomposition_pending(c("decompose_network", "accumulate_domains"))

  total <- unit_accumulation_check(new_hope_d, include_contained = FALSE)

  # excluding contained-only domains, the unit accumulation at the
  # global outlet should equal the count of flow-connected domains
  flow_domains <- vapply(new_hope_d$domains,
    \(dom) is.na(dom$containing_domain_id) ||
      dom$containing_domain_id == "" ||
      is.null(dom$containing_domain_id),
    logical(1))

  expect_equal(total, sum(flow_domains),
    label = "unit accumulation = flow-connected domain count")

})

test_that("walker domain graph operations are well-defined on the degenerate case", {

  decomposition_pending(c("decompose_network", "domain_topo_sort"))

  src <- enrich_for_decomposition(load_walker())
  d <- hydroloom::decompose_network(src)

  ts <- hydroloom::domain_topo_sort(d)

  expect_length(as.character(ts), length(d$domains))
  expect_setequal(as.character(ts), names(d$domains))

})

test_that("non-dendritic network.rds produces a navigable domain graph", {

  decomposition_pending(c("decompose_network", "domain_topo_sort",
    "get_domain_graph"))

  # network.rds carries no GNIS or arbolate columns, so the canonical
  # enrichment chain will skip this test via the membership check.
  src <- enrich_for_decomposition(load_test_rds("network"))

  d <- hydroloom::decompose_network(src)

  g <- hydroloom::get_domain_graph(d, relations = "flow")

  # If the resulting domain graph is itself non-dendritic, it should be
  # carried as hy_flownetwork — but in any case sort_network/domain_topo_sort
  # must succeed without erroring.
  expect_true(hydroloom::hy_network_type(g) %in%
    c("hy_topo", "hy_leveled", "hy_flownetwork"))

  expect_no_error(hydroloom::domain_topo_sort(d))

})
