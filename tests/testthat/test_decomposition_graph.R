# Layer 3 -- domain graph operations.
#
# Validates the `existing_fn(get_domain_graph(d), ...)` pattern: the
# inter-domain graph derived from `nexus_registry` plays cleanly with
# the existing hydroloom toolchain (sort_network, check_hy_graph). No
# bespoke domain-graph wrappers; the recompose-scope decomposition
# exposes the graph and lets callers reach for the right tool.
#
# Five of the seven blocks use new_hope as the source, so the enriched
# network and its decomposition are lifted to file scope and shared.

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

test_that("get_domain_graph(d, 'flow') is acyclic and sortable", {

  decomposition_pending(c("decompose_network", "get_domain_graph"))

  g <- hydroloom::get_domain_graph(new_hope_d, relations = "flow")

  if (nrow(g) == 0L) skip("decomposition has no inter-domain edges")

  expect_true(isTRUE(hydroloom::check_hy_graph(g)),
    label = "inter-domain flow graph is acyclic")

  expect_no_error(hydroloom::sort_network(g))

})

test_that("every flow edge is honored by the topological order", {

  decomposition_pending(c("decompose_network", "get_domain_graph"))

  g <- hydroloom::get_domain_graph(new_hope_d, relations = "flow")

  if (nrow(g) == 0L) skip("decomposition has no inter-domain edges")

  ts <- as.character(hydroloom::sort_network(g)$id)

  pos <- setNames(seq_along(ts), ts)

  for (i in seq_len(nrow(g))) {

    u <- as.character(g$id[i])
    v <- as.character(g$toid[i])

    if (is.na(v) || v == "" || v == "0" || !v %in% names(pos)) next

    expect_lt(pos[[u]], pos[[v]],
      label = paste0("edge ", u, " -> ", v, " honored by topo order"))

  }

})

test_that("walker domain graph is empty on the degenerate case", {

  decomposition_pending(c("decompose_network", "get_domain_graph"))

  src <- enrich_for_decomposition(load_walker())
  d <- hydroloom::decompose_network(src)

  g <- hydroloom::get_domain_graph(d, relations = "flow")

  # Walker decomposes to a single domain with no inter-domain edges.
  # sort_network does not accept zero-row inputs; the empty-graph
  # contract is just structural.
  expect_equal(nrow(g), 0L)
  expect_true(all(c("id", "toid") %in% names(g)))

})

test_that("non-dendritic network.rds produces a navigable domain graph", {

  decomposition_pending(c("decompose_network", "get_domain_graph"))

  # network.rds carries no GNIS or arbolate columns, so the canonical
  # enrichment chain will skip this test via the membership check.
  src <- enrich_for_decomposition(load_test_rds("network"))

  d <- hydroloom::decompose_network(src)

  g <- hydroloom::get_domain_graph(d, relations = "flow")

  # If the resulting domain graph is itself non-dendritic, it should be
  # carried as hy_flownetwork; in any case sort_network must succeed
  # without erroring.
  expect_true(hydroloom::hy_network_type(g) %in%
    c("hy_topo", "hy_leveled", "hy_flownetwork"))

  expect_no_error(hydroloom::sort_network(g))

})
