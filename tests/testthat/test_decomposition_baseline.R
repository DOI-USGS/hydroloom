# Layer 0 — sample-data property baselines for the decomposition tests.
#
# This layer pins the structural facts that later decomposition test
# layers depend on. It uses only existing hydroloom functions and so
# should be green from day one — no decomposition API is exercised.
#
# Each block exercises one shipped or staged dataset and asserts:
#   - row count
#   - levelpath / outlet identity
#   - hy_network_type / dendritic-ness
#   - that accumulate_downstream on the renamed area column reproduces
#     the dataset's TotDASqKM at the outlet (the same arithmetic Layer 5
#     reproduces from a decomposition).
#
# The exact numeric values are pinned so any change to the shipped data
# or to add_toids/accumulate_downstream surfaces here first.

test_that("walker.gpkg has expected structural baseline", {

  w <- load_walker()

  expect_equal(nrow(w), 62)
  expect_equal(length(unique(w$LevelPathI)), 26)

  outlet_idx <- which.max(w$TotDASqKM)
  expect_equal(w$COMID[outlet_idx], 5329303)
  expect_equal(max(w$TotDASqKM), 193.9473, tolerance = 1e-4)

  w_hy <- hydroloom::add_toids(hydroloom::hy(w), return_dendritic = TRUE)

  expect_true(is_dendritic(w_hy))
  expect_equal(hydroloom::hy_network_type(w_hy), "hy_topo")

  acc <- hydroloom::accumulate_downstream(w_hy, "da_sqkm")

  expect_equal(max(acc), max(w$TotDASqKM), tolerance = 1e-4,
    label = "walker accumulated da_sqkm matches TotDASqKM at outlet")

})

test_that("new_hope.gpkg has expected structural baseline", {

  n <- load_new_hope()

  expect_equal(nrow(n), 746)
  expect_equal(length(unique(n$LevelPathI)), 228)

  outlet_idx <- which.max(n$TotDASqKM)
  expect_equal(n$COMID[outlet_idx], 8897784)
  expect_equal(max(n$TotDASqKM), 595.3383, tolerance = 1e-4)

  # raw new_hope has divergence == 2 so hy() lands on hy_node and is_dendritic
  # is FALSE.
  n_node <- hydroloom::hy(n)

  expect_setequal(unique(n$Divergence), c(0, 1, 2))
  expect_false(is_dendritic(n_node))
  expect_equal(hydroloom::hy_network_type(n_node), "hy_node")

  n_hy <- hydroloom::add_toids(n_node, return_dendritic = TRUE)

  # add_toids(return_dendritic = TRUE) collapses to a unique-id hy_topo
  # (structurally dendritic). The retained `divergence` column still
  # carries 2s, so is_dendritic() — which inspects that column — reports
  # FALSE. The structural class check is what later layers care about.
  expect_equal(hydroloom::hy_network_type(n_hy), "hy_topo")
  expect_false(any(duplicated(n_hy$id)))

  acc <- hydroloom::accumulate_downstream(n_hy, "da_sqkm")

  expect_equal(max(acc), max(n$TotDASqKM), tolerance = 1e-4,
    label = "new_hope accumulated da_sqkm matches TotDASqKM at outlet")

})

test_that("network.rds is non-dendritic and routes to hy_flownetwork", {

  net <- load_test_rds("network")

  expect_equal(nrow(net), 707)
  expect_false(inherits(net, "sf"))

  net_hy <- hydroloom::hy(net)

  expect_false(is_dendritic(net_hy))
  expect_equal(hydroloom::hy_network_type(net_hy), "hy_node")

  net_fn <- hydroloom::add_toids(net_hy, return_dendritic = FALSE) |>
    suppressWarnings()

  expect_equal(hydroloom::hy_network_type(net_fn), "hy_flownetwork")

})

test_that("coastal.rds is dendritic hy_topo with expected shape", {

  co <- load_test_rds("coastal")

  expect_equal(nrow(co), 548)
  expect_true(all(c("comid", "tocomid", "totdasqkm") %in% names(co)))

  co_hy <- hydroloom::hy(co)

  expect_true(is_dendritic(co_hy))
  expect_equal(hydroloom::hy_network_type(co_hy), "hy_topo")

})

test_that("hr.rds loads as a node-based hydroloom object at scale", {

  hr <- load_test_rds("hr")

  expect_equal(nrow(hr), 2691)

  hr_hy <- suppressMessages(hydroloom::hy(hr))

  expect_equal(hydroloom::hy_network_type(hr_hy), "hy_node")

})

test_that("loop.rds and loop2.rds carry expected row counts and id/toid", {

  lo <- load_test_rds("loop")

  expect_equal(nrow(lo), 1425)
  expect_true(all(c("id", "toid") %in% names(lo)))

  l2 <- load_test_rds("loop2")

  expect_equal(nrow(l2), 839)
  expect_true(all(c("id", "toid") %in% names(l2)))

})
