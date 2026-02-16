test_that("get_bridge_flowlines with sf object", {
  g <- sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom"))

  x <- hy(g)
  x <- add_toids(x)

  result <- get_bridge_flowlines(x)

  # Should return numeric ids matching the original COMID type
  expect_true(length(result) > 0)
  expect_true(all(result %in% x$id))

  # flowlines within a diversion loop are NOT bridges
  expect_false(all(c(8893532, 8893534, 8893222) %in% result))

  net <- navigate_network_dfs(x, 8893236, "up")

  x <- x[x$id %in% unlist(net), ]

  result <- get_bridge_flowlines(x)

  # flowlines within diversions should not be bridges
  expect_false(all(c(8893222, 8893228) %in% result))
})

test_that("get_bridge_flowlines divergence near outlet", {
  # a trib joins a diversion near the outlet
  #
  # 3 ---------+
  #            v
  # 1 -> 2 -> 4 -> 5 -> 0  # nolint
  # |              ^
  # v              |
  # 6 ----> 7 -----+
  #
  x <- data.frame(
    id =   c(1, 2, 3, 4, 1, 6, 7, 5),
    toid = c(2, 4, 4, 5, 6, 7, 5, 0)
  )

  result <- get_bridge_flowlines(x)

  # pendant flowlines are bridges, loop flowlines {2,4,6,7} are not
  expect_setequal(result, c(1, 3, 5))
})

test_that("get_bridge_flowlines dendritic tree", {
  # Two branches joining:
  # 1 -> 2 -> 3 -> 4 -> 5  # nolint
  #               ^
  #               |
  # 6 -> 7 -> 8 -> 9  # nolint
  x <- data.frame(
    id = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
    toid = c(2, 3, 4, 5, 0, 7, 8, 9, 4)
  )

  result <- get_bridge_flowlines(x)

  # Dendritic tree has no cycles: all flowlines are bridges
  expect_setequal(result, c(1, 2, 3, 4, 5, 6, 7, 8, 9))
})

test_that("get_bridge_flowlines data.frame vs hy", {
  x <- data.frame(
    id = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
    toid = c(2, 3, 4, 5, 0, 7, 8, 9, 4)
  )

  result_df <- get_bridge_flowlines(x)
  result_hy <- get_bridge_flowlines(hy(x))

  expect_equal(result_df, result_hy)
})

test_that("get_bridge_flowlines character ids", {
  # Linear chain: all flowlines are bridges
  x <- data.frame(
    id = c("A", "B", "C", "D", "E"),
    toid = c("B", "C", "D", "E", "")
  )

  result <- get_bridge_flowlines(x)

  expect_setequal(result, c("A", "B", "C", "D", "E"))
  expect_true(is.character(result))
})

test_that("get_bridge_flowlines single flowline", {
  x <- data.frame(
    id = 1,
    toid = 0
  )

  result <- get_bridge_flowlines(x)

  # Single flowline is a bridge
  expect_equal(result, 1)
})

test_that("get_bridge_flowlines two flowlines", {
  x <- data.frame(
    id = c(1, 2),
    toid = c(2, 0)
  )

  result <- get_bridge_flowlines(x)

  # Both flowlines are bridges in a 2-edge path
  expect_setequal(result, c(1, 2))
})

# --- low-level find_bridges tests on raw node adjacency matrices ---

test_that("find_bridges linear 3 nodes", {
  # Node graph: 1 -- 2 -- 3
  # Edges: e1(1-2), e2(2-3). Both are bridges.
  adj_matrix <- matrix(c(
    2L, 1L, 2L,
    0L, 3L, 0L
  ), nrow = 2, ncol = 3, byrow = TRUE)
  lens <- c(1L, 2L, 1L)
  edge_id_matrix <- matrix(c(
    1L, 1L, 2L,
    0L, 2L, 0L
  ), nrow = 2, ncol = 3, byrow = TRUE)

  result <- find_bridges(adj_matrix, lens, edge_id_matrix, 2L)
  expect_setequal(result, c(1L, 2L))
})

test_that("find_bridges triangle no bridges", {
  # Node graph: 1-2 (e1), 2-3 (e2), 1-3 (e3). No bridges.
  #
  # Node 1: neighbors 2(e1), 3(e3)
  # Node 2: neighbors 1(e1), 3(e2)
  # Node 3: neighbors 1(e3), 2(e2)
  adj_matrix <- matrix(c(
    2L, 1L, 1L,
    3L, 3L, 2L
  ), nrow = 2, ncol = 3, byrow = TRUE)
  lens <- c(2L, 2L, 2L)
  edge_id_matrix <- matrix(c(
    1L, 1L, 3L,
    3L, 2L, 2L
  ), nrow = 2, ncol = 3, byrow = TRUE)

  result <- find_bridges(adj_matrix, lens, edge_id_matrix, 3L)
  expect_equal(result, integer(0))
})

test_that("find_bridges two triangles connected by bridge", {
  # Triangle 1: 1-2 (e1), 2-3 (e2), 1-3 (e3)
  # Bridge:     2-4 (e4)
  # Triangle 2: 4-5 (e5), 5-6 (e6), 4-6 (e7)
  # Only e4 is a bridge.
  #
  # Node 1: 2(e1), 3(e3)
  # Node 2: 1(e1), 3(e2), 4(e4)
  # Node 3: 2(e2), 1(e3)
  # Node 4: 2(e4), 5(e5), 6(e7)
  # Node 5: 4(e5), 6(e6)
  # Node 6: 5(e6), 4(e7)
  adj_matrix <- matrix(c(
    2L, 1L, 2L, 2L, 4L, 5L,
    3L, 3L, 1L, 5L, 6L, 4L,
    0L, 4L, 0L, 6L, 0L, 0L
  ), nrow = 3, ncol = 6, byrow = TRUE)
  lens <- c(2L, 3L, 2L, 3L, 2L, 2L)
  edge_id_matrix <- matrix(c(
    1L, 1L, 2L, 4L, 5L, 6L,
    3L, 2L, 3L, 5L, 6L, 7L,
    0L, 4L, 0L, 7L, 0L, 0L
  ), nrow = 3, ncol = 6, byrow = TRUE)

  result <- find_bridges(adj_matrix, lens, edge_id_matrix, 7L)
  expect_equal(result, 4L)
})

test_that("find_bridges star graph all bridges", {
  # Center node 1, leaves 2,3,4
  # Edges: e1(1-2), e2(1-3), e3(1-4). All bridges.
  #
  # Node 1: 2(e1), 3(e2), 4(e3)
  # Node 2: 1(e1)
  # Node 3: 1(e2)
  # Node 4: 1(e3)
  adj_matrix <- matrix(c(
    2L, 1L, 1L, 1L,
    3L, 0L, 0L, 0L,
    4L, 0L, 0L, 0L
  ), nrow = 3, ncol = 4, byrow = TRUE)
  lens <- c(3L, 1L, 1L, 1L)
  edge_id_matrix <- matrix(c(
    1L, 1L, 2L, 3L,
    2L, 0L, 0L, 0L,
    3L, 0L, 0L, 0L
  ), nrow = 3, ncol = 4, byrow = TRUE)

  result <- find_bridges(adj_matrix, lens, edge_id_matrix, 3L)
  expect_setequal(result, c(1L, 2L, 3L))
})

test_that("find_bridges disconnected components", {
  # Component 1: 1--2 (e1)
  # Component 2: 3--4--5 path, edges e2(3-4), e3(4-5)
  # All edges are bridges.
  adj_matrix <- matrix(c(
    2L, 1L, 4L, 3L, 4L,
    0L, 0L, 0L, 5L, 0L
  ), nrow = 2, ncol = 5, byrow = TRUE)
  lens <- c(1L, 1L, 1L, 2L, 1L)
  edge_id_matrix <- matrix(c(
    1L, 1L, 2L, 2L, 3L,
    0L, 0L, 0L, 3L, 0L
  ), nrow = 2, ncol = 5, byrow = TRUE)

  result <- find_bridges(adj_matrix, lens, edge_id_matrix, 3L)
  expect_setequal(result, c(1L, 2L, 3L))
})

test_that("find_bridges single node no edges", {
  adj_matrix <- matrix(0L, nrow = 1, ncol = 1)
  lens <- 0L
  edge_id_matrix <- matrix(0L, nrow = 1, ncol = 1)

  result <- find_bridges(adj_matrix, lens, edge_id_matrix, 0L)
  expect_equal(result, integer(0))
})

test_that("find_bridges two nodes one edge is bridge", {
  # 1 -- 2 (e1). Single edge is a bridge.
  adj_matrix <- matrix(c(2L, 1L), nrow = 1, ncol = 2)
  lens <- c(1L, 1L)
  edge_id_matrix <- matrix(c(1L, 1L), nrow = 1, ncol = 2)

  result <- find_bridges(adj_matrix, lens, edge_id_matrix, 1L)
  expect_equal(result, 1L)
})

test_that("find_bridges multi-edge not a bridge", {
  # Nodes 1 and 2 connected by two parallel edges: e1 and e2.
  # Neither is a bridge (removing one leaves the other).
  #
  # Node 1: 2(e1), 2(e2)
  # Node 2: 1(e1), 1(e2)
  adj_matrix <- matrix(c(
    2L, 1L,
    2L, 1L
  ), nrow = 2, ncol = 2, byrow = TRUE)
  lens <- c(2L, 2L)
  edge_id_matrix <- matrix(c(
    1L, 1L,
    2L, 2L
  ), nrow = 2, ncol = 2, byrow = TRUE)

  result <- find_bridges(adj_matrix, lens, edge_id_matrix, 2L)
  expect_equal(result, integer(0))
})
