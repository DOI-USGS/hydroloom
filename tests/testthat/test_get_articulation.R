test_that("get_articulation_flowlines with sf object", {
  g <- sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom"))

  x <- hy(g)
  x <- add_toids(x)

  result <- get_articulation_flowlines(x)

  # Should return numeric ids matching the original COMID type

  expect_true(length(result) > 0)
  expect_true(all(result %in% x$id))

  # just downstream of diversion
  expect_true(all(c(8893722, 8893780) %in% result))

  # within system of diversions
  expect_false(all(c(8893532, 8893534, 8893222) %in% result))

  net <- navigate_network_dfs(x, 8893236, "up")

  x <- x[x$id %in% unlist(net), ]

  result <- get_articulation_flowlines(x)

  expect_false(all(c(8893222, 8893228) %in% result))
})

test_that("get_articulation near an outlet", {
  # a trib joins a diversion near the outlet of a network

  x <- data.frame(
    id =   c(1, 2, 3, 4, 1, 6, 7, 5),
    toid = c(2, 4, 4, 5, 6, 7, 5, 0)
  )

  result <- get_articulation_flowlines(x)

  result
})

test_that("get_articulation_flowlines with data.frame", {
  # Two branches joining at node 4:
  # 1 -> 2 -> 3 -> 4 -> 5 (outlet)
  # 6 -> 7 -> 8 -> 9 -> 4
  x <- data.frame(
    id = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
    toid = c(2, 3, 4, 5, 0, 7, 8, 9, 4)
  )

  result <- get_articulation_flowlines(x)

  # All middle nodes are articulation points (removing any disconnects part of network)
  # Endpoints 1, 5, and 6 are NOT articulation points
  expect_true(all(c(2, 3, 4, 7, 8, 9) %in% result))
  expect_false(1 %in% result)
  expect_false(5 %in% result)
  expect_false(6 %in% result)
})

test_that("get_articulation_flowlines with hy object", {
  x <- data.frame(
    id = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
    toid = c(2, 3, 4, 5, 0, 7, 8, 9, 4)
  )

  result_df <- get_articulation_flowlines(x)
  result_hy <- get_articulation_flowlines(hy(x))

  expect_equal(result_df, result_hy)
})

test_that("get_articulation_flowlines returns original ids", {
  # Use character ids to verify we get original ids back

  x <- data.frame(
    id = c("A", "B", "C", "D", "E"),
    toid = c("B", "C", "D", "E", "")
  )

  result <- get_articulation_flowlines(x)

  # Linear chain: B, C, D are articulation points
  expect_true(all(c("B", "C", "D") %in% result))
  expect_false("A" %in% result)
  expect_false("E" %in% result)
  expect_true(is.character(result))
})

test_that("get_articulation_flowlines with no articulation points", {
  # Triangle network - no articulation points
  # This requires a non-dendritic setup which hydroloom may not support directly

  # Test with single flowline (no articulation point)
  x <- data.frame(
    id = 1,
    toid = 0
  )

  result <- get_articulation_flowlines(x)
  expect_equal(length(result), 0)
})

test_that("get_articulation_flowlines with two connected flowlines", {
  x <- data.frame(
    id = c(1, 2),
    toid = c(2, 0)
  )

  result <- get_articulation_flowlines(x)
  # Neither node is an articulation point in a 2-node graph

  expect_equal(length(result), 0)
})

test_that("single articulation point in linear graph", {
  # 1 -- 2 -- 3
  # node 2 is articulation point
  adj_matrix <- matrix(c(
    2L, 1L, 2L,
    NA_integer_, 3L, NA_integer_
  ), nrow = 2, ncol = 3, byrow = TRUE)
  lens <- c(1L, 2L, 1L)
  
  result <- find_articulation_points(adj_matrix, lens)
  expect_equal(result, 2L)
})

test_that("no articulation points in triangle", {
  # 1 -- 2
  # |  / 
  # 3
  adj_matrix <- matrix(c(
    2L, 1L, 1L,
    3L, 3L, 2L
  ), nrow = 2, ncol = 3, byrow = TRUE)
  lens <- c(2L, 2L, 2L)
  
  result <- find_articulation_points(adj_matrix, lens)
  expect_equal(result, integer(0))
})

test_that("bridge graph with two triangles", {
  # 1 -- 2       5 -- 4
  # |  /   \     |  /
  # 3       \    6
  #          \  /
  #           \/
  # nodes 2 and 6 are articulation points
  adj_matrix <- matrix(c(
    2L, 1L, 1L, 5L, 4L, 2L,
    3L, 3L, 2L, 6L, 6L, 5L,
    NA_integer_, 6L, NA_integer_, NA_integer_, NA_integer_, 4L
  ), nrow = 3, ncol = 6, byrow = TRUE)
  lens <- c(2L, 3L, 2L, 2L, 2L, 3L)
  
  result <- find_articulation_points(adj_matrix, lens)
  expect_setequal(result, c(2L, 6L))
})

test_that("star graph center is articulation point", {
  # 2
  # |
  # 1 -- 3
  # |
  # 4
  adj_matrix <- matrix(c(
    2L, 1L, 1L, 1L,
    3L, NA_integer_, NA_integer_, NA_integer_,
    4L, NA_integer_, NA_integer_, NA_integer_
  ), nrow = 3, ncol = 4, byrow = TRUE)
  lens <- c(3L, 1L, 1L, 1L)
  
  result <- find_articulation_points(adj_matrix, lens)
  expect_equal(result, 1L)
})

test_that("single node has no articulation points", {
  adj_matrix <- matrix(NA_integer_, nrow = 1, ncol = 1)
  lens <- 0L
  
  result <- find_articulation_points(adj_matrix, lens)
  expect_equal(result, integer(0))
})

test_that("two connected nodes have no articulation points", {
  # 1 -- 2
  adj_matrix <- matrix(c(2L, 1L), nrow = 1, ncol = 2)
  lens <- c(1L, 1L)
  
  result <- find_articulation_points(adj_matrix, lens)
  expect_equal(result, integer(0))
})

test_that("chain graph all middle nodes are articulation points", {
  # 1 -- 2 -- 3 -- 4 -- 5
  adj_matrix <- matrix(c(
    2L, 1L, 2L, 3L, 4L,
    NA_integer_, 3L, 4L, 5L, NA_integer_
  ), nrow = 2, ncol = 5, byrow = TRUE)
  lens <- c(1L, 2L, 2L, 2L, 1L)
  
  result <- find_articulation_points(adj_matrix, lens)
  expect_setequal(result, c(2L, 3L, 4L))
})

test_that("disconnected components handled correctly", {
  # 1 -- 2    3 -- 4 -- 5
  adj_matrix <- matrix(c(
    2L, 1L, 4L, 3L, 4L,
    NA_integer_, NA_integer_, NA_integer_, 5L, NA_integer_
  ), nrow = 2, ncol = 5, byrow = TRUE)
  lens <- c(1L, 1L, 1L, 2L, 1L)

  result <- find_articulation_points(adj_matrix, lens)
  expect_equal(result, 4L)
})