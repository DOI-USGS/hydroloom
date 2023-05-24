test_that("base check_hy_graph", {
  test_data <- data.frame(id = c(1, 2, 3, 4, 6, 7, 8, 9),
                          toid = c(2, 3, 4, 9, 7, 8, 9, 4))

  remove <- check_hy_graph(test_data)

  expect_equal(remove$row, c(4, 8))
})

test_that("loop check", {
  # makes a loop of three features 3 -> 5 -> 4 -> 3
  test_data <- data.frame(id = c(1, 2, 3, 4, 5, 5, 6),
                        toid = c(3, 5, 5, 3, 4, 6, 0))

  g <- make_index_ids(test_data)

  expect_warning(hydroloom:::check_hy_graph_internal(g, 1),
                 "loop")

  expect_warning(hydroloom:::check_hy_graph_internal(g, 2),
                 "loop")

  suppressWarnings(remove <- check_hy_graph(test_data, loop_check = TRUE))

  expect_equal(remove,
               structure(list(id = c(3, 4, 5, 5, 6),
                              toid = c(5, 3, 4, 6, 0)),
                         class = c("hy",
                                   "tbl_df", "tbl", "data.frame"),
                         row.names = c(NA, -5L),
                         orig_names = c(id = "id", toid = "toid")))

  test_data <- data.frame(id = c(1, 1, 2, 3, 4, 5, 6, 6, 7, 8),
                        toid = c(2, 3, 4, 7, 5, 6, 2, 7, 8, 0))

  g <- make_index_ids(test_data)

  expect_warning(hydroloom:::check_hy_graph_internal(g, 1),
                 "loop")

  suppressWarnings(remove <- check_hy_graph(test_data, loop_check = TRUE))

  expect_equal(remove, structure(list(id = c(2, 4, 5, 6, 6, 7, 8),
                                      toid = c(4, 5, 6, 2, 7, 8, 0)),
                                 class = c("hy", "tbl_df", "tbl", "data.frame"),
                                 row.names = c(NA, -7L),
                                 orig_names = c(id = "id", toid = "toid")))


  test_data <- data.frame(id = c(1, 1, 3, 2, 4, 5),
                          toid = c(3, 2, 4, 4, 5, 0))

  g <- make_index_ids(test_data)

  check <- hydroloom:::check_hy_graph_internal(g, 1)

  expect_equal(check, NA_integer_)

  expect_true(check_hy_graph(test_data, loop_check = TRUE))
})

test_that("more check", {
  g <- sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom"))
  f <- add_toids(g)
  expect_true(check_hy_graph(f, loop_check = TRUE))
})

test_that("big_check", {
  g <- readRDS(list.files(pattern = "loop.rds", recursive = TRUE, full.names = TRUE))

  g$toid[!g$toid %in% g$id] <- ""

  g$toid[g$id == 31325125] <- ""

  gi <- make_index_ids(g)

  expect_equal(hydroloom:::check_hy_graph_internal(gi, which(gi$to_list$id == "31325075")),
               NA_integer_)

  expect_equal(hydroloom:::check_hy_graph_internal(gi, which(gi$to_list$id == "31325137")),
               NA_integer_)

})

test_that("recombine", {
  # this could be used as a test later -- the downstream recombinations look like
  # loops unless you track parents.

  test_data <- data.frame(id = c(1, 2, 2, 3, 4, 4, 5, 6, 7, 9, 8, 10),
                          toid = c(2, 3, 7, 4, 5, 8, 6, 0, 9, 5, 10, 6))

  g <- make_index_ids(test_data)

  expect_equal(hydroloom:::check_hy_graph_internal(g, which(g$to_list$id == 1)),
               NA_integer_)
})
