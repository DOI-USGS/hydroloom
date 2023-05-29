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

  suppressWarnings(expect_warning(hydroloom:::check_hy_graph_internal(g, c(1, 2)),
                                  "loop"))

  suppressWarnings(remove <- check_hy_graph(test_data, loop_check = TRUE))

  expect_equal(remove,
               structure(list(id = c(3),
                              toid = c(5)),
                         class = c("hy",
                                   "tbl_df", "tbl", "data.frame"),
                         row.names = c(NA, -1L),
                         orig_names = c(id = "id", toid = "toid")))

  test_data <- data.frame(id = c(1, 1, 2, 3, 4, 5, 6, 6, 7, 8),
                        toid = c(2, 3, 4, 7, 5, 6, 2, 7, 8, 0))

  g <- make_index_ids(test_data)

  suppressWarnings(expect_warning(hydroloom:::check_hy_graph_internal(g, 1),
                 "loop"))

  suppressWarnings(remove <- check_hy_graph(test_data, loop_check = TRUE))

  expect_equal(remove, structure(list(id = c(2),
                                      toid = c(4)),
                                 class = c("hy", "tbl_df", "tbl", "data.frame"),
                                 row.names = c(NA, -1L),
                                 orig_names = c(id = "id", toid = "toid")))


  test_data <- data.frame(id = c(1, 1, 3, 2, 4, 5),
                          toid = c(3, 2, 4, 4, 5, 0))

  g <- make_index_ids(test_data)

  check <- hydroloom:::check_hy_graph_internal(g, 1)

  expect_equal(check, numeric())

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

  g$toid[g$id == "31325125"] <- ""

  gi <- make_index_ids(g)

  expect_true(hydroloom::check_hy_graph(g, loop_check = TRUE))

  g <- readRDS(list.files(pattern = "loop2.rds", recursive = TRUE, full.names = TRUE))

  g$toid[!g$toid %in% g$id] <- ""

  g$toid[g$id == "{9C33A204-E0C5-4047-A81E-23647ED06E2A}"] <- ""

  # found all these loops with some investigation of output but the behavior isn't great
  # can use this to improve hanlding when loops are found
  g$toid[g$id == "{4E7E9D38-373D-4FDA-A920-00EA2AE826EA}"] <- "{89B98684-C80B-4BFB-ADE7-604E87539981}"
  g$toid[g$id == "{B3F20B3F-9141-4009-8935-4930DE818192}"] <- "{89B98684-C80B-4BFB-ADE7-604E87539981}"

  g$toid[g$id == "{99E532FB-6ADD-4DD2-B10F-E0A687590428}"] <- "{0874C471-4F58-471B-B2A0-6140D6332E7D}"

  g$toid[g$id == "{782C5B65-07A0-48BF-9A84-969CEF510DFC}"] <- "{9662E643-E8B7-4B94-B23A-4141E486D5B3}"

  g$toid[g$id == "{2BB601E0-6711-4B5E-92F3-852227D46BE0}"] <- "{B9A43856-3A49-4DF7-8B05-51A26781F539}"

  g$toid[g$id == "{4E7E9D38-373D-4FDA-A920-00EA2AE826EA}"] <- "{B9A43856-3A49-4DF7-8B05-51A26781F539}"
  g$toid[g$id == "{B3F20B3F-9141-4009-8935-4930DE818192}"] <- "{B9A43856-3A49-4DF7-8B05-51A26781F539}"

  g <- distinct(g)

  expect_true(hydroloom::check_hy_graph(g, loop_check = TRUE))

})

test_that("recombine", {
  # this could be used as a test later -- the downstream recombinations look like
  # loops unless you track parents.

  test_data <- data.frame(id = c(1, 2, 2, 3, 4, 4, 5, 6, 7, 9, 8, 10),
                          toid = c(2, 3, 7, 4, 5, 8, 6, 0, 9, 5, 10, 6))

  # igraph::plot.igraph(igraph::graph_from_data_frame(test_data))

  g <- make_index_ids(test_data)

  expect_equal(hydroloom:::check_hy_graph_internal(g, which(g$to_list$id == 1)),
               numeric())
})
