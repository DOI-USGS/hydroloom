test_that("divergence check requires its attributes", {
  expect_error(
    check_hy_graph(data.frame(id = 1:2, toid = c(2, 0)),
      divergence_check = TRUE),
    "requires attributes: fromnode, divergence")
})

test_that("loop check", {
  old_opts <- pbapply::pboptions()
  pbapply::pboptions(type = "none")

  # makes a loop of three features 3 -> 5 -> 4 -> 3
  test_data <- data.frame(id = c(1, 2, 3, 4, 5, 5, 6),
    toid = c(3, 5, 5, 3, 4, 6, 0))

  g <- make_index_ids(test_data, mode = "both")

  suppressMessages(
    expect_warning(expect_warning(expect_warning(hydroloom:::check_hy_graph_internal(g, c(1, 2)),
      "loop"), "loop"), "loop"))

  test_data <- data.frame(id = c(1, 1, 2, 3, 4, 5, 6, 6, 7, 8),
    toid = c(2, 3, 4, 7, 5, 6, 2, 7, 8, 0))

  g <- make_index_ids(test_data, mode = "both")

  suppressWarnings(expect_warning(hydroloom:::check_hy_graph_internal(g, 1),
    "loop"))

  test_data <- data.frame(id = c(1, 1, 3, 2, 4, 5),
    toid = c(3, 2, 4, 4, 5, 0))

  g <- make_index_ids(test_data, mode = "both")

  check <- hydroloom:::check_hy_graph_internal(g, 1)

  expect_equal(check, numeric())

  pbapply::pboptions(old_opts)
})

test_that("recombine", {
  # this could be used as a test later -- the downstream recombinations look like
  # loops unless you track parents.

  test_data <- data.frame(id = c(1, 2, 2, 3, 4, 4, 5, 6, 7, 9, 8, 10),
    toid = c(2, 3, 7, 4, 5, 8, 6, 0, 9, 5, 10, 6))

  #nolint start
  # igraph::plot.igraph(igraph::graph_from_data_frame(test_data))
  #nolint end

  g <- make_index_ids(test_data, mode = "both")

  expect_equal(hydroloom:::check_hy_graph_internal(g, which(g$to$to_list$id == 1)),
    numeric())
})
