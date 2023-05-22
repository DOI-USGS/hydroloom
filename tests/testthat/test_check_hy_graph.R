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

  expect_warning(hydroloom:::navigate_network_dfs_internal(g, 1, FALSE, check_dag = TRUE),
                 "loop")

  suppressWarnings(remove <- check_hy_graph(test_data, loop_check = TRUE))

  expect_equal(hy_reverse(remove), dplyr::tibble(id = c(4),
                                                 toid = c(3)))

  test_data <- data.frame(id = c(1, 1, 2, 3, 4, 5, 6, 6, 7),
                        toid = c(2, 3, 4, 7, 5, 6, 2, 7, 0))

  g <- make_index_ids(test_data)

  expect_warning(hydroloom:::navigate_network_dfs_internal(g, 1, FALSE, check_dag = TRUE),
                 "loop")

  suppressWarnings(remove <- check_hy_graph(test_data, loop_check = TRUE))

  expect_equal(hy_reverse(remove), dplyr::tibble(id = c(2),
                                                 toid = c(4)))


  test_data <- data.frame(id = c(1, 1, 3, 2, 4, 5),
                          toid = c(3, 2, 4, 4, 5, 0))

  g <- make_index_ids(test_data)

  check <- hydroloom:::navigate_network_dfs_internal(g, 1, FALSE, check_dag = TRUE)

  expect_equal(check, NA_integer_)

  expect_true(check_hy_graph(test_data, loop_check = TRUE))
})

test_that("more check", {
  g <- sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom"))
  f <- add_toids(g)
  expect_true(check_hy_graph(f, loop_check = TRUE))
})
