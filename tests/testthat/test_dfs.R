test_that("super basic", {
  net <- data.frame(id = c(2, 3, 3, 4, 5, 6, 7, 8, 9),
                toid = c(3, 4, 5, 6, 7, 8, 8, 9, 0))

  paths <- all_paths_dfs(net, 2, direction = "down")

  expect_equal(paths,
               list(list(`1` = c(2, 3, 5, 7, 8, 9),
                         `2` = c(4, 6, 8))))
})

test_that("total div", {
  net <- data.frame(id = c(2, 3, 3, 4, 5, 6, 7, 8, 9, 10),
                  toid = c(3, 4, 5, 6, 7, 8, 9, 10, 0, 0))

  paths <- all_paths_dfs(net, 2, direction = "down")

  expect_equal(paths,
               list(list(`1` = c(2, 3, 5, 7, 9),
                         `2` = c(4, 6, 8, 10))))
})

test_that("real data", {
  x <- sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom"))
  g <- hy(x)

  g <- add_toids(g, return_dendritic = FALSE)

  g <- dplyr::select(sf::st_drop_geometry(g), id, toid)

  paths <- all_paths_dfs(g, 8893402, direction = "down")

  expect_equal(length(paths), 1)

  expect_equal(length(paths[[1]]), 7)

  expect_equal(length(paths[[1]][[1]]), 20)

  # the tail of diverted paths are back on the path they rejoin
  expect_true(paths[[1]][[2]][3] %in% paths[[1]][[1]])

  paths <- all_paths_dfs(g, starts = 8891152)

  expect_equal(length(paths[[1]]), 47)
})
