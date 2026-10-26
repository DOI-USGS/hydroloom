test_that("real data", {
  x <- sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom"))
  g <- hy(x)

  g <- add_toids(g, return_dendritic = FALSE)

  g <- dplyr::select(sf::st_drop_geometry(g), id, toid)

  gg <- make_index_ids(g, mode = "to")

  expect_error(navigate_network_dfs(g, 12345))

  expect_error(navigate_network_dfs(gg, 12345),
    "all starts must be in x")

  expect_error(navigate_network_dfs(gg, 8891152, direction = "up"),
    "'down' if x contains to index ids")

  expect_error(navigate_network_dfs(list(), 8891152, direction = "down"),
    "make_index_ids")
})

test_that("main", {
  f <- sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom"))

  x <- to_flownetwork(f)

  upmain_df <- distinct(select(x, id, upmain))

  y <- make_index_ids(x[, c("id", "toid")])

  z <- suppressWarnings(make_fromids(y, return_list = TRUE))

  expect_error(navigate_network_dfs(z, 8897784, "upmain"), "include main element")
  expect_error(navigate_network_dfs(y, 8897784, "downmain"), "must include main element")

  y <- make_index_ids(x)

  z <- suppressWarnings(make_fromids(y, return_list = TRUE, upmain = upmain_df))

  expect_error(navigate_network_dfs(y, 8897784, "upmain"), "must be 'down'")
  expect_error(navigate_network_dfs(z, 8897784, "downmain"), "must be 'up'")

  upmain_hy <- navigate_network_dfs(f, 8897784, "upmain")

  upmain_ids <- navigate_network_dfs(z, 8897784, "upmain")

  expect_equal(upmain_hy, upmain_ids)
})
