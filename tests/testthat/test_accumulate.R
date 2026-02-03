test_that("accumulate downstream", {
  test_data <- data.frame(id = c(1, 2, 3, 4, 6, 7, 8, 9),
    toid = c(2, 3, 4, 0, 7, 8, 9, 4),
    a = c(1, 2, 3, 4, 1, 2, 3, 4))

  expect_message(
    expect_equal(accumulate_downstream(test_data, "a"),
      c(1, 3, 6, 20, 1, 3, 6, 10)), "Dendritic routing will be applied")

  expect_error(accumulate_downstream(test_data, "b"), "b must be in x")

  expect_error(accumulate_downstream(dplyr::rename(test_data, borked = toid), "a"),
    "accumulate_downstream requires")

  x <- sf::read_sf(system.file("extdata", "walker.gpkg", package = "hydroloom"))

  x <- add_toids(x, return_dendritic = TRUE)

  expect_message(
    y <- accumulate_downstream(x, "AreaSqKM"),
    "Dendritic")

  expect(mean(abs(y - x$TotDASqKM)) < 1e-3, "drainage area not close enough")
  expect(max(abs(y - x$TotDASqKM)) < 1e-2, "drainage area not close enough")

  x$AreaSqKM[1] <- NA

  expect_warning(accumulate_downstream(x, "AreaSqKM", quiet = TRUE), "NA values found")
})

test_that("divergences with total", {
  skip()
  x <- sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom"))

  net <- navigate_network_dfs(x, 8893236, "up")

  x <- x[x$COMID %in% unlist(net), ]

  z <- x |>
    dplyr::select(COMID, FromNode, ToNode, Divergence, AreaSqKM, TotDASqKM)

  start <- c(8895516, 8893238, 8893180, 8893238, 8893216)
  remove <- navigate_network_dfs(z, start, "up")

  remove <- unlist(remove)
  remove <- remove[!remove %in% start]

  z <- z[!z$COMID %in% remove, ]
  # nolint start
  # d <- sf::st_transform(sf::st_simplify(sf::st_transform(z, 5070),
  #                                       dTolerance = units::as_units(100, "m")),
  #                       4326)
  # d <- add_toids(d, return_dendritic = FALSE)
  # d <- sort_network(d)
  # d <- select(d, -toid) |> distinct()
  # d$id <- 1:nrow(d)
  #
  # nodes <- 1:length(unique(c(d$FromNode, d$ToNode)))
  # nodes <- data.frame(new_node = nodes,
  #                     old_node = unique(c(d$FromNode, d$ToNode)))
  #
  # d <- left_join(d, rename(nodes, fromnode = "new_node"),
  #                by = c("FromNode" = "old_node"))
  # d <- left_join(d, rename(nodes, tonode = "new_node"),
  #                by = c("ToNode" = "old_node"))
  #
  # d <- select(d, id, fromnode, tonode,
  #             divergence = Divergence,
  #             areasqkm = AreaSqKM)

  # sf::write_sf(d, "tests/testthat/data/simple_diversions.geojson")
  # nolint end

  z$dend_totdasqkm <- accumulate_downstream(z, var = "AreaSqKM", quiet = TRUE)

  # for dendritic, we shoud have the outlet be the sum of everything
  expect_equal(max(z$dend_totdasqkm), sum(z$AreaSqKM))
  # a diversion should have its own area as its total
  expect_equal(z$dend_totdasqkm[z$COMID == 8893212], z$AreaSqKM[z$COMID == 8893212])

  z$divergence_fraction <- 1
  z$divergence_fraction[z$Divergence == 2] <- 0.25
  z$divergence_fraction[z$Divergence == 1] <- 0.75

  z$div_totdasqkm <- accumulate_downstream(z, var = "AreaSqKM")

  # we shoud have the outlet be the sum of everything
  expect_equal(max(z$div_totdasqkm), sum(z$AreaSqKM))
  # a diversion should have its own area plus its diversion fraction from upstream
  expect_equal(z$div_totdasqkm[z$COMID == 8893212],
    z$AreaSqKM[z$COMID == 8893212] + 0.25 * z$div_totdasqkm[z$COMID == 8893184])
  # a main should have its own area plus its diversion fraction from upstream
  expect_equal(z$div_totdasqkm[z$COMID == 8893182],
    z$AreaSqKM[z$COMID == 8893182] + 0.75 * z$div_totdasqkm[z$COMID == 8893184])

  z$tot_totdasqkm <- accumulate_downstream(z, "AreaSqKM", total = TRUE)

  # a diversion should have its own area plus everything from upstream
  expect_equal(z$tot_totdasqkm[z$COMID == 8893210],
    z$AreaSqKM[z$COMID == 8893210] + z$tot_totdasqkm[z$COMID == 8893190])
  # a main should have its own area plus its diversion fraction from upstream
  expect_equal(z$tot_totdasqkm[z$COMID == 8893186],
    z$AreaSqKM[z$COMID == 8893186] + z$tot_totdasqkm[z$COMID == 8893190])

  # need all the stuff upstream but only reachable on a diversion in this goofy edge case
  expect_equal(z$tot_totdasqkm[z$COMID == 8893202],
    z$AreaSqKM[z$COMID == 8893202] + sum(z$AreaSqKM[z$COMID == 8893192],
      z$AreaSqKM[z$COMID == 8893172],
      z$AreaSqKM[z$COMID == 8893176],
      z$AreaSqKM[z$COMID == 8893174],
      z$tot_totdasqkm[z$COMID == 8893194]))

  expect_equal(z$tot_totdasqkm[z$COMID == 8893218],
    z$AreaSqKM[z$COMID == 8893218] + sum(z$tot_totdasqkm[z$COMID == 8893202],
      z$AreaSqKM[z$COMID == 8893198],
      z$AreaSqKM[z$COMID == 8893208],
      z$AreaSqKM[z$COMID == 8893212]))

  # we shoud have the outlet be the sum of everything
  expect_equal(max(z$tot_totdasqkm), sum(z$AreaSqKM))

  z <- x |>
    dplyr::select(COMID, LevelPathI, FromNode, ToNode, Divergence, AreaSqKM, TotDASqKM)

  z$tot_totdasqkm <- accumulate_downstream(z, "AreaSqKM", total = TRUE)

  expect_equal(z$tot_totdasqkm, z$TotDASqKM)
})

test_that("simple diversions total", {
  skip()

  x <- sf::read_sf(list.files(pattern = "simple_diversions.geojson", full.names = TRUE, recursive = TRUE))

  x$tot_totareasqkm <- accumulate_downstream(x, "areasqkm", total = TRUE)

  check_fun <- function(check_id) {
    up_net <- navigate_network_dfs(x, check_id, "up")

    expect_equal(x$tot_totareasqkm[x$id == check_id],
      sum(x$areasqkm[x$id %in% unique(unlist(up_net))]))
  }

  # need all the stuff upstream but only reachable on a diversion in this goofy edge case
  check_fun(25)
  check_fun(27)
  check_fun(21)
  check_fun(29)

  # we shoud have the outlet be the sum of everything
  expect_equal(max(x$tot_totareasqkm), sum(x$areasqkm))

  Sys.setenv(accumulate_debug = "debug")

  node_record <- accumulate_downstream(x, "areasqkm", total = TRUE)

  outlet_node <- node_record[[2]]$tonode[node_record[[2]]$id == 32]

  expect_equal(nrow(node_record[[1]]$open[outlet_node][[1]]), 0)

  Sys.unsetenv("accumulate_debug")

})

test_that("complex diversions", {
  skip()
  net <- read.csv(list.files(pattern = "diversions.csv", full.names = TRUE, recursive = TRUE))

  net$tot_totareasqkm <- accumulate_downstream(net, "areasqkm", TRUE)

  # these compare to NHDPlus with the addition of 184.3632 that was removed for testing
  # this only holds on the path downstream of the main set of diversions in this example
  net$diff <- net$tot_totareasqkm - (net$totdasqkm - 184.3632)

  expect_equal(net$tot_totareasqkm[net$comid == 14702428], sum(
    net$areasqkm[net$comid == 14702428],
    net$areasqkm[net$comid == 14702926],
    net$areasqkm[net$comid == 14702436],
    net$areasqkm[net$comid == 14703168],
    net$areasqkm[net$comid == 14702438]
  ))

  check_fun <- function(check_comid) {
    up_net <- navigate_network_dfs(net, check_comid, "up")

    expect_equal(net$tot_totareasqkm[net$comid == check_comid],
      sum(net$areasqkm[net$comid %in% unique(unlist(up_net))]))
  }

  check_fun(14702384)
  check_fun(14702360)
  check_fun(14702406)
  check_fun(14702402)
  check_fun(14702400)
  check_fun(14702376)
  check_fun(14702374)
  check_fun(14702378)
  check_fun(14702336)
  check_fun(14702328)
  check_fun(14702352)

  Sys.setenv(accumulate_debug = "debug")

  node_record <- accumulate_downstream(net, "areasqkm", total = TRUE)

  outlet_node <- node_record[[2]]$tonode[node_record[[2]]$id == 14702352]

  expect_equal(nrow(node_record[[1]]$open[outlet_node][[1]]), 0)

  Sys.unsetenv("accumulate_debug")

})

test_that("part closed test", {
  skip()
  net <- read.csv(text =
    "id,toid,divergence
  1,2,0
  1,3,0
  3,4,2
  3,5,2
  4,6,2
  5,6,1
  2,7,1
  6,7,0
  7,0,0")

  net$val <- 1

  expect_equal(accumulate_downstream(net, "val", TRUE), c(1, 1, 2, 2, 3, 3, 2, 5, 7))

  Sys.setenv(accumulate_debug = "debug")

  node_record <- accumulate_downstream(net, "val", total = TRUE)

  outlet_node <- node_record[[2]]$tonode[node_record[[2]]$id == 7]

  expect_equal(nrow(node_record[[1]]$open[outlet_node][[1]]), 0)

  Sys.unsetenv("accumulate_debug")

  net <- read.csv(text =
    "id,toid,divergence
  1,2,0
  1,3,0
  3,4,1
  3,5,1
  4,6,2
  5,6,1
  2,7,2
  6,7,0
  7,0,0")

  net$val <- 1

  expect_equal(accumulate_downstream(net, "val", TRUE), c(1, 1, 2, 2, 3, 3, 2, 5, 7))

  Sys.setenv(accumulate_debug = "debug")

  node_record <- accumulate_downstream(net, "val", total = TRUE)

  outlet_node <- node_record[[2]]$tonode[node_record[[2]]$id == 7]

  expect_equal(nrow(node_record[[1]]$open[outlet_node][[1]]), 0)

  Sys.unsetenv("accumulate_debug")

  net <- read.csv(text =
    "id, toid, divergence
                  1,2,0
                  1,3,0
                  1,4,0
                  5,6,0
                  2,6,1
                  3,7,2
                  4,7,2
                  6,8,0
                  7,8,0
                  8,0,0")

  net$val <- 1

  expect_equal(accumulate_downstream(net, "val", total = TRUE), c(1, 1, 1, 1, 2, 2, 2, 4, 4, 8))

})
