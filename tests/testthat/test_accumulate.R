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
  x <- sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom"))

  net <- navigate_network_dfs(x, 8893236, "up")

  x <- x[x$COMID %in% unlist(net),]

  z <- x |>
    dplyr::select(COMID, LevelPathI, FromNode, ToNode, Divergence, AreaSqKM, TotDASqKM)

  start <- c(8895516, 8893238, 8893180, 8893238, 8893216)
  remove <- navigate_network_dfs(z, start, "up")

  remove <- unlist(remove)
  remove <- remove[!remove %in% start]

  z <- z[!z$COMID %in% remove,]

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

  z$tot_totdasqkm <- accumulate_downstream(z, "AreaSqKM", "LevelPathI", total = TRUE)

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


test_that("accumulate_utilities", {

  dup_nodes_1 <- structure(list(node_id = c(48, 48, 48),
                                val = c(86.5719, 86.5719, 86.5719),
                                dup = c(TRUE, FALSE, FALSE)),
                           row.names = c(1L, 13L, 14L), class = "data.frame") |>
    group_by(.data$node_id) |>
    filter(n() > 1) |>
    filter(any(.data$dup) & any(!.data$dup))

  dup_nodes_2 <- structure(list(node_id = c(2, 48, 60, 62, 2, 48, 60, 62, 65, 65),
                                val = c(1.7136, 86.5719, 86.6133, 86.6178, 1.7136, 86.5719, 86.6133, 86.6178, 86.6178, 86.6178),
                                dup = c(FALSE, FALSE, FALSE,TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, TRUE)),
                           class = "data.frame",
                           row.names = c(NA, -10L)) |>
    group_by(.data$node_id) |>
    filter(n() > 1) |>
    filter(any(.data$dup) & any(!.data$dup))

  dup_nodes_3 <- data.frame(
    node_id = c(2, 48, 2, 48, 62, 63, 2, 48, 60, 2, 48, 60, 2, 48, 63, 62, 62, 2, 48),
    val = c(
      1.7136, 86.5719, 1.7136, 86.5719, 86.61779999999999, 86.7096, 1.7136, 86.5719,
      86.6133, 1.7136, 86.5719, 86.6133, 1.7136, 86.5719, 86.7096,
      86.61779999999999, 86.61779999999999, 1.7136, 86.5719
    ),
    dup = rep(rep(c(FALSE, TRUE), 3), rep(c(14L, 1L), c(1L, 5L)))) |>
      group_by(.data$node_id) |>
      filter(n() > 1) |>
      filter(any(.data$dup) & any(!.data$dup))

  expect_equal(sum(reconcile_dup_set(dup_nodes_1)$cancel), 2)

  expect_equal(sum(reconcile_dup_set(dup_nodes_2)$cancel), 2)

  check <- reconcile_dup_set(dup_nodes_3)

  expect_equal(sum(check[check$node_id == 48,]$cancel), 2)

})
