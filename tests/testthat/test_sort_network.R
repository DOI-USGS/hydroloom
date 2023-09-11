test_that("get_sorted", {
  test_data <- data.frame(id = c(1, 2, 3, 4, 6, 7, 8, 9),
                          toid = c(2, 3, 4, 9, 7, 8, 9, 4))

  expect_error(sort_network(test_data), "found one or more pairs of features that reference eachother.")

  test_data <- data.frame(id = c(1, 2, 3, 4, 6, 7, 8, 9),
                          toid = c(2, 3, 4, 0, 7, 8, 9, 4))

  expect_equal(nrow(sort_network(test_data)), nrow(test_data))

  test_data <- data.frame(id = c(1, 2, 3, 4),
                          toid = c(2, 3, 4, 0))

  expect_equal(nrow(sort_network(test_data)), nrow(test_data))

  test_data <- data.frame(id = c(1, 2, 3, 4, 6, 7, 8, 9),
                          toid = c(2, 3, 4, 0, 7, 8, 9, 0))

  split_net <- sort_network(test_data, split = TRUE)

  expect_equal(names(split_net), c("id", "toid", "terminal_id"))

  expect_equal(split_net$id, c(6, 7, 8, 9, 1, 2, 3, 4))

  split_net <- sort_network(test_data, split = TRUE, outlets = c(3, 8))

  expect_equal(nrow(split_net), 6)

  expect_equal(split_net$terminal_id, c(8, 8, 8, 3, 3, 3))

  x <- readRDS(list.files(pattern = "network.rds", recursive = TRUE, full.names = TRUE))

  expect_error(sort_network(add_toids(x, return_dendritic = TRUE),
                            split = TRUE, outlets = c(11690196, 11689718)),
               "Are two or more outlets within the same network?")

  x <- sort_network(add_toids(x, return_dendritic = TRUE),
                    split = TRUE, outlets = c(11690186, 11689280))

  expect_true(!x$toid[x$COMID == 11690186] %in% x$COMID)
  expect_true(!x$toid[x$COMID == 11689280] %in% x$COMID)

  x <- sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom"))

  x <- rename_geometry(x, "geometry_test")

  x <- sort_network(add_toids(x))

  expect_s3_class(x, "sf")

  x <- readRDS(list.files(pattern = "coastal.rds", recursive = TRUE, full.names = TRUE))

  x <- x[order(x$hydroseq), ]

  y <- sort_network(x, split = TRUE)

  expect_true(which(y$comid == 2544325) <
                which(y$comid == 2544321))

  expect_equal(length(unique(y$terminal_id)), 32)

  y <- sort_network(x, split = TRUE, outlets = 2544457)

  expect_true(all(y$terminal_id == 2544457))

})

test_that("non-dendritic issues", {

  x <- readRDS(list.files(pattern = "network.rds", recursive = TRUE, full.names = TRUE))

  y <- add_toids(x, FALSE)

  z <- sort_network(y)

  expect_equal(nrow(y), nrow(z))

  x <- sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom"))

  y <- add_toids(x, FALSE)

  z <- sort_network(y)

  z$topo_sort <- nrow(z):1

  # this is a case upstream of a divergence that was complicated.
  expect_true(z$topo_sort[which(z$COMID == 8893296)] > z$topo_sort[which(z$COMID == 8893378)])

  y <- dplyr::filter(x, COMID %in% navigate_hydro_network(x, start = "8894172", mode = "UT"))

  y <- add_toids(y, return_dendritic = FALSE)

  z <- sort_network(y)

  z <- dplyr::left_join(z,
                        tibble(COMID = unique(z$COMID), topo_sort = length(unique(z$COMID)):1),
                        by = "COMID")

  # the two non dendritic paths should have a smaller topo sort than the one upstream of them.
  expect_true(all(c(z$topo_sort[which(z$COMID == 8893472)],
                    unique(z$topo_sort[which(z$COMID == 8893424)])) <
                    z$topo_sort[which(z$COMID == 8893420)]))

  expect_equal(nrow(y), nrow(z))
})

test_that("no stated outlet", {

  wbd <- structure(list(id = c("030300020608", "030300020702", "030300020610",
                                  "030300020701", "030300020509", "030300020607", "030300020507",
                                  "030300020604", "030300020603", "030300020606", "030202010801",
                                  "030300020605", "030300020602", "030300020503", "030300020601",
                                  "030202010502", "030202010504", "030202010403", "030202010302",
                                  "030202010304", "030202010303"),
                        toid = c("030300020610", "030300020704",
                                  "030300020704", "030300020702", "030300020701", "030300020610",
                                  "030300020509", "030300020610", "030300020604", "030300020607",
                                  "030202010803", "030300020610", "030300020604", "030300020506",
                                  "030300020604", "030202010504", "030202010602", "030202010404",
                                  "030202010303", "030202010404", "030202010304")),
                   row.names = c(NA, 21L), class = c("tbl_df", "tbl", "data.frame"))

  expect_warning(expect_warning(net <- hydroloom::sort_network(wbd)))

  expect_equal(nrow(wbd), nrow(net))

})

test_that("divergences as outlets limit the result", {
  net <- dplyr::tibble(
    id = c("59149125", "59149129", "56784393", "59149189"),
    toid = c("59149189", "59149189", "", ""),
    gnis_id = c(NA_character_, NA_character_, NA_character_, NA_character_),
    feature_type = c(460L, 336L, 460L, 336L),
    direction = c(709L, 709L, 713L, 713L),
  ) |>
    structure(
      class = c("hy", "tbl_df", "tbl", "data.frame"),
      orig_names = c(
        id = "id", fromnode = "fromnode", tonode = "tonode", gnis_id = "gnis_id",
        ftype = "feature_type", direction = "direction"
      )
    )

  net2 <- sort_network(net, split = TRUE)

  expect_equal(net2$id, c("59149125", "59149129", "59149189", "56784393"))
  expect_equal(net2$terminal_id, c("59149189", "59149189", "59149189", "56784393"))

})

test_that("custom outlet values aren't messed up", {
  net <- readRDS(list.files(pattern = "sort_test.rds", recursive = TRUE, full.names = TRUE))

  warnings <- capture_warnings(out <- hydroloom::sort_network(dplyr::rename(net, id = ID, toid = toNexID)))

  expect_true(which(out$toid == -1) > which(out$toid == 24))

  expect_equal(sum(grepl("Outlets don't follow|no outlet found", warnings)), 2)
})

test_that("loop warning", {
  test_data <- data.frame(id = c(1, 2, 3, 4, 5, 5, 6, 7),
                          toid = c(3, 4, 4, 5, 6, 3, 7, 0))
  expect_warning(sort_network(test_data), "loops")
})
