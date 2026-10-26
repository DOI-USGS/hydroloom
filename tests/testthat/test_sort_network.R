test_that("get_sorted", {
  test_data <- data.frame(id = c(1, 2, 3, 4, 6, 7, 8, 9),
    toid = c(2, 3, 4, 9, 7, 8, 9, 4))

  expect_error(sort_network(test_data), "Some flowlines drain back into themselves")

  test_data <- data.frame(id = c(1, 2, 3, 4),
    toid = c(2, 3, 4, 0))

  expect_equal(c("id", "toid", "topo_sort"),
    names(add_topo_sort(test_data)))

  test_data <- data.frame(id = c(1, 2, 3, 4, 6, 7, 8, 9),
    toid = c(2, 3, 4, 0, 7, 8, 9, 0))

  split_net <- sort_network(test_data, split = TRUE)

  expect_equal(names(split_net), c("id", "toid", "terminal_id"))

  x <- readRDS(list.files(pattern = "network.rds", recursive = TRUE, full.names = TRUE))

  expect_error(sort_network(add_toids(x, return_dendritic = TRUE),
    split = TRUE, outlets = c(11690196, 11689718)),
  "Are two or more outlets within the same network?")

  x <- sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom"))

  x <- rename_geometry(x, "geometry_test")

  x <- sort_network(add_toids(x))

  expect_s3_class(x, "sf")

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

  # An HUC network with toids pointing to downstream HUCs not in the table
  # follows the implicit-absence outlet convention -- no warnings expected.
  expect_no_warning(hydroloom::sort_network(wbd))

})

test_that("custom outlet values aren't messed up", {
  net <- readRDS(list.files(pattern = "sort_test.rds", recursive = TRUE, full.names = TRUE))

  # Custom outlet values (-1, 24) are detected by membership (toids not
  # in id) -- no convention or "no outlet found" warnings expected.
  expect_no_warning(
    hydroloom::sort_network(dplyr::rename(net, id = ID, toid = toNexID))
  )
})

test_that("loop warning", {
  test_data <- data.frame(id = c(1, 2, 3, 4, 5, 5, 6, 7),
    toid = c(3, 4, 4, 5, 6, 3, 7, 0))
  expect_warning(sort_network(test_data), "loops")
})

test_that("duplicated attributes", {
  network <- readRDS(list.files(pattern = "sort_network_dups.rds", full.names = TRUE, recursive = TRUE))

  dedup <- dplyr::distinct(dplyr::select(network, id, toid))

  # Non-canonical and implicit-absence outlets are both valid under the
  # is_outlet() rule -- no warnings expected.
  expect_no_warning(sort_network(network))
  expect_no_warning(sort_network(dedup))
})
