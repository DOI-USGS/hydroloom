
test_that("navigate connected paths", {
  fline <- sf::read_sf(system.file("extdata", "walker.gpkg", package = "hydroloom"))

  outlets <- c(5329357, 5329317, 5329365, 5329435, 5329817)

  fline <- add_toids(fline)

  pl <- navigate_connected_paths(fline, outlets)

  expect_equal(nrow(pl), 10)

  expect_type(pl$path, "list")

  expect_equal(pl$network_distance_km[pl$id_1 == 5329357 & pl$id_2 == 5329365],
               3.6, tolerance = 0.01)
  expect_equal(pl$network_distance_km[pl$id_1 == 5329357 & pl$id_2 == 5329817],
               8.9, tolerance = 0.01)

  outlets <- c(outlets, 5329303)

  pl <- navigate_connected_paths(fline, outlets)

  expect_equal(nrow(pl), 15)

  expect_equal(pl$network_distance_km[pl$id_1 == 5329317 & pl$id_2 == 5329303],
               5.8, tolerance = 0.01)

  expect_error(navigate_connected_paths(fline, c(outlets, 12345)),
                 "All outlets must be in x.")

  mess <- capture_messages(navigate_connected_paths(fline, outlets, status = TRUE))

  pbopts <- pbapply::pboptions(type = "none")
  on.exit(pbapply::pboptions(pbopts), add = TRUE)

  expect_equal(length(mess), 3)
})
