test_that("get_partial_length", {
  x <- sf::read_sf(system.file("extdata", "walker.gpkg", package = "hydroloom"))

  hydro_location <- list(comid = 5329339,
    reachcode = "18050005000078",
    reach_meas = 30)

  pl30 <- get_partial_length(hydro_location, x)

  hydro_location <- list(comid = 5329339,
    reachcode = "18050005000078",
    reach_meas = 60)

  pl60 <- get_partial_length(hydro_location, x)

  expect_true(pl30$dn < pl60$dn)

  expect_true(pl30$up > pl60$up)

  expect_error(get_partial_length(hydro_location),
    "network must be supplied if flowline is null")

  hydro_location <- list(comid = 5329339,
    reachcode = "180500050000bork",
    reach_meas = 60)

  expect_error(get_partial_length(hydro_location, x),
    "hydrolocation not found in network provided")

  hydro_location <- list(comid = 5329339,
    reachcode = "18050005000078",
    reach_meas = 100)

  pl100 <- get_partial_length(hydro_location, x)

  expect_equal(pl100, list(dn = 4.786, up = 0))

  hydro_location <- list(comid = 5329339,
    reachcode = "18050005000078",
    reach_meas = 0)

  pl0 <- get_partial_length(hydro_location, x)

  expect_equal(pl0, list(dn = 0, up = 4.786))
})
