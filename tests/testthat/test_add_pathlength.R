test_that("get_terminal", {
  fl <- sf::read_sf(system.file("extdata", "walker.gpkg", package = "hydroloom"))

  fl <- add_toids(fl)

  pl <- add_pathlength(dplyr::select(fl, -Pathlength))

  expect_equal(fl$COMID, pl$COMID)

  expect_equal(fl$Pathlength, pl$pathlength_km, tolerance = 1e-6)
})
