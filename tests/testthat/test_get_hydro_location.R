test_that("get location", {
  source(system.file("extdata", "sample_flines.R", package = "nhdplusTools"))

  points <- sf::st_sfc(sf::st_sfc(list(sf::st_point(c(-76.86934, 39.49328)),
                                       sf::st_point(c(-76.91711, 39.40884)),
                                       sf::st_point(c(-76.88081, 39.36354))),
                                  crs = 4326))

  indexes <- index_points_to_lines(sample_flines, points)

  locations <- get_hydro_location(indexes, sample_flines)

  expect_equal(sf::st_coordinates(locations)[, 1:2],
               structure(c(-76.8693957911233, -76.9176139910484, -76.8810037244386,
                           39.4932572053652, 39.4090934721626, 39.3632976055671),
                         .Dim = 3:2, .Dimnames = list(c("1", "2", "3"),
                                                      c("X", "Y"))))

  points <- sf::st_sfc(sf::st_sfc(list(sf::st_point(c(-76.86934, 39.49328))),
                                  crs = 4326))

  indexes <- index_points_to_lines(sample_flines, points)

  locations <- get_hydro_location(indexes, sample_flines)

  expect_equal(sf::st_coordinates(locations),
               sf::st_coordinates(sf::st_sfc(sf::st_point(c(-76.8694, 39.49326)), crs= 4326)),
               tolerance = 0.001)

})
