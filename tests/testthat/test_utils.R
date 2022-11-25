test_that("compatibalize", {
  x <- sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom"))
  one <- sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom"))

  attr(one, "sf_column") <- "geotest"
  names(one)[names(one) == "geom"] <- "geotest"

  two <- sf::st_transform(x, 5070)

  three <- st_compatibalize(one, two)

  expect_equal(sf::st_crs(two), sf::st_crs(three))

  expect_true(all(names(two) == names(three)))

})

test_that("drop_geometry", {

  x <- sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom"))

  expect_true("sf" %in% class(x))

  x <- hydroloom:::drop_geometry(x)

  expect_true(!"sf" %in% class(x))

  x <- hydroloom:::drop_geometry(x)

  expect_true(!"sf" %in% class(x))
})

test_that("rname geometry", {
  g <- sf::st_sf(a=3, geo = sf::st_sfc(sf::st_point(1:2)))

  g <- rename_geometry(g, "geometry")

  expect_true("geometry" %in% names(g))

  expect_equal(attr(g, "sf_column"), "geometry")

})
