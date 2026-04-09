test_that("check_valid returns NULL for NULL input", {
  expect_null(check_valid(NULL))
})

test_that("check_valid handles valid polygons", {
  p <- sf::st_polygon(list(
    rbind(c(0, 0), c(10, 0), c(10, 10), c(0, 10), c(0, 0))
  ))
  x <- sf::st_sf(geometry = sf::st_sfc(p, crs = 5070))

  result <- check_valid(x)
  expect_s3_class(result, "sf")
  expect_true(all(sf::st_is_valid(result)))
})

test_that("check_valid fixes invalid geometry", {
  # Bowtie polygon (self-intersecting)
  p <- sf::st_polygon(list(
    rbind(c(0, 0), c(10, 10), c(10, 0), c(0, 10), c(0, 0))
  ))
  x <- sf::st_sf(geometry = sf::st_sfc(p, crs = 5070))

  # Message only fires in interactive(); just verify it fixes the geometry
  result <- check_valid(x)
  expect_true(all(sf::st_is_valid(result)))
})

test_that("check_valid reprojects when out_prj differs", {
  p <- sf::st_polygon(list(
    rbind(c(0, 0), c(10, 0), c(10, 10), c(0, 10), c(0, 0))
  ))
  x <- sf::st_sf(geometry = sf::st_sfc(p, crs = 5070))

  result <- check_valid(x, out_prj = sf::st_crs(4326))
  expect_equal(sf::st_crs(result), sf::st_crs(4326))
})

test_that("check_valid demotes single-part MULTIPOLYGON to POLYGON", {
  p <- sf::st_multipolygon(list(
    list(rbind(c(0, 0), c(10, 0), c(10, 10), c(0, 10), c(0, 0)))
  ))
  x <- sf::st_sf(geometry = sf::st_sfc(p, crs = 5070))

  result <- check_valid(x)
  expect_equal(
    as.character(sf::st_geometry_type(result, by_geometry = FALSE)),
    "POLYGON"
  )
})
