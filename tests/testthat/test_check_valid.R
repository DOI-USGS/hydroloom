test_that("check_valid returns NULL for NULL input", {
  expect_null(check_valid(NULL))
})

test_that("check_valid errors on non-sf input", {
  expect_error(check_valid(data.frame(a = 1)), "requires sf or sfc")
  expect_error(check_valid(42), "requires sf or sfc")
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

test_that("check_valid.sf handles GEOMETRYCOLLECTION from st_make_valid", {
  # Figure-8 with pinch point — invalid polygon that gets repaired
  coords <- rbind(c(0,0), c(5,5), c(10,0), c(10,10), c(5,5), c(0,10), c(0,0))
  p <- sf::st_polygon(list(coords))
  x <- sf::st_sf(geometry = sf::st_sfc(p, crs = 5070))

  result <- check_valid(x)
  expect_true(all(sf::st_is_valid(result)))
  types <- as.character(sf::st_geometry_type(result, by_geometry = TRUE))
  expect_false(any(grepl("^GEOM", types)))
})

test_that("check_valid.sf demotes single-part MULTILINESTRING to LINESTRING", {
  l <- sf::st_multilinestring(list(rbind(c(0, 0), c(1, 1))))
  x <- sf::st_sf(geometry = sf::st_sfc(l, crs = 5070))

  result <- check_valid(x)
  expect_equal(
    as.character(sf::st_geometry_type(result, by_geometry = FALSE)),
    "LINESTRING"
  )
})

test_that("check_valid.sf unifies residual mixed geometry types", {
  # Mix of POLYGON + GEOMETRYCOLLECTION; polygons dominate, GC gets cast/dropped
  p1 <- sf::st_polygon(list(rbind(c(0,0), c(10,0), c(10,10), c(0,10), c(0,0))))
  p2 <- sf::st_polygon(list(rbind(c(20,20), c(30,20), c(30,30), c(20,30), c(20,20))))
  gc <- sf::st_geometrycollection(list(
    sf::st_polygon(list(rbind(c(40,40), c(50,40), c(50,50), c(40,50), c(40,40)))),
    sf::st_linestring(rbind(c(40,40), c(45,45)))
  ))
  x <- sf::st_sf(geometry = sf::st_sfc(p1, p2, gc, crs = 5070))

  result <- check_valid(x)
  types <- as.character(sf::st_geometry_type(result, by_geometry = TRUE))
  expect_false(any(grepl("^GEOM", types)))
})

test_that("check_valid.sfc mirrors sf behavior", {
  # Valid polygon

  p <- sf::st_polygon(list(rbind(c(0,0), c(10,0), c(10,10), c(0,10), c(0,0))))
  result <- check_valid(sf::st_sfc(p, crs = 5070))
  expect_s3_class(result, "sfc")
  expect_true(all(sf::st_is_valid(result)))

  # Bowtie (invalid)
  p <- sf::st_polygon(list(rbind(c(0,0), c(10,10), c(10,0), c(0,10), c(0,0))))
  result <- check_valid(sf::st_sfc(p, crs = 5070))
  expect_true(all(sf::st_is_valid(result)))

  # MULTIPOLYGON demotion
  p <- sf::st_multipolygon(list(
    list(rbind(c(0,0), c(10,0), c(10,10), c(0,10), c(0,0)))
  ))
  result <- check_valid(sf::st_sfc(p, crs = 5070))
  expect_equal(
    as.character(sf::st_geometry_type(result, by_geometry = FALSE)),
    "POLYGON"
  )

  # Reprojection
  p <- sf::st_polygon(list(rbind(c(0,0), c(10,0), c(10,10), c(0,10), c(0,0))))
  result <- check_valid(sf::st_sfc(p, crs = 5070), out_prj = sf::st_crs(4326))
  expect_equal(sf::st_crs(result), sf::st_crs(4326))

  # GEOMETRYCOLLECTION from repair
  coords <- rbind(c(0,0), c(5,5), c(10,0), c(10,10), c(5,5), c(0,10), c(0,0))
  result <- check_valid(sf::st_sfc(sf::st_polygon(list(coords)), crs = 5070))
  types <- as.character(sf::st_geometry_type(result, by_geometry = TRUE))
  expect_false(any(grepl("^GEOM", types)))

  # Residual mixed types (POLYGON + GEOMETRYCOLLECTION)
  p1 <- sf::st_polygon(list(rbind(c(0,0), c(10,0), c(10,10), c(0,10), c(0,0))))
  p2 <- sf::st_polygon(list(rbind(c(20,20), c(30,20), c(30,30), c(20,30), c(20,20))))
  gc <- sf::st_geometrycollection(list(
    sf::st_polygon(list(rbind(c(40,40), c(50,40), c(50,50), c(40,50), c(40,40)))),
    sf::st_linestring(rbind(c(40,40), c(45,45)))
  ))
  result <- check_valid(sf::st_sfc(p1, p2, gc, crs = 5070))
  types <- as.character(sf::st_geometry_type(result, by_geometry = TRUE))
  expect_false(any(grepl("^GEOM", types)))
})

test_that("fix_g_type handles empty geometry", {
  g <- sf::st_polygon()
  result <- hydroloom:::fix_g_type(g, type = "POLYGON", orig_type = "MULTIPOLYGON")
  expect_true(sf::st_is_empty(result))
})

test_that("fix_g_type casts simple geometry to orig_type", {
  g <- sf::st_polygon(list(rbind(c(0,0), c(1,0), c(1,1), c(0,1), c(0,0))))
  result <- hydroloom:::fix_g_type(g, type = "POLYGON", orig_type = "MULTIPOLYGON")
  expect_true(grepl("MULTI", sf::st_geometry_type(result)))
})

test_that("fix_g_type extracts matching type from GEOMETRYCOLLECTION", {
  p <- sf::st_polygon(list(rbind(c(0,0), c(1,0), c(1,1), c(0,1), c(0,0))))
  l <- sf::st_linestring(rbind(c(0,0), c(1,1)))
  gc <- sf::st_geometrycollection(list(p, l))

  result <- hydroloom:::fix_g_type(gc, type = "POLYGON", orig_type = "MULTIPOLYGON")
  types <- as.character(sf::st_geometry_type(result, by_geometry = TRUE))
  expect_false(any(grepl("LINE", types)))
})

test_that("get_empty returns correct empty geometries", {
  expect_true(sf::st_is_empty(hydroloom:::get_empty("POLYGON")))
  expect_true(sf::st_is_empty(hydroloom:::get_empty("MULTIPOLYGON")))
  expect_true(sf::st_is_empty(hydroloom:::get_empty("LINESTRING")))
  expect_true(sf::st_is_empty(hydroloom:::get_empty("MULTILINESTRING")))
  expect_true(sf::st_is_empty(hydroloom:::get_empty("POINT")))
  expect_true(sf::st_is_empty(hydroloom:::get_empty("MULTIPOINT")))
  expect_error(hydroloom:::get_empty("BOGUS"), "unexpected geometry type")
})
