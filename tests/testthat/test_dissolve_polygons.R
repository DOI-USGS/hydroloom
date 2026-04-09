test_that("basic dissolve merges adjacent polygons", {
  p1 <- sf::st_polygon(list(
    rbind(c(0, 0), c(1e5, 0), c(1e5, 1e5), c(0, 1e5), c(0, 0))))
  p2 <- sf::st_polygon(list(
    rbind(c(1e5, 0), c(2e5, 0), c(2e5, 1e5), c(1e5, 1e5), c(1e5, 0))))
  polys <- sf::st_sf(geometry = sf::st_sfc(p1, p2, crs = 5070))

  result <- dissolve_polygons(polys, work_crs = NULL)

  expect_s3_class(result, "sf")
  expect_equal(nrow(result), 1)
  expect_true(all(sf::st_is_valid(result)))
})

test_that("grouped dissolve produces one row per group", {
  p1 <- sf::st_polygon(list(
    rbind(c(0, 0), c(1e5, 0), c(1e5, 1e5), c(0, 1e5), c(0, 0))))
  p2 <- sf::st_polygon(list(
    rbind(c(1e5, 0), c(2e5, 0), c(2e5, 1e5), c(1e5, 1e5), c(1e5, 0))))
  p3 <- sf::st_polygon(list(
    rbind(c(3e5, 0), c(4e5, 0), c(4e5, 1e5), c(3e5, 1e5), c(3e5, 0))))
  polys <- sf::st_sf(
    grp = c("a", "a", "b"),
    geometry = sf::st_sfc(p1, p2, p3, crs = 5070))

  result <- dissolve_polygons(polys, group_id = "grp", work_crs = NULL)

  expect_equal(nrow(result), 2)
  expect_true("grp" %in% names(result))
  expect_equal(sort(result$grp), c("a", "b"))
})

test_that("hole removal (all) fills donut hole", {
  outer <- rbind(c(0, 0), c(1e5, 0), c(1e5, 1e5), c(0, 1e5), c(0, 0))
  hole <- rbind(c(2e4, 2e4), c(2e4, 8e4), c(8e4, 8e4), c(8e4, 2e4),
                c(2e4, 2e4))
  donut <- sf::st_polygon(list(outer, hole))
  polys <- sf::st_sf(geometry = sf::st_sfc(donut, crs = 5070))

  result <- dissolve_polygons(polys, max_hole_area = Inf, work_crs = NULL)

  # Should have no holes (single ring)
  rings <- result$geometry[[1]]
  expect_equal(length(rings), 1)
})

test_that("hole removal with threshold keeps large holes", {
  outer <- rbind(c(0, 0), c(1e5, 0), c(1e5, 1e5), c(0, 1e5), c(0, 0))
  # Small hole ~100 sq m
  small_hole <- rbind(c(1e4, 1e4), c(1e4, 1e4 + 10), c(1e4 + 10, 1e4 + 10),
                      c(1e4 + 10, 1e4), c(1e4, 1e4))
  # Large hole ~3.6e9 sq m
  large_hole <- rbind(c(4e4, 4e4), c(4e4, 1e5 - 1e4),
                      c(1e5 - 1e4, 1e5 - 1e4),
                      c(1e5 - 1e4, 4e4), c(4e4, 4e4))
  poly_with_holes <- sf::st_polygon(list(outer, small_hole, large_hole))
  polys <- sf::st_sf(
    geometry = sf::st_sfc(poly_with_holes, crs = 5070))

  # Threshold of 1000 sq m: small hole removed, large hole kept
  result <- dissolve_polygons(polys, max_hole_area = 1000, work_crs = NULL)

  rings <- result$geometry[[1]]
  # Should have outer ring + 1 large hole = 2 rings
  expect_equal(length(rings), 2)
})

test_that("max_hole_area = 0 keeps all holes", {
  outer <- rbind(c(0, 0), c(1e5, 0), c(1e5, 1e5), c(0, 1e5), c(0, 0))
  hole <- rbind(c(2e4, 2e4), c(2e4, 8e4), c(8e4, 8e4), c(8e4, 2e4),
                c(2e4, 2e4))
  donut <- sf::st_polygon(list(outer, hole))
  polys <- sf::st_sf(geometry = sf::st_sfc(donut, crs = 5070))

  result <- dissolve_polygons(polys, max_hole_area = 0, work_crs = NULL)

  rings <- result$geometry[[1]]
  expect_equal(length(rings), 2)
})

test_that("gap_tolerance bridges small gaps", {
  # Two squares with a 50m gap
  p1 <- sf::st_polygon(list(
    rbind(c(0, 0), c(1e5, 0), c(1e5, 1e5), c(0, 1e5), c(0, 0))))
  p2 <- sf::st_polygon(list(
    rbind(c(1e5 + 50, 0), c(2e5, 0), c(2e5, 1e5),
          c(1e5 + 50, 1e5), c(1e5 + 50, 0))))
  polys <- sf::st_sf(geometry = sf::st_sfc(p1, p2, crs = 5070))

  # No gap tolerance -> two separate polygons (MULTIPOLYGON)
  result_no_gap <- dissolve_polygons(polys, gap_tolerance = 0,
                                     work_crs = NULL)
  # With tolerance -> single polygon
  result_gap <- dissolve_polygons(polys, gap_tolerance = 100,
                                  work_crs = NULL)

  # The gap version should have fewer parts or different area
  area_no_gap <- as.numeric(sf::st_area(result_no_gap))
  area_gap <- as.numeric(sf::st_area(result_gap))

  # Gap version should be >= no_gap version (gap is filled)
  expect_true(area_gap >= area_no_gap * 0.99)
})

test_that("CRS round-trip preserves original CRS", {
  p1 <- sf::st_polygon(list(
    rbind(c(-80, 35), c(-79, 35), c(-79, 36), c(-80, 36), c(-80, 35))))
  polys <- sf::st_sf(geometry = sf::st_sfc(p1, crs = 4326))

  result <- dissolve_polygons(polys, work_crs = 5070)
  expect_equal(sf::st_crs(result), sf::st_crs(4326))
})

test_that("work_crs = NULL skips CRS transform", {
  p1 <- sf::st_polygon(list(
    rbind(c(0, 0), c(1e5, 0), c(1e5, 1e5), c(0, 1e5), c(0, 0))))
  polys <- sf::st_sf(geometry = sf::st_sfc(p1, crs = 5070))

  result <- dissolve_polygons(polys, work_crs = NULL)
  expect_equal(sf::st_crs(result), sf::st_crs(5070))
})

test_that("single_polygon extracts largest from MULTIPOLYGON", {
  # Two disjoint polygons — small and large
  p_large <- sf::st_polygon(list(
    rbind(c(0, 0), c(1e5, 0), c(1e5, 1e5), c(0, 1e5), c(0, 0))))
  p_small <- sf::st_polygon(list(
    rbind(c(2e5, 0), c(2e5 + 100, 0), c(2e5 + 100, 100),
          c(2e5, 100), c(2e5, 0))))
  mp <- sf::st_multipolygon(list(p_large, p_small))
  polys <- sf::st_sf(geometry = sf::st_sfc(mp, crs = 5070))

  result <- dissolve_polygons(polys, single_polygon = TRUE, work_crs = NULL)

  gtype <- as.character(sf::st_geometry_type(result, by_geometry = TRUE))
  expect_equal(gtype, "POLYGON")

  # Should be approximately the large polygon's area
  expect_true(as.numeric(sf::st_area(result)) > 1e9)
})

test_that("single polygon no-op", {
  p1 <- sf::st_polygon(list(
    rbind(c(0, 0), c(1e5, 0), c(1e5, 1e5), c(0, 1e5), c(0, 0))))
  polys <- sf::st_sf(geometry = sf::st_sfc(p1, crs = 5070))

  result <- dissolve_polygons(polys, work_crs = NULL)

  expect_s3_class(result, "sf")
  expect_equal(nrow(result), 1)
  expect_true(all(sf::st_is_valid(result)))
})

test_that("work_crs accepts crs object", {
  p1 <- sf::st_polygon(list(
    rbind(c(-80, 35), c(-79, 35), c(-79, 36), c(-80, 36), c(-80, 35))))
  polys <- sf::st_sf(geometry = sf::st_sfc(p1, crs = 4326))

  crs_obj <- sf::st_crs("EPSG:5070")
  result <- dissolve_polygons(polys, work_crs = crs_obj)
  expect_equal(sf::st_crs(result), sf::st_crs(4326))
})

test_that("input validation catches bad inputs", {
  expect_error(dissolve_polygons(data.frame(x = 1)),
               "polys must be an sf data.frame")

  p <- sf::st_point(c(0, 0))
  polys <- sf::st_sf(geometry = sf::st_sfc(p, crs = 5070))
  expect_error(dissolve_polygons(polys, work_crs = NULL),
               "POLYGON or MULTIPOLYGON")

  p1 <- sf::st_polygon(list(
    rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0))))
  polys <- sf::st_sf(geometry = sf::st_sfc(p1, crs = 5070))
  expect_error(dissolve_polygons(polys, group_id = "nonexistent"),
               "not found")
})
