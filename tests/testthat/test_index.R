test_that("index to waterbodies", {
  if (!requireNamespace("nhdplusTools", quietly = TRUE)) skip("Missing nhdplusTools")

  source(system.file("extdata/sample_data.R", package = "nhdplusTools"))

  wb <- sf::read_sf(sample_data, "NHDWaterbody")
  gage <- sf::read_sf(sample_data, "Gage")
  fline <- sf::read_sf(sample_data, "NHDFlowline_Network")

  gage_l <- sf::st_drop_geometry(gage) |>
    dplyr::filter(!is.na(LonSite)) |>
    sf::st_as_sf(coords = c("LonSite", "LatSite"), crs = 4326)

  gage_l <- dplyr::select(gage_l, hy_locid = SOURCE_FEA)

  expect_warning(match <- index_points_to_waterbodies(wb, gage_l),
    "st_transform points to match waterbodies")

  expect_equal(match[18, ]$near_wb_COMID, match[18, ]$in_wb_COMID)

  expect_equal(match[13, ]$near_wb_COMID, 13293262)

  gage_l <- sf::st_transform(gage_l, 5070)
  wb_l <- sf::st_transform(dplyr::select(wb, COMID), 5070)

  match <- index_points_to_waterbodies(wb_l, gage_l, search_radius = units::set_units(50, "m"))

  expect_true(is.na(match[13, ]$near_wb_COMID))

  expect_true(is.na(match[13, ]$near_wb_dist))

  match <- index_points_to_waterbodies(wb_l, gage_l, search_radius = units::set_units(200, "m"))

  expect_equal(match[13, ]$near_wb_dist, 164, tolerance = 1)

  match <- index_points_to_waterbodies(wb_l, gage_l, flines = fline,
    search_radius = units::set_units(200, "m"))

  gage_l <- cbind(gage_l, match)

  # waterbody without flowline
  expect_true(is.na(match[18, ]$outlet_fline_COMID))

  # point near waterbody
  expect_equal(match[7, ]$outlet_fline_COMID, 13294312)

  sf::st_geometry(wb_l)[[1]] <- sf::st_multipolygon(
    list(sf::st_geometry(wb_l)[[1]][[1]], sf::st_geometry(wb_l)[[1]][[1]]))

  expect_error(match <- index_points_to_waterbodies(wb_l, gage_l, search_radius = units::set_units(50, "m")),
    "Multipart geometries not supported.")
})

sr <- units::set_units(0.1, "degrees")

if (requireNamespace("nhdplusTools", quietly = TRUE)) {
  source(system.file("extdata", "sample_flines.R", package = "nhdplusTools"))

  sample_flines <- sf::st_cast(sample_flines, "LINESTRING", warn = FALSE)
}

test_that("point indexing to nearest existing node works as expected", {

  if (!requireNamespace("nhdplusTools", quietly = TRUE)) skip("Missing nhdplusTools")
  if (!requireNamespace("lwgeom", quietly = TRUE)) skip("Missing lwgeom")

  flines_in <- sample_flines

  flines_in <- sf::st_transform(flines_in, 4269)

  point <- sf::st_sfc(sf::st_point(c(-76.86876, 39.49345)), crs = 4269)

  expect_equal(index_points_to_lines(flines_in, point),
    data.frame(point_id = 1,
      COMID = 11688298,
      REACHCODE = "02060003000579",
      REACHCODE_measure = 34.6,
      offset = 0.000348), tolerance = 0.01)

  expect_equal(index_points_to_lines(flines_in, point)$REACHCODE,
    index_points_to_lines(sf::st_transform(flines_in, 5070),
      sf::st_transform(point, 5070))$REACHCODE)

  expect_equal(index_points_to_lines(sf::st_transform(flines_in, 5070),
    sf::st_transform(point, 5070)),
  data.frame(point_id = 1,
    COMID = 11688298,
    REACHCODE = "02060003000579",
    REACHCODE_measure = 33.8,
    offset = 30.27), tolerance = 0.01)

  expect_equal(nrow(index_points_to_lines(flines_in, point, search_radius = sr,
    max_matches = 5)),
  5)

  suppressMessages(
    expect_equal(index_points_to_lines(flines_in, point, search_radius = sr,
      precision = 30),
    data.frame(point_id = 1,
      COMID = 11688298,
      REACHCODE = "02060003000579",
      REACHCODE_measure = 25.9,
      offset = 0.0000959), tolerance = 0.001))

  expect_equal(index_points_to_lines(dplyr::select(flines_in, -REACHCODE),
    point),
  data.frame(point_id = 1,
    COMID = 11688298,
    offset = 0.00034), tolerance = 0.001)

  point_w <- sf::st_sfc(sf::st_point(c(-76.86934, 39.49328)), crs = 4326)

  expect_warning(index_points_to_lines(flines_in, point_w,
    search_radius = sr),
  "crs of lines and points don't match. attempting st_transform of lines")

  names(flines_in)[1] <- "broken"
  expect_error(index_points_to_lines(flines_in, point, search_radius = sr),
    "index_points_to_lines requires id hydroloom attributes.")
})

test_that("point indexing works without measures", {

  if (!requireNamespace("nhdplusTools", quietly = TRUE)) skip("Missing nhdplusTools")

  flines_in <- sample_flines

  flines_in <- sf::st_cast(sf::st_transform(flines_in, 4269),
    "LINESTRING", warn = FALSE)

  flines_in <- dplyr::select(flines_in, -FromMeas, -ToMeas)

  point <- sf::st_sfc(sf::st_point(c(-76.86876, 39.49345)), crs = 4269)

  expect_equal(index_points_to_lines(flines_in, point),
    data.frame(point_id = 1,
      COMID = 11688298,
      REACHCODE = "02060003000579",
      offset = 0.000348), tolerance = 0.01)
})

test_that("point indexing to for multiple points works", {

  if (!requireNamespace("nhdplusTools", quietly = TRUE)) skip("Missing nhdplusTools")
  if (!requireNamespace("lwgeom", quietly = TRUE)) skip("Missing lwgeom")
  flines_in <- sample_flines

  flines_in <- sf::st_transform(flines_in, 4269)

  point <- sf::st_sfc(list(sf::st_point(c(-76.86934, 39.49328)),
    sf::st_point(c(-76.91711, 39.40884)),
    sf::st_point(c(-76.88081, 39.36354))), crs = 4269)

  expect_equal(index_points_to_lines(flines_in, point, search_radius = sr),
    data.frame(point_id = c(1, 2, 3),
      COMID = c(11688298, 11688808, 11688980),
      REACHCODE = c("02060003000579",
        "02060003000519",
        "02060003000253"),
      REACHCODE_measure = c(0, 53.58737, 75.37795),
      offset = c(0.00006026811,
        0.00056414104,
        0.00031029699)), tolerance = 1e-2)

  expect_equal(index_points_to_lines(flines_in, point, search_radius = sr,
    precision = 5),
  data.frame(point_id = c(1, 2, 3),
    COMID = c(11688298, 11688808, 11688980),
    REACHCODE = c("02060003000579",
      "02060003000519",
      "02060003000253"),
    REACHCODE_measure = c(2.1599, 50.52674, 77.40798),
    offset = c(0.0000602681,
      0.0002523808,
      0.0001566810)), tolerance = 1e-2)

  matches <- index_points_to_lines(flines_in, point, search_radius = sr, max_matches = 10)
  expect_true("point_id" %in% names(matches))

  matches2 <- index_points_to_lines(flines_in, point, search_radius = sr,
    precision = 50, max_matches = 10)

  expect_equal(nrow(matches), nrow(matches2))

  expect_true(all(matches2$REACHCODE %in% matches$REACHCODE))

  expect_equal(index_points_to_lines(flines_in, point,
    search_radius = units::set_units(0.2, "degrees"),
    ids = c(11689926, 11690110, 11688990))$COMID,
  c(11689926L, 11690110L, 11688990L))

  # check that a large search radius still works
  expect_equal(suppressWarnings(index_points_to_lines(flines_in, point,
    search_radius = units::set_units(5, "degrees"),
    ids = c(11689926, 11690110, 11688990))$COMID),
  c(11689926L, 11690110L, 11688990L))

  expect_error(index_points_to_lines(flines_in, point,
    search_radius = units::set_units(0.2, "degrees"),
    ids = c(11689926, 11690110, 11688992)),
  "not all ids are in the id field of x")

  expect_error(index_points_to_lines(flines_in, point,
    search_radius = units::set_units(0.2, "degrees"),
    ids = c(11689926, 11690110)),
  "ids input must be 1:1 with points")

})

test_that("multipart indexing", {

  points <- sf::read_sf(list.files(pattern = "*flowline_index_reprex.gpkg",
    recursive = TRUE, full.names = TRUE), "sites")
  lines <- sf::read_sf(list.files(pattern = "*flowline_index_reprex.gpkg",
    recursive = TRUE, full.names = TRUE), "reaches")

  warn <- capture_warnings(index <- index_points_to_lines(lines, points,
    search_radius = 500))

  expect_true(all(c("Attempting to combine multipart lines into single part lines. Check results!!",
    "search_radius units not set, trying units of points CRS.")
  %in% warn))

  expect_true(all(index$COMID == 51664))

})

test_that("no duplicates when using precision", {
  # https://github.com/DOI-USGS/hydroloom/issues/11

  # Define points
  locs <- dplyr::tibble(id = c(1, 2),
    lon = c(-83.87865, -83.87975),
    lat = c(35.60989, 35.60963))
  points <- sf::st_as_sf(locs, coords = c("lon", "lat"), crs = 4326) |>
    sf::st_transform(5070)
  flines_aoi <- sf::st_buffer(points, dist = units::as_units(5000, "m")) |>
    sf::st_bbox() |>
    sf::st_as_sfc()

  # Define objects that we'd pass to get_flowline_index
  search_rad <- units::as_units(500, "m")
  max_match <- 3
  precision <- 10
  flines <- sf::read_sf(list.files(pattern = "index_precision.gpkg",
    full.names = TRUE, recursive = TRUE))

  check <- index_points_to_lines(x = flines, points = points,
    search_radius = search_rad,
    max_matches = max_match,
    precision = precision) |>
    dplyr::filter(point_id == 2)

  expect_equal(length(unique(check$comid)), 3)
})

test_that("disambiguate", {

  if (!requireNamespace("nhdplusTools", quietly = TRUE)) skip("Missing nhdplusTools")

  source(system.file("extdata", "sample_flines.R", package = "nhdplusTools"))

  points <- sf::st_sfc(list(sf::st_point(c(-76.86934, 39.49328)),
    sf::st_point(c(-76.91711, 39.40884)),
    sf::st_point(c(-76.88081, 39.36354))),
  crs = 4326)

  hydro_location <- sf::st_sf(point_id = c(1, 2, 3),
    geom = points,
    totda = c(23.6, 7.3, 427.9),
    nameid = c("Patapsco", "", "Falls Run River"))

  indexes <- index_points_to_lines(sample_flines,
    hydro_location,
    search_radius = units::set_units(0.2, "degrees"),
    max_matches = 10)

  result <- disambiguate_indexes(indexes,
    dplyr::select(sample_flines, COMID, TotDASqKM),
    dplyr::select(hydro_location, point_id, totda))

  expect_equal(nrow(result), 3)

  expect_equal(names(result),
    c("point_id", "COMID", "REACHCODE", "REACHCODE_measure", "offset"))

  result <- disambiguate_indexes(indexes,
    hy(dplyr::select(sample_flines, COMID, TotDASqKM)),
    dplyr::select(hydro_location, point_id, totda))

  expect_equal(nrow(result), 3)

  expect_equal(names(result),
    c("point_id", "id", "aggregate_id",
      "aggregate_id_measure", "offset"))

  result <- disambiguate_indexes(indexes,
    dplyr::select(sample_flines, COMID, GNIS_NAME),
    dplyr::select(hydro_location, point_id, nameid))

  expect_equal(nrow(result[result$point_id == 1, ]), 3)

  expect_equal(nrow(result[result$point_id == 2, ]), 10)

  expect_equal(nrow(result[result$point_id == 3, ]), 1)

  expect_error(disambiguate_indexes(indexes,
    dplyr::select(sample_flines, COMID, GNIS_NAME),
    hydro_location),
  "flowpath and hydrolocation must be two-column data.frames")

  expect_error(disambiguate_indexes(indexes,
    dplyr::select(sample_flines, COMID, GNIS_NAME),
    dplyr::select(hydro_location, point_id, totda)),
  "flowpath and hydrolocation metrics must both be numeric or character")
})

test_that("3dhp", {
  if (!requireNamespace("nhdplusTools", quietly = TRUE)) skip("Missing nhdplusTools")

  try(source(system.file("extdata", "3dhp_yahara_flowlines.R", package = "nhdplusTools")))

  if (!grepl("geojson", sample_3dhp_data)) skip("data not available?")

  threedhp_data <- sf::read_sf(sample_3dhp_data)

  suppressWarnings(threedhp_data <- select(threedhp_data, id3dhp, mainstemid) |>
    sf::st_transform(5070) |>
    sf::st_cast("LINESTRING"))

  expect_message(threedhp_data <- add_measures(threedhp_data), "no toid found")

  point <- sf::st_as_sfc("POINT (-89.3525 43.20889)", crs = 4326) |>
    sf::st_transform(5070)

  expect_equal(index_points_to_lines(threedhp_data, point),
    structure(list(point_id = 1L, id3dhp = "9Q1QM",
      mainstemid = "https://geoconnex.us/ref/mainstems/377002",
      mainstemid_measure = 80.7, offset = 16.825),
    row.names = c(NA, -1L),
    class = "data.frame"), tolerance = 0.1)
})
