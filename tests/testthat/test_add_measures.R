test_that("reproduce new_hope", {
  g <- sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom"))

  suppressWarnings(d <- select(g, COMID, REACHCODE) |>
    sf::st_cast("LINESTRING"))

  expect_message(d <- add_measures(d), "no toid found")

  # g[g$REACHCODE == "03030002000581",]
  #
  # g[g$COMID == 8893350, c("COMID", "REACHCODE", "FromMeas", "ToMeas"), drop = TRUE]
  # g[g$COMID == 8894160, c("COMID", "REACHCODE", "FromMeas", "ToMeas"), drop = TRUE]
  #
  # d[d$REACHCODE == "03030002000581",]
  #
  # d[d$COMID == 8893350, c("COMID", "REACHCODE", "aggregate_id_from_measure", "aggregate_id_to_measure"), drop = TRUE]
  # d[d$COMID == 8894160, c("COMID", "REACHCODE", "aggregate_id_from_measure", "aggregate_id_to_measure"), drop = TRUE]

  d <- d[match(d$COMID, g$COMID),]

  expect_equal(d$REACHCODE_from_measure, g$FromMeas, tolerance = 0.1)
  expect_equal(d$REACHCODE_to_measure, g$ToMeas, tolerance = 0.1)
})

test_that("mainstem", {

  if(!requireNamespace("nhdplusTools", quietly = TRUE)) skip("Missing nhdplusTools")

  try(source(system.file("extdata", "3dhp_yahara_flowlines.R", package = "nhdplusTools")))

  if(!grepl("geojson", sample_3dhp_data)) skip("data not available?")

  threedhp_data <- sf::read_sf(sample_3dhp_data)

  suppressWarnings(threedhp_data <- select(threedhp_data, id3dhp, mainstemid) |>
    sf::st_transform(5070) |>
    sf::st_cast("LINESTRING"))

  expect_message(threedhp_data <- add_measures(threedhp_data), "no toid found")

  expect_equal(threedhp_data$mainstemid_from_measure[threedhp_data$id3dhp == "CAF7T"], 0)

  expect_equal(threedhp_data$mainstemid_to_measure[threedhp_data$id3dhp == "CAF7T"], 2.86, tolerance = 0.01)

  expect_equal(threedhp_data$mainstemid_to_measure[threedhp_data$id3dhp == "1I1L0"], 100)

})
