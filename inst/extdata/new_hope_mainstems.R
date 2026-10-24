# The New Hope NHDPlusV2 sample flowlines with ref/mainstems URIs joined on
# as mainstemid, ready for hy(). new_hope_mainstems.csv is the comid to
# mainstem_uri lookup for the flowlines in new_hope.gpkg. REACHCODE is
# dropped so hy() maps mainstemid, not REACHCODE, to aggregate_id. The
# flowlines are cast to LINESTRING so get_subpath() can cut them.
#
# source(system.file("extdata", "new_hope_mainstems.R", package = "hydroloom"))
#
# net <- add_toids(hy(new_hope_mainstems))

new_hope_mainstems <- sf::read_sf(
  system.file("extdata", "new_hope.gpkg", package = "hydroloom"))

new_hope_mainstems_csv <- utils::read.csv(
  system.file("extdata", "new_hope_mainstems.csv", package = "hydroloom"))

new_hope_mainstems$mainstemid <- new_hope_mainstems_csv$mainstem_uri[
  match(new_hope_mainstems$COMID, new_hope_mainstems_csv$comid)]

new_hope_mainstems$REACHCODE <- NULL

new_hope_mainstems <- sf::st_cast(new_hope_mainstems, "LINESTRING",
  warn = FALSE)

rm(new_hope_mainstems_csv)
