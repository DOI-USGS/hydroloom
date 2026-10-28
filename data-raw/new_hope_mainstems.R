# Builds inst/extdata/new_hope_mainstems.csv: the comid -> mainstem_uri table
# for the New Hope sample, from the ref_rivers v3.2 NHDPlusV2 lookup cached in
# hydrogeofetch_data_dir(). Examples join this CSV to new_hope.gpkg by COMID,
# drop REACHCODE (it and mainstemid both map to aggregate_id) and set
# mainstemid to the URI, so the only sample data hydroloom's examples need
# ships with the package and the examples run on CRAN.
#
# Rebuild: Rscript data-raw/new_hope_mainstems.R

flowlines <- sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom"))

mainstems <- hydrogeofetch::add_mainstems(flowlines["COMID"])

lookup <- sf::st_drop_geometry(mainstems)

lookup <- lookup[!is.na(lookup$mainstem_uri), c("COMID", "mainstem_uri")]

names(lookup) <- c("comid", "mainstem_uri")

lookup <- lookup[order(lookup$comid), ]

utils::write.csv(lookup, "inst/extdata/new_hope_mainstems.csv", row.names = FALSE)
