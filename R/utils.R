#' make spatial inputs compatible
#' @description makes sf1 compatible with sf2 by projecting into
#' the projection of 2 and ensuring that the geometry columns are the
#' same name.
#' @param sf1 sf data.frame
#' @param sf2 sf data.frame
#' @importFrom sf st_transform st_crs
#' @export
#' @examples
#'
#' source(system.file("extdata", "sample_flines.R", package = "nhdplusTools"))
#'
#' (one <- dplyr::select(sample_flines))
#' (two <- sf::st_transform(one, 5070))
#'
#' attr(one, "sf_column") <- "geotest"
#' names(one)[names(one) == "geom"] <- "geotest"
#'
#' st_compatibalize(one, two)
#'
st_compatibalize <- function(sf1, sf2) {

  sf1 <- st_transform(sf1, st_crs(sf2))

  rename_geometry(sf1, attr(sf2, "sf_column"))

}

#' rename_geometry
#' @description correctly renames the geometry column
#' of a sf object.
#' @param g sf data.table
#' @param name character name to be used for geometry
#' @export
#' @examples
#'
#' (g <- sf::st_sf(a=3, geo = sf::st_sfc(sf::st_point(1:2))))
#' rename_geometry(g, "geometry")
#'
rename_geometry <- function(g, name){
  current = attr(g, "sf_column")

  names(g)[names(g)==current] = name

  attr(g, "sf_column") <- name

  g
}

#' drop_geometry
#' @description drops geometry if present, does nothing otherwise.
#' @param x data.frame
#' @return data.frame without geometry column
#' @importFrom sf st_drop_geometry
#' @export
#' @examples
#'
#' (g <- sf::st_sf(a=3, geo = sf::st_sfc(sf::st_point(1:2))))
#' drop_geometry(g)
#'
drop_geometry <- function(x) {
  if("sf" %in% class(x)) {
    st_drop_geometry(x)
  } else {
    x
  }
}
