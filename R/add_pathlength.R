#' Add Path Length
#' @description Generates the main path length to a basin's terminal path.
#'
#' Requires id, toid, and length_km hydroloom compatible attributes.
#'
#' @inheritParams add_levelpaths
#' @name add_pathlength
#' @export
#' @returns data.frame containing pathlength_km
#' @examples
#' x <- sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom"))
#'
#' x <- add_toids(x)
#'
#' x <- add_pathlength(x)
#'
#' plot(x["Pathlength"])
#'
add_pathlength <- function(x) {

  UseMethod("add_pathlength")

}

#' @name add_pathlength
#' @export
add_pathlength.data.frame <- function(x) {

  x <- hy(x)

  x <- add_pathlength(x)

  hy_reverse(x)
}

#' @name add_pathlength
#' @export
add_pathlength.hy <- function(x) {

  check_names(x, c(id, toid, length_km), "add_pathlength")

  orig_order <- select(x, id)

  x <- sort_network(st_drop_geometry(x))[rev(seq_len(nrow(x))), ]

  pathlength_km <- rep(0, nrow(x))
  length_km <- x$length_km
  toid <- x$toid

  toids <- match(x$toid, x$id)

  for (i in seq_along(toid)) {
    tid <- toid[i]
    if (tid != 0) {

      pathlength_km[i] <- length_km[toids[i]] + pathlength_km[toids[i]]

    }
    if (i %% 10000 == 0) message(i)
  }

  x$pathlength_km <- pathlength_km

  left_join(orig_order, x, by = id)
}
