#' Add Path Length
#' @description Generates the main path length to a basin's terminal path.
#' @param x data.frame network compatible with \link{hydroloom_names}.
#' @details
#'
#' Required attributes: `id`, `toid`, `length_km`
#'
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

  orig_names <- attr(x, "orig_names")

  x <- add_pathlength(x)

  attr(x, "orig_names") <- orig_names
  if (!inherits(x, "hy")) class(x) <- c("hy", class(x))

  hy_reverse(x)
}

#' @name add_pathlength
#' @export
add_pathlength.hy <- function(x) {

  x <- classify_hy(x)
  if (!identical(hy_network_type(x), "hy")) return(add_pathlength(x))

  hy_dispatch_error("add_pathlength", "hy_topo", x,
    "Use add_toids() to build toid from fromnode/tonode, or hy(x, add_topo = TRUE).")
}

# TODO: support hy_node auto-convert via add_toids()
#' @name add_pathlength
#' @export
add_pathlength.hy_node <- function(x) {
  hy_dispatch_error("add_pathlength", "hy_topo", x,
    "Use add_toids() to convert fromnode/tonode to edge list.")
}

#' @name add_pathlength
#' @export
add_pathlength.hy_topo <- function(x) {

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

  x <- left_join(orig_order, x, by = id)

  classify_hy(x)
}
