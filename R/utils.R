get_hyg <- function(x, add, id = "id") {
  if(add && inherits(x, "sf")) {
    select(x, all_of(id))
  } else {
    NULL
  }
}

put_hyg <- function(x, hy_g) {
  if(!is.null(hy_g)) {
    orig_names <- attr(x, "orig_names")
    x <- sf::st_sf(left_join(x, hy_g, by = id))
    attr(x, "orig_names") <- orig_names

    if(!inherits(x, "hy")) {
      class(x) <- c("hy", class(x))
    }
  }
  x
}

# can replace this with something better?
replace_na <- function(x, y) {
  x[is.na(x)] <- y
  x
}

#' make spatial inputs compatible
#' @description makes sf1 compatible with sf2 by projecting into
#' the projection of 2 and ensuring that the geometry columns are the
#' same name.
#' @param sf1 sf data.frame
#' @param sf2 sf data.frame
#' @export
#' @examples
#'
#' x <- sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom"))
#'
#' one <- dplyr::select(x)
#' two <- sf::st_transform(one, 5070)
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
#' @param x data.frame that may contain a geometry column
#' @return data.frame without geometry column
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

#' @title Get line Node
#' @description Given one or more lines, returns a particular node from
#' the line.
#' @param x sf sf data.frame with one or more LINESTRING features
#' @param position character either "start" or "end"
#' @export
#' @return sf data.frame containing requested nodes
#' @examples
#'
#' x <- sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom"))
#'
#' start <- get_node(x, "start")
#' end <- get_node(x, "end")
#'
#' plot(sf::st_zm(sf::st_geometry(x)),
#'      lwd = x$StreamOrde, col = "blue")
#' plot(sf::st_geometry(start), add = TRUE)
#'
#' plot(sf::st_zm(sf::st_geometry(x)),
#'      lwd = x$StreamOrde, col = "blue")
#' plot(sf::st_geometry(end), add = TRUE)
#'
get_node <- function(x, position = "end") {
  in_crs <- st_crs(x)

  x <- x |>
    st_coordinates() |>
    as.data.frame()

  if("L2" %in% names(x)) {
    x <- group_by(x, .data$L2)
  } else {
    x <- group_by(x, .data$L1)
  }

  if(position == "end") {
    x <- filter(x, row_number() == n())
  } else if(position == "start") {
    x <- filter(x, row_number() == 1)
  }

  x <- select(ungroup(x), "X", "Y")

  st_as_sf(x, coords = c("X", "Y"), crs = in_crs)
}

#' Fix flow direction
#' @description If flowlines aren't digitized in the expected direction,
#' this will reorder the nodes so they are.
#' @param id integer The id of the flowline to check
#' @param network data.frame network compatible with \link{hydroloom_names}.
#' @param fn_list list containing named elements `flowline`, `network`, and `check_end`,
#' where `flowline` is the flowline to be checked and `network` the feature up or downstream
#' of the flowline to be checked, and `check_end` is `"start"` or `"end"` depending if the
#' `network` input is upstream (`"start"`) or downstream (`"end"`) of the flowline to be checked.
#' This option allows pre-compilation of pairs of features which may be useful for very large
#' numbers of flow direction checks.
#' @return a geometry for the feature that has been reversed if needed.
#' @export
#' @examples
#'
#' x <- sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom"))
#'
#' # We add a tocomid with prepare_nhdplus
#' x <- add_toids(hy(x))
#'
#' # Look at the end node of the 10th line.
#' (n1 <- get_node(x[10, ], position = "end"))
#'
#' # Break the geometry by reversing it.
#' sf::st_geometry(x)[10] <- sf::st_reverse(sf::st_geometry(x)[10])
#'
#' # Note that the end node is different now.
#' (n2 <- get_node(x[10, ], position = "end"))
#'
#' # Pass the broken geometry to fix_flowdir with the network for toCOMID
#' sf::st_geometry(x)[10] <- fix_flowdir(x$COMID[10], x)
#'
#' # Note that the geometry is now in the right order.
#' (n3 <- get_node(x[10, ], position = "end"))
#'
#' plot(sf::st_geometry(x)[10])
#' plot(n1, add = TRUE)
#' plot(n2, add = TRUE, col = "blue")
#' plot(n3, add = TRUE, cex = 2, col = "red")
#'
fix_flowdir <- function(id, network = NULL, fn_list = NULL) {

  if(!is.null(network))
    network <- hy(network)

  try({

    if(!is.null(fn_list)) {
      f <- fn_list$flowline

      check_line <- hy(fn_list$network)

      check_position <- fn_list$check_end
    } else {
      f <- network[network$id == id, ]

      if(is.na(f$toid) | f$toid == 0) {

        check_line <- network[network$toid == f$id, ][1, ]

        check_position <- "start"

      } else {

        check_line <- network[network$id == f$toid, ][1, ]

        check_position <- "end"

      }
    }

    suppressMessages(
      check_end <- st_join(get_node(f, position = check_position),
                           select(check_line, check_id = "id")))

    reverse <- is.na(check_end$check_id)

    if(reverse) {
      st_geometry(f)[reverse] <- st_reverse(st_geometry(f)[reverse])
    }

    return(st_geometry(f))
  })
}
