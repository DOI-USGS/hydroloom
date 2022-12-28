#' @title add Streamorder
#' @description Adds a strahler stream order.
#' Algorithm: If more than one upstream flowpath has an order equal to the
#' maximum upstream order then the downstream flowpath is assigned the maximum
#' upstream order plus one. Otherwise it is assigned the max upstream order.
#' @inheritParams add_levelpaths
#' @return ddata.frame containing added `stream_order` attribute.
#' @importFrom dplyr left_join select
#' @export
#' @name add_streamorder
#' @examples
#' x <- sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom"))
#'
#' x <- add_toids(x)
#'
#' x <- add_streamorder(x)
#'
#' plot(sf::st_geometry(x), lwd = x$order, col = "blue")
#'
add_streamorder <- function(x, status = TRUE) {
  UseMethod("add_streamorder")
}

#' @name add_streamorder
#' @export
add_streamorder.data.frame <- function(x, status = TRUE) {
  x <- hy(x)

  x <- add_streamorder(x, status)

  hy_reverse(x)
}

add_streamorder.hy <- function(x, status = TRUE) {

  check_names(x, c(id, toid), "add_streamorder")

  net <- select(drop_geometry(x), all_of(c(id, toid)))

  net$toid <- replace_na(net$toid, 0)

  # First sort so we have upstream first and outlets last.
  net <- sort_network(net)

  # Now generate a working index against the sorted data.
  index_ids <- make_index_ids(net)

  # Find fromids from the working index.
  # columns of the included matrix correspond to the index ids.
  # rows of the matrix correspond to adjacent upstream ids
  froms <- make_fromids(index_ids)

  # will fill in order as we go in this
  order <- rep(1, nrow(net))

  for(i in seq_len(nrow(net))) {

    # nothing to do if nothing upstream
    if((l <- froms$lengths[i]) > 0) {

      # these are the upstream orders
      orders <- order[froms$froms[1:l,i]]

      # Need the max upstream order for this work
      m <- max(orders)

      # the core stream order algorithm.
      # if more than one upstream order is the same
      # as the max upstream order, increment by one.
      # otherwise use the max upstream order.
      if(length(orders[orders == m]) > 1) {
        order[i] <- m + 1
      } else {
        order[i] <- m
      }

    }
  }

  left_join(x,
            bind_cols(id = net$id, tibble(stream_order = order)),
            by = "id")

}

#' @title add Streamlevel
#' @description Applies a topological sort and calculates stream level.
#' Algorithm: Terminal level paths are assigned level 1 (see note 1).
#' Paths that terminate at a level 1 are assigned level 2. This pattern is
#' repeated until no paths remain.
#'
#' If a TRUE/FALSE coastal attribute is included, coastal terminal paths
#' begin at 1 and internal terminal paths begin at 4 as is implemented by
#' the NHD stream leveling rules.
#' @inheritParams add_levelpaths
#' @param coastal_flag character attribute name containing a logical flag
#' indicating if a given terminal catchment flows to the coast of is an
#' inland sink. If no coastal flag is included, all terminal paths are
#' assumed to be coastal.
#'
#' @return data,frame containing added `stream_level` attribute
#' @export
#' @name add_streamlevel
#' @examples
#' x <- sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom"))
#'
#' x <- add_toids(x)
#'
#' y <- add_streamlevel(x)
#'
#' plot(sf::st_geometry(y), lwd = y$streamlevel, col = "blue")
#'
#' x$coastal <- rep(FALSE, nrow(x))
#'
#' y <- add_streamlevel(x, coastal = "coastal")
#'
#' unique(y$streamlevel)
#'
#' x$coastal[!x$Hydroseq == min(x$Hydroseq)] <- TRUE
#'
#' y <- add_streamlevel(x)
#'
#' unique(y$streamlevel)
#'
add_streamlevel <- function(x, coastal = NULL) {
  UseMethod("add_streamlevel")
}

#' @name add_streamlevel
#' @export
add_streamlevel.data.frame <- function(x, coastal = NULL) {
  x <- hy(x)

  x <- add_streamlevel(x, coastal)

  hy_reverse(x)
}

#' @name add_streamlevel
#' @export
add_streamlevel.hy <- function(x, coastal = NULL) {

  check_names(x, c(levelpath, dn_levelpath), "add_streamlevel")

  x$dn_levelpath <- replace_na(x$dn_levelpath, 0)

  net <- select(drop_geometry(x), any_of(c(levelpath, dn_levelpath, coastal)))

  l <- net |>
    filter(.data$levelpath != .data$dn_levelpath) |>
    rename(id = "levelpath",
           toid = "dn_levelpath") |>
    sort_network() |>
    distinct()

  l <- l[nrow(l):1, ]

  l$level <- rep(0, nrow(l))

  l$level[!l$toid %in% l$id] <- 1

  if(!is.null(coastal) && coastal %in% names(l)) {
    l$level[l$level == 1 & !l[[coastal]]] <- 4
  }

  id <- l$id
  toid <- l$toid
  level <- l$level

  toids <- match(toid, id)

  # walk the network from bottom path up
  for(i in seq_len(length(id))) {
    if(!is.na(toids[i])) {

      level[i] <- # level at current
        level[toids[i]] + 1 # level of downstream + 1

    }
  }

  left_join(x, tibble(levelpath = id, stream_level = level), by = "levelpath")

}
