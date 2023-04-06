#' make node topology from edge topology
#' @description creates a node topology table from an edge topology
#' @inheritParams add_levelpaths
#' @param add_div data.frame containing id and toid diverted paths to add.
#' Should have id and toid fields.
#' @param add logical if TRUE, node topology will be added to x in return.
#' @return data.frame containing id, fromnode, and tonode attributes or all
#' attributes provided with id, fromnode and tonode in the first three columns.
#' @export
#' @name make_node_topology
#' @examples
#' x <- sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom"))
#'
#' y <- dplyr::select(add_toids(x), -ToNode, -FromNode)
#'
#' y <- make_node_topology(y)
#'
#' # just the divergences which have unique fromids in x but don't in new hope.
#' div <- add_toids(dplyr::select(x, COMID, FromNode, ToNode),
#'                  return_dendritic = FALSE)
#' div <- div[div$toid %in%
#'            x$COMID[x$Divergence == 2],]
#'
#' y <- dplyr::select(add_toids(x), -ToNode, -FromNode)
#'
#' y <- make_node_topology(y, add_div = div)
#'
make_node_topology <- function(x, add_div = NULL, add = TRUE) {
  UseMethod("make_node_topology")
}

#' @name make_node_topology
#' @export
make_node_topology.data.frame <- function(x, add_div = NULL, add = TRUE) {

  x <- hy(x)

  x <- make_node_topology(x, add_div, add)

  hy_reverse(x)

}

#' @name make_node_topology
#' @export
make_node_topology.hy <- function(x, add_div = NULL, add = TRUE) {

  check_names(x, c(id, toid), "make_node_topology")

  hy_g <- get_hyg(x, add, id)

  x <- drop_geometry(x)

  if(length(unique(x$id)) != nrow(x)) stop("duplicate identifiers found")

  if(any(is.na(x$toid))) stop("NA toids found -- must be 0")
  if(!all(x$toid[x$toid != 0] %in% x$id)) stop("Not all non zero toids are in ids")
  if(any(c(fromnode, tonode) %in% names(x))) stop("fromnode or tonode already in data")

  order <- data.frame(id = x$id)

  x <- sort_network(x)

  head_count <- nrow(x)
  head_nodes <- seq_len(head_count)

  x$fromnode <- head_nodes

  x <- left_join(x, select(x, all_of(c(id = id, tonode = fromnode))),
                 by = c(toid = id))

  outlets <- x$toid == get_outlet_value(x)

  x$tonode[outlets] <- seq(max(x$tonode, na.rm = TRUE) + 1,
                           max(x$tonode, na.rm = TRUE) + sum(outlets))

  if(!is.null(add_div)) {
    # we need to get the node the divergences upstream neighbor goes to
    # first get the new outlet nodes for our old ids
    add_div <- drop_geometry(add_div[, 1:2])
    names(add_div)[1:2] <- c(id, toid)
    add_div <- left_join(select(add_div, all_of(c(id, toid))),
                         select(x, all_of(c(id, tonode))), by = id)

    # now join upstream renaming the tonode to fromnode
    x <- left_join(x, select(add_div, all_of(c(toid = toid, new_fromnode = tonode))),
                   by = c(id = toid))

    x <- mutate(x, fromnode = ifelse(!is.na(.data$new_fromnode),
                                     .data$new_fromnode, .data$fromnode))

    x <- select(x, -"new_fromnode")

    x <- distinct(x)
  }

  if(add) {

    if(!is.null(hy_g)) {
      x <- sf::st_sf(left_join(x, hy_g, by = id))
    }

    x <- x[ , c(id, toid, fromnode, tonode,
                names(x)[!names(x) %in% c(id, toid, fromnode, tonode)])]

    x

  } else {

    x <- select(x, all_of(c(id, fromnode, tonode)))

    x

  }
}
