#' @title Make Node Topology from Edge Topology
#' @description creates a node topology table from an edge topology
#' @inheritParams add_levelpaths
#' @param add_div data.frame of logical containing id and toid diverted paths to add.
#' Should have id and toid fields. If TRUE, the network will be interpreted as
#' a directed acyclic graph with downstream divergences included in the edge
#' topology.
#' @param add logical if TRUE, node topology will be added to x in return.
#' @return data.frame containing id, fromnode, and tonode attributes or all
#' attributes provided with id, fromnode and tonode in the first three columns.
#'
#' If `add_div` is TRUE, will also add a `divergence` attribute where the
#' provided diverted paths are assigned value 2, existing main paths that
#' eminate from a divergence are assigned value 1, and all other paths
#' are assigned value 0.
#'
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

  if(inherits(x, "hy")) {
    hy_reverse(x)
  } else {
    x
  }

}

#' @name make_node_topology
#' @export
make_node_topology.hy <- function(x, add_div = NULL, add = TRUE) {

  check_names(x, c(id, toid), "make_node_topology")

  hy_g <- get_hyg(x, add, id)

  x <- drop_geometry(x)

  if(length(unique(x$id)) != nrow(x)) {
    if(!isTRUE(add_div))
      stop("duplicate identifiers found and 'add_div' is not TRUE")

    return(make_nondendritic_topology(x))

  } else {

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

      div2 <- add_div$toid
      div1 <- x$toid[x$id %in% add_div$id]

      # now join upstream renaming the tonode to fromnode
      x <- left_join(x, select(add_div, all_of(c(toid = toid, new_fromnode = tonode))),
                     by = c(id = toid))

      x <- mutate(x, fromnode = ifelse(!is.na(.data$new_fromnode),
                                       .data$new_fromnode, .data$fromnode))

      x <- select(x, -"new_fromnode")

      x <- distinct(x)

      x <- mutate(x, divergence = ifelse(id %in% div2, 2, ifelse(id %in% div1, 1, 0)))
    }
  }
  if(add & !isTRUE(add_div)) {

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

make_nondendritic_topology <- function(x) {

  # First create a unique node id that groups on sets of downstream ids
  n <- select(x, all_of(c(fromid = id, toid))) |>
    filter(!is.na(.data$fromid) & !is.na(.data$toid)) |>
    group_by(.data$fromid) |>
    mutate(node_id = paste(toid, collapse = "-")) |>
    ungroup()

  hw <- unique(n$fromid[!n$fromid %in% n$toid])
  tl <- unique(n$toid[!n$toid %in% n$fromid])

  # now get an integer for the nodes
  node <- data.frame(node = seq(1, length(unique(n$node_id))),
                     node_id = unique(n$node_id))

  # join the integer id in.
  n <- left_join(n, node, by = "node_id")

  # need to create nodes at the edge of the network so these don't end up NA
  hw_node <- data.frame(id = hw, fromnode = seq((max(n$node) + 1), length.out = length(hw)))
  tl_node <- data.frame(id = tl, tonode = seq((max(hw_node$fromnode) + 1), length.out = length(tl)))

  # create to and from attributes to join to flowlines
  to <- distinct(select(n, id = "fromid", tonode = "node")) |>
    bind_rows(tl_node) |>
    distinct()
  from <- select(n, id = toid, fromnode = "node") |>
    bind_rows(hw_node) |>
    distinct()

  # create a rudimentary node based topology.
  out <- distinct(data.frame(id = c(x$id, x$toid))) |>
    left_join(to, by = "id") |>
    left_join(from, by = "id") |>
    select(id, fromnode, tonode) |>
    filter(!id == get_outlet_value(x))

  if(inherits(x, "hy")) {
    class(out) <- c("hy", class(out))
    attr(out, "orig_names") <- attr(x, "orig_names")
  }

  out
}
