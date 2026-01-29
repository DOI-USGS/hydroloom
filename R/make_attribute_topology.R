#' @title Make Attribute Topology
#' @description given a set of lines with starting and ending nodes that
#' form a geometric network, construct an attribute topology.
#' @inheritParams add_levelpaths
#' @details
#' If a `future` plan is set up, node distance calculations will be
#' applied using future workers.
#'
#' @param min_distance numeric distance in units compatible with the units of
#' the projection of `lines`. If no nodes are found within this distance, no
#' connection will be returned.
#' @returns data.frame with id and toid
#' @export
#' @examples
#'
#' x <- sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom"))
#'
#' y <- dplyr::select(x, COMID)
#'
#' y <- sf::st_transform(y, 5070)
#'
#' z <- make_attribute_topology(y, 10)
#'
#' x <- add_toids(hy(x), return_dendritic = FALSE)
#'
#' x[x$id == x$id[1], ]$toid
#' z[z$COMID == x$id[1], ]$toid
#'
make_attribute_topology <- function(x, min_distance) {
  UseMethod("make_attribute_topology")
}

#' @name make_attribute_topology
#' @export
make_attribute_topology.data.frame <- function(x, min_distance) {

  x <- hy(x)

  hy_reverse(make_attribute_topology(x, min_distance))

}

#' @name make_attribute_topology
#' @export
make_attribute_topology.hy <- function(x, min_distance) {

  x <- select(x, all_of(c(id)))

  # first we get start and end nodes
  nodes <- as.data.frame(cbind(
    st_coordinates(get_node(x, "start")),
    st_coordinates(get_node(x, "end"))))

  # add the id to the nodes
  nodes$id <- x$id

  # name for sanity
  names(nodes) <- c("sx", "sy", "ex", "ey", "id")

  # share row id
  nodes$row <- seq_len(nrow(nodes))
  x$row <- seq_len(nrow(nodes))

  xs <- seq_len(nrow(nodes))

  # apply over allnodes
  closest <- pblapply(xs, function(x, nodes) {

    # distance from one node to all other nodes
    d <- sqrt((nodes$ex[x] - nodes$sx)^2 + (nodes$ey[x] - nodes$sy)^2)

    # if nothing close, 0
    if (min(d) > min_distance) {
      0
    } else {
      # whichever is minimum but not na
      which(d == min(d, na.rm = TRUE))
    }

  }, nodes = nodes, cl = future_available())

  # Add resulting list as a list column
  nodes$torow <- closest

  # remove row == torow and get group size.
  nodes <- select(nodes, all_of(c("row", "torow"))) |>
    unnest(cols = "torow") |>
    filter(.data$row != .data$torow) |>
    left_join(st_drop_geometry(x), by = "row") |>
    left_join(select(st_drop_geometry(x), row, toid = id),
      by = c("torow" = "row")) |>
    select(-all_of(c("row", "torow")))

  nodes$toid <- replace_na(nodes$toid, get_outlet_value(nodes))

  out <- left_join(select(st_drop_geometry(x), -all_of("row")), select(nodes, id, toid), by = id)

  out$toid <- replace_na(out$toid, get_outlet_value(out))

  out
}
