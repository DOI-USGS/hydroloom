# renames to align
rename_measures <- function(x) {
  orig_aggregate_id <- names(attr(x, "orig_names")[attr(x, "orig_names") == aggregate_id])
  new_aggregate_from_measure <- paste0(orig_aggregate_id, "_from_measure")
  new_aggregate_to_measure <- paste0(orig_aggregate_id, "_to_measure")

  rename(x, any_of(setNames(c(aggregate_id_from_measure, aggregate_id_to_measure),
    c(new_aggregate_from_measure, new_aggregate_to_measure))))
}

#' Add aggregate id measures to flowlines
#' @description
#' given a set of connected flowlines that have ids and aggregate ids, adds
#' from_aggregate_id_measure and to_aggregate_id_measure for use with
#' \link{index_points_to_lines}
#'
#' Aggregate ids, such as mainstem ids or reachcodes span multiple flowlines.
#' Linear referencing along these features requires knowledge of the portion
#' of the aggregate line a given flowline makes up. This function assumes
#' that the complete aggregate feature is included and calculates the measure
#' of the top and bottom of each flowline along each aggregate line.
#' @param x sf data.frame compatible with \link{hydroloom_names} with at least
#' id and aggregate_id attributes. A pre-populated toid attribute will be used
#' if present.
#' @returns x with aggregate measures added to it
#' @details
#' If no "toid" attribute is included, \link{make_attribute_topology} is used to
#' to create one. This is required to ensure the flowlines making up each
#' aggregate line are sorted in a known upstream to downstream order.
#'
#' This function assumes that all flowlines that make up an aggregate feature
#' are included. Returned measures will be incomplete and incorrect if aggregate
#' features (mainstems of reaches) are truncated.
#'
#' @examples
#' g <- sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom"))
#'
#' d <- dplyr::select(g, COMID, REACHCODE) |>
#'   sf::st_cast("LINESTRING")
#'
#' add_measures(d)
#' @export
add_measures <- function(x) {
  UseMethod("add_measures")
}

#' @name add_measures
#' @export
add_measures.data.frame <- function(x) {
  hy(x) |>
    add_measures() |>
    rename_measures() |>
    hy_reverse()

}

#' @name add_measures
#' @export
add_measures.hy <- function(x) {

  check_names(x, c(id, aggregate_id), "add_measures")

  remove_names <- c()

  if (!toid %in% names(x)) {
    message("no toid found, attempting to add one from geometry.")

    x <- left_join(x, make_attribute_topology(x, 0.0001), by = id)

    remove_names <- c(remove_names, toid)

  }

  hy_g <- get_hyg(x, TRUE)

  x <- force_linestring(x)

  id_order <- x$id

  # sorted top to bottom
  x <- sort_network(x) |>
    mutate(index = seq_len(nrow(x)))

  coords <- st_coordinates(x) |>
    as.data.frame()

  x <- sf::st_drop_geometry(x)

  coords <- left_join(x, coords, by = c("index" = "L1")) |>
    group_by(.data$aggregate_id) |>
    group_split() |>
    pbapply::pblapply(\(m) {
      m |>
        add_len() |>
        group_by(id) |>
        summarise(aggregate_id_from_measure = min(id_measure),
          aggregate_id_to_measure = max(id_measure))
    }) |>
    bind_rows()

  remove_names <- c(remove_names, "index")

  x <- x[match(id_order, x$id), ]

  select(x, -all_of(remove_names)) |>
    distinct() |>
    left_join(coords, by = c(id)) |>
    put_hyg(hy_g) |>
    sf::st_sf()

}
