#' Add aggregate id measures to flowlines
#' @description
#'given a set of connected flowlines that have ids and aggregate ids, adds
#' from_aggregate_id_measure and to_aggregate_id_measure for use with
#' \link{index_points_to_lines}
#'
#' Aggregate ids, such as mainstem ids or reachcodes span multiple flowlines.
#' Linear referencing along these features requires knowledge of the portion
#' of the aggregate line a given flowline makes up. This function assumes
#' that the complete aggregate feature is included and calculates the measure
#' of the top and bottom of each flowline along each aggregate line.
#' @param x sf data.frame compatible with \link{hydroloom_names} with atleast
#' id and aggregate_id attributes. A pre-populated toid attribute will be used
#' if present.
#' @details
#' If no "toid" attribute is included, \link{make_attribute_topology} is used to
#' to create one. This is required to ensure the flowlines making up each
#' aggregate line are sorted in a known upstream to downstream order.
#'
add_measures <- function(x) {
 UseMethod("add_measures")
}

#' @name add_measures
#' @export
add_measures.data.frame <- function(x) {
  x <- hy(x)

  hy_reverse(add_measures(x))

}

#' @name add_measures
#' @export
add_measures.hy <- function(x) {

  check_names(x, c(id, aggregate_id), "add_measures")

  if(!toid %in% names(x)) {
    message("no toid found, attempting to add one from geometry.")

    x <- left_join(x, make_attribute_topology(x, 0.1), by = id)

  }

  # sorted top to bottom
  x <- sort_network(x) |>
    mutate(index = seq_len(nrow(x)))

  coords <- st_coordinates(x) |>
    as.data.frame()

  coords <- st_drop_geometry(x) |>
    left_join(coords, by = c("index" = "L1")) |>
    add_len() |>
    group_by(id) |>
    summarise(from_mainstem_measure = min(id_measure),
              to_mainstem_measure = max(id_measure))

  left_join(x, coords, by = c(id))

}
