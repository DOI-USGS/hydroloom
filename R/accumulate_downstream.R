#' Accumulate Variable Downstream
#' @description given a variable, accumulate according to network topology.
#' @inheritParams add_levelpaths
#' @param var variable to accumulate.
#' @param total logical if TRUE, accumulation will use "total" apportionment
#' if FALSE, divergence or dendritic apportionment will apply ( see details).
#' @param main_path character attribute name (required if total is TRUE.)
#' The attribute should contain a unique path identifier such as levelpath
#' or mainstem id. Used to avoid double counting accumulations through
#' systems of diversions. When a diversion derives from a main path it
#' will not be added back to the main path again when the diversion rejoins.
#' @param quiet logical quiet messages?
#' @details
#'
#'Accumulation Methods:
#'
#'    Divergence apportioned (divergence routing): Where upstream values are passed with
#'    fractional apportionment such that each downstream connection gets between
#'    0 and 100 percent of the upstream value. This has also been referred to as
#'    "divergence routing" Requires a "divergence_fraction" attribute and the
#'    "total" parameter to be `FALSE`.
#'
#'    Dendritic apportionment (no divergence routing): Where upstream values are not passed to
#'    secondary paths at all -- this is essentially a special case of divergence
#'    apportioned where no diversion fraction value is provided and 0 is
#'    assumed for all divergences. Do not include a "divergence_fraction" and
#'    set "total" to `FALSE`.
#'
#'    No apportionment (total upstream): where upstream values are passed without being
#'    apportioned such that each downstream connection gets the full upstream
#'    value and there is special handling where diversions join back to the main
#'    flow to avoid double counting. This is also referred to as
#'    "total upstream routing". Set "total" to TRUE -- this method requires
#'    a path attribute such as levelpath or mainstem identifier.
#'
#' @name accumulate_downstream
#' @returns vector of the same length as `nrow(x)` containing values of `var` accumulated downstream
#' @export
#' @examples
#' x <- sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom"))
#'
#' # All default gives dendritic routing
#' x$dend_totdasqkm <- accumulate_downstream(add_toids(x), "AreaSqKM")
#'
#' # notice that diversions reset as if they were headwaters
#' plot(x['dend_totdasqkm'], lwd = x$dend_totdasqkm / 50)
#'
#' # add a diversion_fraction that splits flow evenly
#' # max(dplyr::n()) is the number of flowlines in a FromNode group.
#' y <- x |>
#'   dplyr::group_by(FromNode) |>
#'   dplyr::mutate(divergence_fraction = 1 / max(dplyr::n())) |>
#'   dplyr::ungroup()
#'
#' y$div_totdasqkm <- accumulate_downstream(y, "AreaSqKM")
#'
#' # notice that diversions don't reset -- they carry a fraction of area
#' plot(y['div_totdasqkm'], lwd = y$div_totdasqkm  / 50)
#'
accumulate_downstream <- function(x, var, main_path = NULL, total = FALSE, quiet = FALSE) {

  if(!var %in% names(x)) stop(var, " must be in x")

  UseMethod("accumulate_downstream")

}

#' @name accumulate_downstream
#' @export
accumulate_downstream.data.frame <- function(x, var, main_path = NULL, total = FALSE, quiet = FALSE) {
  x <- hy(x)

  accumulate_downstream(x, attr(x, "orig_names")[var], attr(x, "orig_names")[main_path], total, quiet)
}

#' @name accumulate_downstream
#' @export
accumulate_downstream.hy <- function(x, var, main_path = NULL, total = FALSE, quiet = FALSE) {

  if(nrow(x) == 0) return(c())

  if(total) {
    if(is.null(main_path) | length(main_path) == 0) stop("if 'total' parameter is TRUE, a main_path attribute is required.")

    if(!main_path %in% names(x)) stop("couldn't find specified'main_path' attribute in provided data.")

    required_atts <- c(id, toid, main_path, var)
  } else {

    required_atts <- c(id, toid, var)

  }

  net <- add_toids_internal(x, c(var, divergence_fraction))

  if(length(unique(net$id)) < nrow(net)) {

    required_atts <- c(required_atts, divergence)
    error_context <- "accumulate_downstream for a non-dendritic network"

    if(divergence_fraction %in% names(net)) required_atts <- c(required_atts, divergence_fraction)

  } else {

    error_context <- "accumulate_downstream"
  }

  check_names(net, required_atts, error_context)

  net <- select(net, all_of(as.character(required_atts)))

  x <- select(st_drop_geometry(x), id)

  out_val <- get_outlet_value(net)

  net[["toid"]] <- replace_na(net[["toid"]], out_val)

  if(any(is.na(net[[var]]))) {
    warning("NA values found in accumulation variable, accumulation math may fail.")
  }

  # if we got this far without a divergence attribute, it's dendritic so all are 0
  if(!divergence %in% names(net)) net[[divergence]] <- 0

  # if no divergence fraction, we can set 1 for divergence = 1 and 0 for 2
  if(!divergence_fraction %in% names(net)) {
    if(!total & !quiet)
      message("Dendritic routing will be applied. Diversions are assumed to have 0 flow fraction.")
    net[[divergence_fraction]] <- ifelse(net$divergence == 2, 0, 1)
  }

  net <- net |>
    select(all_of(c(id, toid, as.character(var), divergence, divergence_fraction))) |>
    distinct() |>
    # First sort so we have upstream first and outlets last.
    sort_network()

  # Now generate a working index against the sorted data.
  # Find fromids from the working index.
  # columns of the included matrix correspond to the index ids.
  # rows of the matrix correspond to adjacent upstream ids
  froms <- make_fromids(make_index_ids(net), return_list = TRUE)

  stopifnot(all(froms$froms_list$id == unique(net[[id]])))

  out <- select(net, all_of(c(id, as.character(var))), divergence_fraction) |>
    distinct()

  stopifnot(all(froms$froms_list$id == out[[id]]))

  for(i in seq_len(length(froms$lengths))) {

    l <- froms$lengths[i]

    # nothing to do if nothing upstream
    if(l > 0) {

      ups <- froms$froms[1:l,i]

      # this is the fraction of flow coming from upstream
      # that goes to the current catchment
      div_fractions <- out[[divergence_fraction]][i]

      # sum the current value with the fraction of upstream flows coming in
      out[[var]][i] <- sum(out[[var]][i], out[[var]][ups] * div_fractions)

    }
  }

  x <- left_join(x, out, by = "id")

  x[[var]]
}
