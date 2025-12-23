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
#' x$diff <- x$TotDASqKM - x$dend_totdasqkm
#'
#' mapview::mapview(x, zcol = "diff")
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
#' z <- x |>
#'   dplyr::select(COMID, LevelPathI, FromNode, ToNode, Divergence, AreaSqKM, TotDASqKM)
#'
#' z$tot_totdasqkm <- accumulate_downstream(z, "AreaSqKM", "LevelPathI", total = TRUE)
#'
#' z$diff <- z$tot_totdasqkm - z$TotDASqKM
#'
#' plot(z['tot_totdasqkm'], lwd = z$tot_totdasqkm  / 50)
#'
#' mapview::mapview(z, zcol = "diff")
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

  var <- as.character(var)
  main_path <- as.character(main_path)

  if(total) {
    if(is.null(main_path) | length(main_path) == 0) stop("if 'total' parameter is TRUE, a main_path attribute is required.")

    if(!main_path %in% names(x)) stop("couldn't find specified'main_path' attribute in provided data.")

    required_atts <- c(id, toid, main_path, var)
  } else {

    required_atts <- c(id, toid, var)

  }

  net <- add_toids_internal(x, c(var, divergence_fraction, required_atts))

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

  # if no divergence fraction or total is true, we can set 1 for divergence = 1 and 0 for 2
  if(!divergence_fraction %in% names(net)) {
    if(!total & !quiet)
      message("Dendritic routing will be applied. Diversions are assumed to have 0 flow fraction.")
    net[[divergence_fraction]] <- ifelse(net$divergence == 2, 0, 1)
    required_atts <- unique(c(required_atts, divergence_fraction))
  }

  net <- net |>
    select(all_of(required_atts)) |>
    distinct() |>
    # First sort so we have upstream first and outlets last.
    sort_network()

  # Now generate a working index against the sorted data.
  # Find fromids from the working index.
  # columns of the included matrix correspond to the index ids.
  # rows of the matrix correspond to adjacent upstream ids
  froms <- make_fromids(make_index_ids(net), return_list = TRUE)

  stopifnot(all(froms$froms_list$id == unique(net[[id]])))

  out <- select(net, any_of(c(id, as.character(var), main_path, divergence_fraction, divergence))) |>
    distinct()

  stopifnot(all(froms$froms_list$id == out[[id]]))

  if(total) {
    # will use this list to hold the value passed to diverted paths
    nodup <- rep(list(data.frame(node_id = integer(), # tracks where val was duplicated
                                 val = numeric(), # tracks how much was duplicated
                                 dup = logical(), # tracks which side of the duplication is which
                                 closed = logical())), # if TRUE, the node_id is upstream and is represented in val
                 nrow(out))

    # add fromnode to node to track interactions if diversions and returns
    nodes <- make_nondendritic_topology(net)

    out <- left_join(out, nodes, by = id)
  }

  for(i in seq_len(length(froms$lengths))) {
    # # initial diversion

    l <- froms$lengths[i]

    # nothing to do if nothing upstream
    if(l > 0) {

      ups <- froms$froms[1:l,i]

      if(total) {

        if(out[[id]][i] == 8893218) browser()

        upstream_nodups <- bind_rows(nodup[ups])

        updated_values <- reconcile_nodup(upstream_nodups, sum(out[[var]][ups]))

        nodup[[i]] <- update_nodup(out[[fromnode]][i], out[[divergence]][i],
                                   updated_values$pass_on, updated_values$value)

        out[[var]][i] <- sum(out[[var]][i], updated_values$value)

      } else {

        # sum the current value with the fraction of upstream flows coming in
        out[[var]][i] <- sum(out[[var]][i], out[[var]][ups] * out[[divergence_fraction]][i])

      }

    }

  }

  x <- left_join(x, out, by = "id")

  x[[var]]
}

reconcile_nodup <- function(pass_on_nodups, node_contribution) {

  if(nrow(pass_on_nodups) > 0) {
    closed_out <- unique(filter(pass_on_nodups, pass_on_nodups$closed)$node_id)

    # we need to check to see if anything is returning
    dup_nodes <- group_by(pass_on_nodups, .data$node_id) |>
      filter(n() > 1) |>
      filter(any(.data$dup) & any(!.data$dup))

    if(nrow(dup_nodes) > 0) {

      dup_nodes <- reconcile_dup_set(dup_nodes)

      pass_on_nodups <- filter(pass_on_nodups, !.data$node_id %in% dup_nodes$node_id) |>
        bind_rows(filter(dup_nodes, !.data$cancel) |>
                    select(-any_of("cancel")))

      remove_values <- dup_nodes |>
        filter(.data$cancel & !.data$node_id %in% closed_out) |>
        group_by(.data$node_id) |>
        summarise(val = val[1])

      pass_on_nodups <- pass_on_nodups |>
        bind_rows(data.frame(node_id = unique(remove_values$node_id), closed = TRUE))

      node_contribution <- node_contribution - sum(remove_values$val)

    }
  }

  return(list(pass_on = pass_on_nodups, value = node_contribution))

}

# dup_nodes_1 <- structure(list(node_id = c(48, 48, 48),
#                               val = c(86.5719, 86.5719, 86.5719),
#                               dup = c(TRUE, FALSE, FALSE)),
#                          row.names = c(1L, 13L, 14L), class = "data.frame")
#
# dup_nodes_2 <- structure(list(node_id = c(2, 48, 60, 62, 2, 48, 60, 62, 65, 65),
#                               val = c(1.7136, 86.5719, 86.6133, 86.6178, 1.7136, 86.5719, 86.6133, 86.6178, 86.6178, 86.6178),
#                               dup = c(FALSE, FALSE, FALSE,TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, TRUE)),
#                          class = "data.frame",
#                          row.names = c(NA, -10L))
#
# dup_nodes_3 <- data.frame(
#   node_id = c(2, 48, 2, 48, 62, 63, 2, 48, 60, 2, 48, 60, 2, 48, 63, 62, 62, 2, 48),
#   val = c(
#     1.7136, 86.5719, 1.7136, 86.5719, 86.61779999999999, 86.7096, 1.7136, 86.5719,
#     86.6133, 1.7136, 86.5719, 86.6133, 1.7136, 86.5719, 86.7096,
#     86.61779999999999, 86.61779999999999, 1.7136, 86.5719
#   ),
#   dup = rep(rep(c(FALSE, TRUE), 3), rep(c(14L, 1L), c(1L, 5L)))
# )

reconcile_dup_set <- function(dup_nodes) {
  dup_nodes |>
    # also group by TRUE/FALSE
    group_by(.data$node_id, .data$dup) |>
    mutate(subgroup_size = n()) |>
    group_by(.data$node_id) |>
    mutate(remove = min(subgroup_size)) |>
    group_by(.data$node_id, .data$dup) |>
    mutate(cancel = row_number() <= remove) |>
    select(-"subgroup_size", -"remove")
}

update_nodup <- function(fromnode_value, div_value, pass_on_nodups, node_contribution) {

  if(div_value == 0) {

    pass_on_nodups

  } else {

    here <- data.frame(node_id = fromnode_value,
                       val = node_contribution,
                       dup = div_value == 2)


      bind_rows(pass_on_nodups,
                here)

  }
}
