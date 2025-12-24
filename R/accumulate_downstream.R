#' Accumulate Variable Downstream
#' @description given a variable, accumulate according to network topology.
#' @inheritParams add_levelpaths
#' @param var variable to accumulate.
#' @param total logical if TRUE, accumulation will use "total" apportionment
#' if FALSE, divergence or dendritic apportionment will apply ( see details).
#' @param quiet logical quiet messages?
#' @details
#'
#'Accumulation Methods:
#'
#'    Divergence apportioned (divergence routing): Where upstream values are passed with
#'    fractional apportionment such that each downstream connection gets between
#'    0 and 100 percent of the upstream value. Requires a "divergence_fraction"
#'    attribute and the "total" parameter to be `FALSE`.
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
#'    "total upstream routing". Set "total" to TRUE.
#'
#' @name accumulate_downstream
#' @returns vector of the same length as `nrow(x)` containing values of `var` accumulated downstream
#' @export
#' @examples
#' x <- sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom"))
#'
#' net <- navigate_network_dfs(x, 8893236, "up")
#'
#' x <- x[x$COMID %in% unlist(net),]
#'
#' # All default gives dendritic routing
#' x$dend_totdasqkm <- accumulate_downstream(add_toids(x), "AreaSqKM")
#' x$diff <- x$TotDASqKM - x$dend_totdasqkm
#'
#' # notice that diversions reset as if they were headwaters
#' plot(x['dend_totdasqkm'], lwd = x$dend_totdasqkm / 20)
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
#' plot(y['div_totdasqkm'], lwd = y$div_totdasqkm  / 20)
#'
#' z <- x |>
#'   dplyr::select(COMID, FromNode, ToNode, Divergence, AreaSqKM, TotDASqKM)
#'
#' z$tot_totdasqkm <- accumulate_downstream(z, "AreaSqKM", total = TRUE)
#'
#' plot(z['tot_totdasqkm'], lwd = z$tot_totdasqkm  / 20)
#'
#' # equivalent values from the nhdplusv2 match!
#' any(abs(z$tot_totdasqkm - z$TotDASqKM) > 0.001)
#'
accumulate_downstream <- function(x, var, total = FALSE, quiet = FALSE) {

  if(!var %in% names(x)) stop(var, " must be in x")

  UseMethod("accumulate_downstream")

}

#' @name accumulate_downstream
#' @export
accumulate_downstream.data.frame <- function(x, var, total = FALSE, quiet = FALSE) {
  x <- hy(x)

  accumulate_downstream(x, var = attr(x, "orig_names")[var], total = total, quiet = quiet)
}

#' @name accumulate_downstream
#' @export
accumulate_downstream.hy <- function(x, var, total = FALSE, quiet = FALSE) {

  if(nrow(x) == 0) return(c())

  var <- as.character(var)

  required_atts <- c(id, toid, var)

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

  net[["toid"]] <- replace_na(net[["toid"]], get_outlet_value(net))

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
  froms <- make_fromids(make_index_ids(net))

  out <- select(net, any_of(c(id, as.character(var), divergence_fraction, divergence))) |>
    distinct()

  if(total) {
    # will use this list to hold the value passed to diverted paths
    nodup <- rep(list(data.frame(node_id = integer(), # tracks where val was duplicated
                                 val = numeric(), # tracks how much was duplicated
                                 dup = logical(),
                                 req_close = integer())), # tracks which side of the duplication is which
                 nrow(out))

    closed <- rep(list(integer()), nrow(out))

    # add fromnode to node to track interactions if diversions and returns
    nodes <- make_nondendritic_topology(net)

    nodes <- as.data.table(nodes)

    nodes <- nodes[, valence := .N, by = fromnode]

    out <- left_join(out, nodes, by = id)

    rm(nodes)
  }

  for(i in seq_len(length(froms$lengths))) {
    # # initial diversion

    l <- froms$lengths[i]

    # nothing to do if nothing upstream
    if(l > 0) {

      ups <- froms$froms[1:l,i]

      if(total) {

        if(out$id[i] == 14702378) browser()

        if(out$id[[i]] %in% c(14702336, 14702376)) {
          message(out$id[[i]])
          message(print(bind_rows(nodup[ups])))
          message(print(unlist(c(closed[ups]))))
        }

        updated_values <- reconcile_nodup(bind_rows(nodup[ups]),
                                          unlist(c(closed[ups])),
                                          sum(out[[var]][ups]))

        closed[[i]] <- updated_values$closed


        nodup[[i]] <- update_nodup(out[[fromnode]][i],
                                   out[[divergence]][i],
                                   updated_values$pass_on,
                                   updated_values$value,
                                   out[["valence"]][i])

        out[[var]][i] <- sum(out[[var]][i],
                             updated_values$value)

      } else {

        # sum the current value with the fraction of upstream flows coming in
        out[[var]][i] <- sum(out[[var]][i],
                             out[[var]][ups] * out[[divergence_fraction]][i])

      }

    }

  }

  x <- left_join(x, out, by = "id")

  x[[var]]
}

reconcile_nodup <- function(pass_on_nodups, closed_out, node_contribution) {

  if(nrow(pass_on_nodups) > 0) {
    # we need to check to see if anything is returning here
    # look for cases where there is both TRUE and FALSE for a given node.
    dup_nodes <- group_by(pass_on_nodups, .data$node_id) |>
      filter(n() > 1) |>
      filter(any(.data$dup) & any(!.data$dup))

    # if we have stuff to investigate:
    if(nrow(dup_nodes) > 0) {

      # this is in a function to facilitate testing
      dup_nodes <- reconcile_dup_set(dup_nodes)

      # the stuff that's not being removed gets passed on downstream.
      pass_on_nodups <- filter(pass_on_nodups, !.data$node_id %in% dup_nodes$node_id) |>
        bind_rows(filter(dup_nodes, !.data$cancel) |>
                    select(-any_of("cancel")))

      # these need to be subtracted from the current value.
      remove_values <- dup_nodes |>
        # ones that are closed_out are nested above others and get removed here.
        filter(.data$cancel & !.data$node_id %in% closed_out) |>
        group_by(.data$node_id) |>
        summarise(val = val[1],
                  req_close = max(req_close))

      closed_add <- remove_values$node_id[remove_values$req_close == 1]

      if(nrow(remove_values) > 0 & any(pass_on_nodups$node_id %in% remove_values$node_id)) {
        pass_on_nodups$req_close[pass_on_nodups$req_close > 1 & pass_on_nodups$node_id %in% remove_values$node_id] <-
          pass_on_nodups$req_close[pass_on_nodups$req_close > 1 & pass_on_nodups$node_id %in% remove_values$node_id] - 1
      }

      if(any(remove_values$req_close > 1)) {
        pass_on_nodups <- bind_rows(pass_on_nodups,
                                    select(mutate(filter(dup_nodes, .data$req_close > 1 & .data$cancel == FALSE),
                                           req_close = req_close - 1),
                                           -any_of("cancel")))

        remove_values$req_close[remove_values$req_close > 1] <-
          remove_values$req_close[remove_values$req_close > 1] - 1

        remove_values <- filter(remove_values, req_close == 1)
      }

      # add things that are being closed out for later.
      closed_out <- unique(c(closed_out, closed_add))

      node_contribution <- node_contribution - sum(remove_values$val)

    }
  }

  return(list(pass_on = pass_on_nodups, value = node_contribution, closed = closed_out))

}

# finds pairs that cancel eachother out.
# see tests for examples
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

update_nodup <- function(fromnode_value, div_value, pass_on_nodups, node_contribution, valence) {

  if(div_value == 0) {

    pass_on_nodups

  } else {

    rep_num <- 1
    close_num <- 1

    if(div_value == 1) {
      rep_num <- valence - 1
    } else {
      close_num <- valence - 1
    }

      bind_rows(rep(list(pass_on_nodups), rep_num),
                rep(list(data.frame(node_id = fromnode_value,
                                    val = node_contribution,
                                    dup = div_value == 2,
                                    req_close = close_num)),
                    rep_num))
  }
}
