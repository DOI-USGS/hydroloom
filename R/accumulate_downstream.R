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

    # add node topology to x with valence indication
    out <- left_join(out,
                     as.data.table(make_nondendritic_topology(net))[,down_valence := .N, by = fromnode],
                     by = id)

    out$rep_num <- ifelse(out$divergence == 1, out$down_valence - 1, 1)

    # each node is going to hold a list of upstream nodes and the value associated
    # with each. At each diversion, a record of the split will be recorded.
    # once solved, the node will be marked solved so future visits can keep moving.
    nodes <- data.frame(node = seq(1:max(c(out$fromnode, out$tonode))))
    nodes$open <- rep(list(data.frame(node = integer(), # the node the diversion came from
                                      dup = logical())), # true for duplicate false for original
                      nrow(nodes))
    nodes$val <- nodes$to_close <- 0
    nodes$closed <- nodes$part_closed <- rep(list(integer()), nrow(nodes))

    # we will work on the basis of a node table solving which nodes are open and
    # which are closed working from upstream to downstream.
    for(i in seq_len(length(froms$lengths))) {

      l <- froms$lengths[i]

      if(l > 0) {

        fromnode_id <- out[[fromnode]][i]

        out[[var]][i] <- sum(out[[var]][i],
                             nodes[fromnode_id,]$val)

        updated_node <- update_node(down_node = nodes[out[[tonode]][i],],
                                             fromnode_id = out[[fromnode]][i],
                                             pass_on = nodes[fromnode_id,]$open[[1]],
                                             closed = nodes[fromnode_id,]$closed[[1]],
                                             part_closed = nodes[fromnode_id,]$part_closed[[1]],
                                             div_att = out[[divergence]][i],
                                             rep_num = out$rep_num[i],
                                             upnode_value = out[[var]][i],
                                             node_values = nodes$val,
                                             node_to_closes = nodes$to_close)

        nodes[out$tonode[i],] <- updated_node$node
        nodes$to_close <- updated_node$to_close


      } else {

        nodes$val[out[[tonode]][i]] <- nodes$val[out[[tonode]][i]] +
          out[[var]][i]

      }

    }

  } else {
    for(i in seq_len(length(froms$lengths))) {

      l <- froms$lengths[i]

      # nothing to do if nothing upstream
      if(l > 0) {

        # sum the current value with the fraction of upstream flows coming in
        out[[var]][i] <- sum(out[[var]][i],
                             out[[var]][froms$froms[1:l,i]] *
                               out[[divergence_fraction]][i])

      }
    }
  }

  if(Sys.getenv("accumulate_debug") == "debug") return(list(nodes, out))

  left_join(x, out, by = "id")[[var]]

}

update_node <- function(down_node, fromnode_id, pass_on, closed, part_closed,
                        div_att, rep_num, upnode_value, node_values, node_to_closes) {

  if(div_att == 0) {
    # we can just pass to the outlet node
    down_node$open[[1]] <- bind_rows(down_node$open, pass_on)

  } else {

    down_node$open[[1]] <-
      bind_rows(down_node$open, pass_on,
                rep(list(data.frame(node = fromnode_id, # where the diversion eminates from
                                    dup = div_att == 2)), # TRUE for a duplicate, FALSE for no duplicate
                    rep_num))
    if(div_att == 1)
      node_to_closes[fromnode_id] <- rep_num
  }

  down_node$val <- down_node$val + upnode_value
  down_node$closed[[1]] <- c(down_node$closed[[1]], closed)
  down_node$part_closed[[1]] <- c(down_node$part_closed[[1]], part_closed)

  updated_node <- reconcile_nodes(pass_on = down_node$open[[1]],
                                  value = down_node$val[[1]],
                                  node_values = node_values,
                                  closed = down_node$closed[[1]],
                                  part_closed = down_node$part_closed[[1]],
                                  node_to_closes = node_to_closes)

  down_node$closed[[1]] <- updated_node$closed
  down_node$open[[1]] <- updated_node$pass_on
  down_node$val <- updated_node$value
  down_node$part_closed[[1]] <- updated_node$part_closed

  list(node = down_node, to_close = updated_node$node_to_closes)
}

reconcile_nodes <- function(pass_on, value, node_values, closed, part_closed, node_to_closes) {

  if(nrow(pass_on) > 0) {

    dup_nodes <- group_by(pass_on, .data$node) |>
      filter(n() > 1) |>
      filter(any(.data$dup) & any(!.data$dup))

    if(nrow(dup_nodes) > 0) {

      dup_nodes <- reconcile_dup_set(dup_nodes)

      # these partly canceled out but still have some open paths out there
      still_open <- select(filter(dup_nodes, !.data$cancel), -any_of("cancel"))

      # this is the number of duplicates that were created at a given node
      still_open$to_close <- node_to_closes[still_open$node]

      # need to save this value because we are going to modify it
      part_closed_incoming <- part_closed

      # this is the value that will get passed on
      part_closed <- unique(c(part_closed, still_open$node))

      # these are where they are still open and it is because many copies were created.
      still_open_valence <- still_open$node[still_open$to_close > 1]

      # part_closed_incoming <- c(part_closed_incoming, still_open_valence)

      # track that we closed one.
      node_to_closes[still_open_valence] <- node_to_closes[still_open_valence] - 1

      # we will pass on all the stuff in pass on that wasn't in dup nodes
      pass_on <- filter(pass_on, !.data$node %in% dup_nodes$node) |>
        # and all the stuff that is in still_open.
        bind_rows(select(still_open, -"to_close"))

      # We will remove all the dup nodes that cancel,
      # are the dup = TRUE record so we only get one per pair
      # and are not already in what's been closed.
      remove_nodes <- filter(dup_nodes, .data$cancel &
                               .data$dup &
                               !.data$node %in% closed)

      # Add what we are removing to closed
      closed <- unique(c(closed, remove_nodes$node[!remove_nodes$node %in% still_open]))

      # don't pass on if closed
      pass_on <- filter(pass_on, !.data$node %in% closed)

      # update value to remove values that are being canceled here.
      value <- value - sum(node_values[remove_nodes$node[!remove_nodes$node %in% part_closed_incoming]])
    }

    # remove closed records
    pass_on <- filter(pass_on, !.data$node %in% closed)
  }

  return(list(pass_on = pass_on, value = value, closed = closed, part_closed = part_closed, node_to_closes = node_to_closes))

}

reconcile_dup_set <- function(dup_nodes) {
  dup_nodes |>
    group_by(.data$node, .data$dup) |>
    mutate(subgroup_size = n()) |>
    group_by(.data$node) |>
    mutate(remove = min(subgroup_size)) |>
    group_by(.data$node, .data$dup) |>
    mutate(cancel = row_number() <= remove) |>
    select(-"subgroup_size", -"remove")
}
