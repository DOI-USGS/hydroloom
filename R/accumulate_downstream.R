#' Accumulate Variable Downstream
#' @description given a variable, accumulate according to network topology.
#'
#' `x` input requires a valid dendritic or non-dendritic network in either
#' id/toid or fromnode/tonode form. See details for additional information.
#'
#' @inheritParams add_levelpaths
#' @param var variable to accumulate.
#' @param total logical if TRUE, accumulation will use "total" apportionment
#' if FALSE, divergence or dendritic apportionment will apply ( see details).
#' @param quiet logical quiet messages?
#' @details
#'
#' Accumulation Methods:
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
#' "No apportionment" (total upstream) routing includes considerably more logic
#' and requires a noteable amount more computation to avoid double counting
#' through systems of diverted channels. The implementation has been tested
#' to match the total drainage area calculations of NHDPlusV2.
#'
#' When flow splits at a diversion, the duplicated part is tracked until it
#' recombines with the non-duplicated part. In this tracking, both nested
#' diversions and diversions that have two or more flow splits in one place
#' are supported. For this algorithm to work, it is critical that the supplied
#' data be a directed acyclic graph and have a complete divergence attribute
#' where 0 indicates no diversion, 1 indicates the main catchment downstream
#' of a diversion and 2 indicates a secondary (one or more) downstram of a
#' diversion.
#'
#' @name accumulate_downstream
#' @returns vector of the same length as `nrow(x)` containing values of `var` accumulated downstream
#' @export
#' @examples
#' x <- sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom"))
#'
#' net <- navigate_network_dfs(x, 8893236, "up")
#'
#' x <- x[x$COMID %in% unlist(net), ]
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
#' plot(y['div_totdasqkm'], lwd = y$div_totdasqkm / 20)
#'
#' z <- x |>
#'   dplyr::select(COMID, FromNode, ToNode, Divergence, AreaSqKM, TotDASqKM)
#'
#' z$tot_totdasqkm <- accumulate_downstream(z, "AreaSqKM", total = TRUE)
#'
#' plot(z['tot_totdasqkm'], lwd = z$tot_totdasqkm / 20)
#'
#' # equivalent values from the nhdplusv2 match!
#' any(abs(z$tot_totdasqkm - z$TotDASqKM) > 0.001)
#'
accumulate_downstream <- function(x, var, total = FALSE, quiet = FALSE) {

  if (!var %in% names(x)) stop(var, " must be in x")

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

  if (nrow(x) == 0) return(c())

  var <- as.character(var)

  required_atts <- c(id, toid, var)

  net <- add_toids_internal(x, c(var, divergence_fraction, required_atts))

  if (length(unique(net$id)) < nrow(net)) {

    required_atts <- c(required_atts, divergence)
    error_context <- "accumulate_downstream for a non-dendritic network"

    if (divergence_fraction %in% names(net)) required_atts <- c(required_atts, divergence_fraction)

  } else {

    error_context <- "accumulate_downstream"
  }

  check_names(net, required_atts, error_context)

  net <- select(net, all_of(as.character(required_atts)))

  x <- select(st_drop_geometry(x), id)

  net[["toid"]] <- replace_na(net[["toid"]], get_outlet_value(net))

  if (any(is.na(net[[var]]))) {
    warning("NA values found in accumulation variable, accumulation math may fail.")
  }

  # if we got this far without a divergence attribute, it's dendritic so all are 0
  if (!divergence %in% names(net)) net[[divergence]] <- 0

  # if no divergence fraction or total is true, we can set 1 for divergence = 1 and 0 for 2
  if (!divergence_fraction %in% names(net)) {
    if (!total & !quiet)
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

  prog <- pbapply::dopb() & !quiet & length(froms$lengths) > 10000

  if (prog) {
    pb = txtProgressBar(0, length(froms$lengths), style = 3)
    on.exit(close(pb))
  }

  if (total) {

    # add node topology to x with valence indication
    out <- left_join(out,
      as.data.table(make_nondendritic_topology(net))[, down_valence := .N, by = fromnode][, up_valence := .N, by = tonode],
      by = id)

    # each node is going to hold a list of upstream nodes and the value associated
    # with each. At each diversion, a record of the split will be recorded.
    # once solved, the node will be marked solved so future visits can keep moving.
    nodes <- data.frame(node = seq(1:max(c(out$fromnode, out$tonode))))

    template <- list(data.table(node = integer(), # the node the diversion came from
      catchment = integer(), # the catchment the diversion went TO
      local_id = character(),
      dup = logical())) # true for duplicate false for original

    nodes$open <- rep(template, nrow(nodes))
    nodes$val <- 0
    nodes$closed <- nodes$part_closed <- rep(list(character()), nrow(nodes))

    divs <- out[out[[divergence]] > 0, ]

    divs <- group_by(divs, .data$fromnode) |>
      select(fromnode, id, divergence) |>
      group_split(.keep = TRUE)

    divs <- bind_cols(data.frame(fromnode = sapply(divs, \(x) x$fromnode[1])),
      data.frame(down_ids = divs))

    # also add a "check_visit" attribute to track when we're done with a node
    # down_valence is how many flowlines are downstream of a node
    # once we've visited them, we can remove tracking information to save memory
    nodes <- left_join(nodes, divs, by = c("node" = "fromnode")) |>
      left_join(select(out, fromnode, check_visit = down_valence) |>
        distinct(), by = c("node" = "fromnode"))

    nodes <- data.table::as.data.table(nodes)

    # we will work on the basis of a node table solving which nodes are open and
    # which are closed working from upstream to downstream.
    for (i in seq_len(length(froms$lengths))) {

      if (!i %% 100 & prog)
        setTxtProgressBar(pb, i)

      l <- froms$lengths[i]

      if (l > 0) {

        out[[var]][i] <- sum(out[[var]][i],
          nodes[out[[fromnode]][i], ]$val)

        updated_node <- update_node(id = out[[id]][i],
          up_node = nodes[out[[fromnode]][i], ],
          down_node = nodes[out[[tonode]][i], ],
          div_att = out[[divergence]][i],
          current_value = out[[var]][i],
          node_values = nodes$val)

        nodes[out$tonode[i], (names(nodes)) := as.list(updated_node)]

        if (nodes[out[[fromnode]][i], check_visit] == 1) {

          nodes[out[[fromnode]][i],
            `:=`(open = list(NULL),
              closed = list(NULL),
              part_closed = list(NULL))]

        } else {

          nodes[out[[fromnode]][i], check_visit := check_visit - 1]
        }

      } else {

        nodes[out[[tonode]][i], val := val + out[[var]][i]]

      }

    }

  } else {
    for (i in seq_len(length(froms$lengths))) {

      if (!i %% 100 & prog)
        setTxtProgressBar(pb, i)

      l <- froms$lengths[i]

      # nothing to do if nothing upstream
      if (l > 0) {

        # sum the current value with the fraction of upstream flows coming in
        out[[var]][i] <- sum(out[[var]][i],
          out[[var]][froms$froms[1:l, i]] *
            out[[divergence_fraction]][i])

      }
    }
  }

  if (prog)
    setTxtProgressBar(pb, i)

  if (Sys.getenv("accumulate_debug") == "debug") return(list(nodes, out))

  left_join(x, out, by = "id")[[var]]

}

#' update_node
#' @description
#' Works at a single catchment and updates the downstream node given
#' information from the upstream node and current catchment.
#'
#' At a newly opened diversion, information about duplication is
#' passed to the downstream node. Diversions are identified uniquely
#' such that a given diversion can have many secondary paths.
#'
#' The downstream node is then inspected with a nested function,
#' "reconcile_nodes" which tries to figure out if any diversions
#' rejoin at the downstream node.
#'
#' @param id identifier of current catchment
#' @param up_node node tracking object for fromnode of catchment
#' @param down_node node tracking object for tonode of catchment
#' @param div_att divergence attribute for current catchment
#' @param current_value value of accumulation at the current catchment
#' @param node_values values for all nodes to be used in deduplicating returning diversions
#' @returns an updated copy of the down_node input with information passed
#' to it from upstream.
#' @noRd
#' @importFrom data.table rbindlist
update_node <- function(id, up_node, down_node, div_att, current_value, node_values) {

  if (div_att == 0) {
    # we can just pass to the outlet node
    # need to bind in case this is a confluence
    down_node$open[[1]] <- rbindlist(list(down_node$open[[1]], up_node$open[[1]]),
      use.names = FALSE, fill = FALSE)

  } else {

    if (div_att == 1) {
      # each main path below a divergence can have more than one duplicate pair
      divs <- filter(up_node$down_ids[[1]], .data$divergence == 2) |> pull(id)
    } else {
      # each sectondary path below a divergence is going to get one and only one dup TRUE record
      divs <- id
    }

    down_node$open[[1]] <-
      rbindlist(list(down_node$open[[1]], up_node$open[[1]],
        data.frame(node = up_node$node,
          catchment = divs,
          local_id = paste0(up_node$node, "-", divs),
          dup = div_att == 2)))
  }

  down_node$val <- down_node$val + current_value
  down_node$closed[[1]] <- list(c(down_node$closed[[1]], up_node$closed[[1]]))
  down_node$part_closed[[1]] <- list(c(down_node$part_closed[[1]], up_node$part_closed[[1]]))

  updated_node <- reconcile_nodes(pass_on = down_node$open[[1]],
    value = down_node$val[[1]],
    node_values = node_values,
    closed = down_node$closed[[1]],
    part_closed = down_node$part_closed[[1]])

  down_node$closed[[1]] <- list(updated_node$closed)
  down_node$open[[1]] <- list(updated_node$pass_on)
  down_node$val <- updated_node$value
  down_node$part_closed[[1]] <- updated_node$part_closed

  down_node
}

#' reconcile nodes
#' @description
#' works at a single node and considers a list of upstream diversions.
#' If the duplicate TRUE and duplicate FALSE record from a diversion are present,
#' the ammount that was duplicated in that diversion is removed from the current value
#'
#' There is special handling for partially closed diversions caused by nested
#' diversions.
#'
#' See code comments for details of implementation.
#'
#' @param pass_on data.frame containing diversion records to be inspected
#' @param value the current value, which may have duplication in it.
#' @param node_values all node values for use in deduplication of values
#' @param closed fully closed diversions
#' @param part_closed partly closed diversions
#' @noRd
#' @importFrom data.table setcolorder
reconcile_nodes <- function(pass_on, value, node_values, closed, part_closed) {

  if (nrow(pass_on) > 0) {

    dup_nodes <- setcolorder(pass_on[, .SD[(.N > 1 && any(dup) && any(!dup))], by = local_id],
      names(pass_on))

    if (nrow(dup_nodes) > 0) {

      dup_nodes <- reconcile_dup_set(dup_nodes)

      # these partly canceled out but still have some open paths out there
      still_open <- dup_nodes[!cancel & !dup][, !c("cancel")]

      # need to save this value because we are going to modify it
      part_closed_incoming <- part_closed

      # this is the value that will get passed on
      part_closed <- unique(c(part_closed, still_open$local_id))

      # we will pass on all the stuff in pass on that wasn't in dup nodes
      pass_on <- rbindlist(list(pass_on[!local_id %in% dup_nodes$local_id],
        # and all the stuff that is in still_open.
        still_open))

      # We will remove all the dup nodes that cancel,
      # are the dup = TRUE record so we only get one per pair
      # and are not already in what's been closed.
      remove_nodes <- dup_nodes[cancel & dup & !local_id %in% closed]

      # Add what we are removing to closed
      closed <- unique(c(closed, remove_nodes$local_id[!remove_nodes$local_id %in% still_open]))

      # update value to remove values that are being canceled here.
      value <- value - sum(node_values[remove_nodes$node[!remove_nodes$local_id %in% part_closed_incoming]])

      # real duplicates were removed above. Remaining duplicates will have the same dup value
      pass_on <- unique(pass_on)

    }

    pass_on <- pass_on[
      # remove closed records
      !local_id %chin% closed]

  }

  return(list(pass_on = pass_on, value = value, closed = closed, part_closed = part_closed))

}

#' reconcile duplicate set
#' @description
#' given a set of duplicate records, determines
#' which can be canceled and which should be passed on.
#' @param dup_nodes data.frame containing potentially cancelable duplicate nodes
#' @returns same data.fram as dup_nodes with an additional "cancel" logical column
#' @noRd
#'
reconcile_dup_set <- function(dup_nodes) {
  dup_nodes[
    , subgroup_size := .N, by = .(local_id, dup) # Step 1
  ][
    , remove := min(subgroup_size), by = local_id # Step 2
  ][
    , cancel := seq_len(.N) <= remove, by = .(local_id, dup) # Step 3
  ][
    , c("subgroup_size", "remove") := NULL # Step 4
  ]
}
