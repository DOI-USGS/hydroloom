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
  if(!divergence_fraction %in% names(net) | total) {
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
    nodup <- rep(list(data.frame(node_id = integer(), val = numeric(), dup = logical())), nrow(out))

    # add fromnode to node to track interactions if diversions and returns
    nodes <- make_nondendritic_topology(net)

    out <- left_join(out, nodes, by = id)
  }

  for(i in seq_len(length(froms$lengths))) {
    # # initial diversion
    # if(out$id[i] == 8893210) browser()
    # # initial main
    # if(out$id[i] == 8893186) browser()
    # # div = 0 below initial main
    # if(out$id[i] == 8893184) browser()
    #
    # # # diversion downstream of a flowline with a dup = FALSE
    # if(out$id[i] == 8893212) browser()
    #
    # # # diversion 2
    # if(out$id[i] == 8893220) browser()

    # diversion 3
    # if(out$id[i] == 8893230) browser()

    # # two up diversion
    # if(out$id[i] == 8893530) browser()

    ## single flowline diversion
    # if(out$id[i] == 8893174) browser()

    ## downstream of single flowline diversion
    # if(out$id[i] == 8893172) browser()

    if(out$id[i] == 8893188) browser()

    l <- froms$lengths[i]

    # nothing to do if nothing upstream
    if(l > 0) {

      ups <- froms$froms[1:l,i]

      # 0 for normal, 1 for "main", 2 for "not main"
      here_divergence <- out[[divergence]][i]

      if(total) {

        # this is a list of data.frames -- one row per upstream diversion
        # 0 rows means nothing from that path is tracked as duplicated
        # or a duplicates match.
        up_nodups <- nodup[ups]

        up_vals <- out[[var]][ups]

        here_fromnode <- out$fromnode[i]

        node_contribution <- sum(up_vals)

        node_contribution_nodup <- node_contribution

        # need to track how much duplicate value needs to be removed from
        # each incoming value and make sure we pass updated nodup values
        # down through diversions.
        pass_on_nodups <- data.frame(node_id = integer(), val = numeric(), dup = logical())

        # TODO: pass_on_nodups here needs to respect the pass on logic below.
        # pass_on_nodups is getting messed with here then different assumptions are used
        # just below. Need to combine this logic with that of the loop below
        if(any(sapply(up_nodups, nrow))) {
          pass_on_nodups <- distinct(bind_rows(up_nodups))

          dup_nodes <- pass_on_nodups[pass_on_nodups$node_id %in%
                                        pass_on_nodups$node_id[duplicated(pass_on_nodups$node_id)],]

          if(nrow(dup_nodes) > 0) {
            dup_nodes <- group_by(dup_nodes, .data$node_id) |>
              filter(any(.data$dup) & any(!.data$dup))

            pass_on_nodups <- filter(pass_on_nodups, !.data$node_id %in% dup_nodes$node_id)

            remove_values <- summarise(dup_nodes, val = val[1])

            node_contribution <- node_contribution - sum(remove_values$val)
          }
        }

        # v for values - loop over all upstream values
        # some will be traces of duplicated area, others will be traces of duplicate matches
        # duplicated area (dup TRUE) gets passed down diversions
        # source area (dup FALSE) gets passed down primaries
        for(v in 1:length(up_vals)) {

          # if we have something to consider
          if(nrow(up_nodups[[v]]) > 0) {

            # n for nodes where diversions eminated from
            for(n in 1:nrow(up_nodups[[v]])) {

              # if the current catchment is below a diversion we have to track node stuff
              if(here_divergence == 2) {
                #  this is the secondary side of a divergence

                # we need to track the portion that this is getting from here_node
                # as dup == TRUE

                pass_on_nodups <- bind_rows(pass_on_nodups, filter(up_nodups[[v]][n,], .data$dup))

              } else if(here_divergence == 1) {
                # This is the main side of a divergence

                # We need to track the portion that this is getting from here_node
                # as dup == FALSE

                pass_on_nodups <- bind_rows(pass_on_nodups, up_nodups[[v]][n,])

              }

            }

          }

        }

        # if we are on a diversion, we need to add information about duplication
        # the value here is the sum of the total area upstream minus the
        # portion of that area that is duplicate.
        if(here_divergence == 2) {

          nodup_here <- data.frame(
            node_id = here_fromnode, # this is the path we are duplicating
            val = node_contribution_nodup,
            dup = TRUE)

          # pass_on_nodups was created above from all upstream tracking nodup tables
          nodup[[i]] <- distinct(bind_rows(pass_on_nodups, nodup_here))


        } else if(here_divergence == 1) {
          # this is the main side of a diversion
          nodup_here <- data.frame(
            node_id = here_fromnode, # this is the path we are duplicating
            val = sum(up_vals),
            dup = FALSE)

          # pass_on_nodups was created above from all upstream tracking nodup tables
          nodup[[i]] <- distinct(bind_rows(pass_on_nodups, nodup_here))

        } else {
          # we are not apportioning a diversion -- just pass downstream
          nodup[[i]] <- pass_on_nodups
        }

        out[[var]][i] <- sum(out[[var]][i], node_contribution)

      } else {

        # sum the current value with the fraction of upstream flows coming in
        out[[var]][i] <- sum(out[[var]][i], out[[var]][ups] * div_fraction)

      }

    }

  }

  x <- left_join(x, out, by = "id")

  x[[var]]
}
