#' Accumulate Variable Downstream
#' @description given a variable, accumulate according to network topology.
#'
#' `x` input requires a valid dendritic or non-dendritic network in either
#' id/toid or fromnode/tonode form. See details for additional information.
#'
#' @inheritParams add_levelpaths
#' @param var variable to accumulate.
#' @param total logical !!NOT IMPLEMENTED!! if TRUE, accumulation will use "total" apportionment
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
#' # total not implemented yet, but will be soon
#' # z <- x |>
#' #   dplyr::select(COMID, FromNode, ToNode, Divergence, AreaSqKM, TotDASqKM)
#'
#' # z$tot_totdasqkm <- accumulate_downstream(z, "AreaSqKM", total = TRUE)
#'
#' # plot(z['tot_totdasqkm'], lwd = z$tot_totdasqkm / 20)
#'
#' # equivalent values from the nhdplusv2 match!
#' # any(abs(z$tot_totdasqkm - z$TotDASqKM) > 0.001)
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
    if (!total && !quiet)
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
  froms <- make_index_ids(net, mode = "from")

  out <- select(net, any_of(c(id, as.character(var), divergence_fraction, divergence))) |>
    distinct()

  prog <- pbapply::dopb() & !quiet & length(froms$lengths) > 10000

  if (prog) {
    pb = txtProgressBar(0, length(froms$lengths), style = 3)
    on.exit(close(pb))
  }

  if (total) {
    stop("not implemented")
    # replace ids to avoid row lookups
    id_lookup <- data.frame(indid = seq_len(nrow(out)), id = out$id)
    net$id <- id_lookup$indid[match(net$id, id_lookup$id)]
    net$toid <- id_lookup$indid[match(net$toid, id_lookup$id)]
    net$toid <- replace_na(net$toid, 0)

    # get articulation flowlines
    artic_ids <- get_articulation_flowlines(net)

    # logical vector for which rows are articulation flowlines
    artic_indices <- rep(FALSE, nrow(out))
    artic_indices[artic_ids] <- TRUE
    
    # we need to treat the outlets as if they are articulation flowlines
    artic_indices[net$id[net$toid == 0]] <- TRUE
    
    # flip naming for use in dfs
    names(froms)[names(froms) == "froms"] <- "to"

    # make a copy of the values where we will update only the articulation flowlines
    working_vals <- out[[var]]
    
    # traverse the network doing normal accumulation for articulation flowlines
    # and upstream dfs for non-articulation
    
    for (i in seq_along(froms$lengths)) {
      
      if (!i %% 100 && prog)
        setTxtProgressBar(pb, i)
           
      l <- froms$lengths[i]
      
      if (l > 0) {
         if (artic_indices[i]) {
           
          # articulation: normal accumulation
          out[[var]][i] <- sum(out[[var]][i], out[[var]][froms$to[seq_len(l), i]])

          # we can break the network here since this is an articulation flowline
          froms$to[seq_len(l), i] <- 0

          # update working_vals so the articulation flowline's total gets used with dfs results
          working_vals[i] <- out[[var]][i]
           
         } else {
          # non-articulation: upstream dfs to get all contributing area
          # froms has been broken where we already visited an articulation flowline
          upstream_ids <- navigate_network_dfs_internal(
            g = froms, all_starts = i, reset = FALSE, main = FALSE, ind_id_mode = TRUE
          )
          
          out[[var]][i] <- sum(working_vals[c(i, unlist(upstream_ids))])
                               
         }
      }
    }
      
    } else {
    
    for (i in seq_along(froms$lengths)) {

      if (!i %% 100 && prog)
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
