# File Structure Overview:
#
# This file creates index IDs for graph traversal algorithms, mapping network
# IDs to sequential row indices. Functions support four graph modes: "to"
# (downstream), "from" (upstream), and "both" (bidirectional).
#
# Main exported function:
#   - make_index_ids(): Creates adjacency matrices with S3 methods for data.frame/hy
#
# Internal helper functions (not exported, @noRd):
#   - make_from(): Wrapper to create upstream/from direction using make_from_dt()
#   - call_format_index_ids(): Wrapper to format index tables with mode-specific naming
#   - format_index_ids_internal(): Converts long-form tables to matrix format
#   - make_from_dt(): Creates "from" direction index table by inverting "to" direction
#   - make_to_dt(): Creates "to" direction index table from network topology
#
# Deprecated functions (bottom of file):
#   - format_index_ids(): Use make_index_ids() instead
#   - make_fromids(): Use make_index_ids(mode = "from") instead

#' @title Make Index ids
#' @description makes index ids for the provided hy object. These can be used
#' for graph traversal algorithms such that the row number and id are equal.
#' @param x data.frame network compatible with \link{hydroloom_names}.
#' @param long_form logical DEPRECATED
#' @param mode character indicating the mode of the graph. Choose from "to",
#' "from", or "both". Default is "to". Se Details for more information.
#' @details
#'
#' Required attributes: `id`, `toid`
#'
#' mode determines the direction of the graph. If "to", the graph will
#' be directed from the `id` to the `toid`. If "from", the graph will be
#' directed from the `toid` to the `id`. If "both", the list will contain both
#' a "from" and a "to" element containing each version of the graph.
#' @returns list containing named elements:
#' \describe{
#'   \item{to}{adjacency matrix with columns that correspond to `unqiue(x$id)`}
#'   \item{lengths}{vector indicating the number of connections from each node}
#'   \item{to_list}{a data.frame with an `id`, `indid` and a `toindid` list column.}
#' }
#'
#' List will have names `froms`, `lengths`, and `froms_list` for mode "from".
#'
#' NOTE: the long_form output is deprecated and will be removed in a future release.
#'
#' @name make_index_ids
#' @export
#' @examples
#'
#' x <- data.frame(
#'   id = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
#'   toid = c(2, 3, 4, 5, 0, 7, 8, 9, 4)
#' )
#'
#' make_index_ids(x, mode = "to")
#'
#' make_index_ids(x, mode = "from")
#'
#' make_index_ids(x, mode = "both")
#'
#' x <- hy(sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom")))
#'
#' x <- add_toids(x, return_dendritic = FALSE)
#'
#' x <- make_index_ids(x)
#'
#' names(x)
#' class(x$to)
#' class(x$lengths)
#' class(x$to_list)
#' is.list(x$to_list$toindid)
#'
make_index_ids <- function(x, mode = "to", long_form = NULL) {
  UseMethod("make_index_ids")
}

#' @name make_index_ids
#' @export
make_index_ids.data.frame <- function(x, mode = "to", long_form = NULL) {
  make_index_ids(hy(x), mode, long_form)
}

#' @name make_index_ids
#' @export
make_index_ids.hy <- function(x, mode = "to", long_form = NULL) {
  if (!mode %in% c("to", "from", "both")) {
    stop("mode must be one of 'to', 'from', or 'both'.")
  }

  x <- check_hy_outlets(x, fix = FALSE)

  if (!isTRUE(check_hy_graph(x))) {
    stop("found one or more pairs of features that reference eachother.
          Run check_hy_graph to identify issues.")
  }

  if (!is.null(long_form)) {
    warning("long_form is deprecated and will be removed in a future release.")
  }

  x <- sf::st_drop_geometry(x)

  # generate the long form data.table without a list column
  out <- make_to_dt(x)

  # this will be a deprecated pattern but for now, short cut and return.
  if (!is.null(long_form) && long_form) {
    return(out)
  }

  if (mode == "both") {
    list(
      to = call_format_index_ids(out, return_list = TRUE, mode = "to"),
      from = make_from(x, out)
    )
  } else if (mode == "from") {
    # return the from format with a list column
    make_from(x, out)
  } else {
    # return the to format with a list column
    call_format_index_ids(out, return_list = TRUE, mode = "to")
  }
}

#' @title Create "from" direction adjacency structure
#' @description Internal helper to generate upstream adjacency matrix from
#' the to direction table
#' Used as a wrapper for call_format_index_ids with mode = "from"
#' @param x hy object with optional upmain attribute
#' @param out data.table from make_to_dt with id, indid, toindid columns
#' @returns list with froms matrix, lengths vector, and froms_list data.frame
#' @noRd
make_from <- function(x, out) {
  upmain_atts <- NULL

  if ("upmain" %in% names(x)) {
    upmain_atts <- distinct(select(x, all_of(c(id, upmain))))
  }

  out <- make_from_dt(out, upmain = upmain_atts, convert_list = TRUE)

  call_format_index_ids(out, return_list = TRUE, mode = "from")
}

#' @title Format index table to adjacency matrix
#' @description Internal wrapper around format_index_ids_internal that handles
#' column renaming for different graph directions (to/from/link). Used to convert
#' long-form index tables to matrix format with appropriate naming.
#' @param g data.frame with id, indid, and connection column (toindid/fromindid/link)
#' @param return_list logical if TRUE, include the list format in output
#' @param mode character direction mode: "to", "from", or "link"
#' @returns list with adjacency matrix, lengths vector, and optionally list format
#' @noRd
call_format_index_ids <- function(g, return_list = FALSE, mode = "to") {
  if (mode == "from") {
    setnames(g, old = "fromindid", new = "toindid")
  }

  g <- format_index_ids_internal(g, return_list = return_list)

  if (mode == "from") {
    g <- setnames_list(g, c("to", "to_list"), c("froms", "froms_list"))
    g$froms_list <- setnames_list(g$froms_list, c("toindid"), c("fromindid"))
  }

  g
}

setnames_list <- function(l, old, new) {
  names(l)[match(old, names(l))] <- new
  l
}

# this is here to avoid the warning for internal use
# see format_index_ids for documentation
format_index_ids_internal <- function(g, return_list = FALSE) {
  if (!is.list(g$toindid)) {
    base <- data.frame(
      id = unique(g$id),
      indid = unique(g$indid),
      toindid = I(split(g$toindid, g$indid))
    )
    if ("downmain" %in% names(g)) base$main <- I(split(g$downmain, g$indid))
    if ("edge_id" %in% names(g)) base$edge_id <- I(split(g$edge_id, g$indid))
    g <- base
  }

  to_l <- lengths(g$toindid)
  max_to <- max(to_l)

  # Convert list to matrix with NA fill
  to_m <- as.matrix(sapply(g$toindid, "[", seq(max_to)))

  if ("main" %in% names(g)) {
    main <- as.matrix(sapply(g$main, "[", seq(max_to)))
  }

  if ("edge_id" %in% names(g)) {
    edge_id_m <- as.matrix(sapply(g$edge_id, "[", seq(max_to)))
  }

  if (max_to == 1) {
    to_m <- matrix(to_m, nrow = 1)

    if ("main" %in% names(g)) {
      main <- matrix(main, nrow = 1)
    }

    if ("edge_id" %in% names(g)) {
      edge_id_m <- matrix(edge_id_m, nrow = 1)
    }
  }

  # NAs should be length 0
  to_l[is.na(to_m[1, ])] <- 0

  out <- list(to = to_m, lengths = to_l)

  if ("main" %in% names(g)) {
    out <- c(out, list(main = main))
  }

  if ("edge_id" %in% names(g)) {
    out <- c(out, list(edge_id = edge_id_m))
  }

  if (return_list) {
    return(c(out, list(to_list = g)))
  }

  out
}

#' @title Create "from" direction index table
#' @description Internal helper function that inverts a "to" direction index
#' table to create a "from" direction index table with upstream connections.
#' This function takes the to_list output and reorganizes it so that each node
#' has a list of upstream (fromindid) connections instead of downstream
#' (toindid) connections.
#' @param index_ids data.table or data.frame containing `id`, `indid`, and
#' `toindid` in long form.
#' @param upmain optional data.frame containing id and upmain columns, where
#' upmain is a logical value indicating if the id is the upmain connection
#' from its downstream neighbors. If provided, the output will include a main
#' list column indicating which upstream connections are main paths.
#' @returns data.frame with columns: id, indid, and fromindid (list column
#' containing upstream index ids). If upmain is provided, also includes a main
#' list column.
#' @importFrom data.table setcolorder
#' @noRd
make_from_dt <- function(index_ids, upmain, convert_list = FALSE) {
  index_ids <- select(index_ids, -any_of(c("upmain", "main")))

  # slightly faster but requires data.table
  index_ids <- as.data.table(index_ids)

  if (!is.null(upmain)) {
    index_ids <- merge(index_ids, as.data.table(upmain), by = "id", all.x = TRUE, sort = FALSE)
  }

  ids <- unique(index_ids[, c("indid", "id")])

  froms <- unique(merge(
    index_ids[, list(indid)],
    setnames(index_ids, c("toindid", "indid"), c("indid", "fromindid")),
    by = "indid", all.x = TRUE
  ))

  if (convert_list) {
    if (!is.null(upmain)) {
      froms <- froms[, list(
        fromindid = list(c(fromindid)),
        main = list(c(upmain))
      ), by = indid]
    } else {
      froms <- froms[, list(fromindid = list(c(fromindid))), by = indid]
    }

    setcolorder(merge(ids, froms, by = "indid", all.x = TRUE), c("id", "indid", "fromindid"))
  } else {
    setcolorder(froms, c("id", "indid", "fromindid"))
  }
}

#' @title Create undirected node adjacency table
#' @description Internal helper that builds a long-form undirected adjacency
#' data.table from a node topology table. Each flowline (edge) produces two rows,
#' one per endpoint, so the result can be passed directly to
#' format_index_ids_internal. Nodes are remapped to contiguous integer indices
#' using the same merge pattern as make_to_dt.
#' @param node_topo data.frame with columns id (flowline id), fromnode, tonode
#' as returned by make_nondendritic_topology.
#' @returns data.table with columns: id (original node value), indid (contiguous
#' node index), toindid (neighbor node's contiguous index), edge_id (row position
#' in node_topo, i.e. flowline index).
#' @importFrom data.table rbindlist
#' @importFrom data.table setorder
#' @noRd
make_adj_dt <- function(node_topo) {
  edge_id <- seq_len(nrow(node_topo))

  # Each flowline is an undirected edge: two rows per edge, one per endpoint
  fwd <- data.table(
    id = node_topo$fromnode,
    toid = node_topo$tonode,
    edge_id = edge_id
  )
  rev <- data.table(
    id = node_topo$tonode,
    toid = node_topo$fromnode,
    edge_id = edge_id
  )
  combined <- rbindlist(list(fwd, rev))

  # Remap nodes to contiguous 1..n (same merge pattern as make_to_dt non-dendritic)
  all_nodes <- sort(unique(c(node_topo$fromnode, node_topo$tonode)))
  node_map <- data.table(id = all_nodes, indid = seq_along(all_nodes))
  node_map_to <- copy(node_map)
  setnames(node_map_to, c("id", "indid"), c("toid", "toindid"))

  combined <- merge(
    combined, node_map, by = "id", all.x = TRUE, sort = FALSE
  )
  combined <- merge(
    combined, node_map_to, by = "toid", all.x = TRUE, sort = FALSE
  )

  # Sort by indid so unique(id) and split(..., indid) align in format_index_ids_internal
  setorder(combined, "indid")

  select(combined, all_of(c("id", "indid", "toindid", "edge_id")))
}

#' @title Create "to" direction index table
#' @description Internal helper function that creates a data.frame with index
#' ids for network graph traversal. This function maps original network ids to
#' sequential index ids (indid) and identifies downstream connections (toindid).
#' The function handles both dendritic networks (where each id is unique) and
#' non-dendritic networks (where ids may appear multiple times due to divergences).
#' @param x hy object or data.frame containing network topology with id and toid
#' attributes. May optionally include downmain and/or upmain attributes to track
#' main path connections.
#' @returns data.frame with columns: id (original network id), indid (sequential
#' index id), and toindid (index id of downstream connection, 0 for outlets).
#' If downmain or upmain attributes are present in x, they are preserved in the
#' output.
#' @noRd
make_to_dt <- function(x) {

  vars <- c("id", "toid")
  if ("downmain" %in% names(x)) vars <- c(vars, "downmain")
  if ("upmain" %in% names(x)) vars <- c(vars, "upmain")

  x <- distinct(select(x, all_of(vars)))

  if (any(duplicated(x$id))) { # we have a nondedritic network

    # start table with id and row id
    out <- data.table(
      id = unique(x$id),
      indid = seq(1, length(unique(x$id)))
    )

    out_rename <- copy(out)
    setnames(out_rename, old = "indid", new = "toindid")

    # merge the original table to the indid table by id then again by toid
    out <-
      as.data.table(x)[, vars, with = FALSE] |> # only vars we need
      merge(out, by = "id", all.x = TRUE, sort = FALSE) |> # merge to get indid
      merge(out_rename, by.x = "toid", by.y = "id", all.x = TRUE, sort = FALSE) # merge to get toindid

    out$toindid <- replace_na(out$toindid, 0)

  } else {
    # we have a dendritic network, so we can just use the id and toid
    out <- data.table(id = x$id, indid = seq(1, nrow(x)))

    out$toindid <- match(x$toid, x$id, nomatch = 0)

    out <- merge(out, x[, vars, with = FALSE], by = "id", all.x = TRUE, sort = FALSE)

  }
  select(out, -any_of("toid"))
}

#' @title DEPRECATED: Format Index ids
#' @description format_index_ids is deprecated and will be removed in a future release.
#' @param g data.frame graph with `id`, `inid` and `toindid` as returned by \link{make_index_ids}
#' @param return_list logical if TRUE, the returned list will include a
#' "froms_list" element containing all from ids in a list form.
#' @returns list containing an adjacency matrix and a lengths vector indicating
#' the number of connections from each node. If `return_list` is `TRUE`, return
#' will also include a data.frame with an `indid` column and a `toindid` list
#' column.
#' @export
format_index_ids <- function(g, return_list = FALSE) {
  warning("format_index_ids is deprecated and will be removed in a future release.
         Use make_index_ids instead.")

  format_index_ids_internal(g, return_list = return_list)
}

#' @title DEPRECATED Convert "to" index ids to "from" index ids
#' @description make_fromids is deprecated and will be removed in a future release.
#' Use \link{make_index_ids} with `mode = "from"` instead.
#' @param index_ids data.frame as returned by \link{make_index_ids}
#' @param return_list logical if TRUE, the returned list will include a
#' "froms_list" element containing all from ids in a list form.
#' @param upmain data.frame containing `id` and `upmain` columns. `upmain` should
#' be a logical value indicating if the id is the upmain connection from its
#' downstream neighbors.
#' @returns list containing a "froms" matrix, "lengths" vector,
#' and optionally "froms_list" elements.
#' @export
make_fromids <- function(index_ids, return_list = FALSE, upmain = NULL) {
  if (is.list(index_ids$to_list)) {
    warning("make_fromids is deprecated and will be removed in a future release.
         Use make_index_ids with mode = 'from' instead.")

    froms <- make_from_dt(unnest(index_ids$to_list, "toindid"), upmain, convert_list = TRUE)
  } else {
    froms <- make_from_dt(index_ids, upmain, convert_list = TRUE)
  }

  froms_l <- lengths(froms$fromindid, use.names = FALSE)
  max_from <- max(froms_l)

  # Convert list to matrix with NA fill
  froms_m <- matrix(sapply(froms$fromindid, "[", seq(max_from)),
    nrow = max_from, ncol = nrow(froms)
  )

  main_m <- matrix(sapply(froms$main, "[", seq(max_from)),
    nrow = max_from, ncol = nrow(froms)
  )

  # NAs should be length 0
  froms_l[is.na(froms_m[1, ])] <- 0

  out <- list(froms = froms_m, lengths = froms_l)

  if (!is.null(upmain)) {
    out <- c(out, list(main = main_m))
  }

  if (return_list) {
    return(c(out, list(froms_list = froms)))
  }

  out
}

fromindid <- NULL
