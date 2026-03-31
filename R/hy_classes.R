##### hy_classes.R -- S3 class hierarchy constructors, query helpers, and print methods #####

# ---- internal constructors (not exported) ----

#' Create hy_topo from hy
#' @description self-referencing edge list: id, toid (unique id)
#' @param x hy object
#' @returns hy_topo object
#' @noRd
new_hy_topo <- function(x) {

  stopifnot(inherits(x, "hy"))

  check_names(x, c(id, toid), "hy_topo")

  if (any(duplicated(x$id)))
    stop("hy_topo requires unique id. This self-referencing edge list ",
         "cannot have duplicated rows.\n",
         "  For non-dendritic connectivity, use:\n",
         "  - make_node_topology() for fromnode/tonode (hy_node)\n",
         "  - to_flownetwork() for junction table (hy_flownetwork)")

  class(x) <- unique(c("hy_topo", class(x)))

  x
}

#' Create hy_leveled from hy_topo
#' @description enriched edge list: + topo_sort, levelpath, levelpath_outlet_id
#' @param x hy object (promoted to hy_topo if needed)
#' @returns hy_leveled object
#' @noRd
new_hy_leveled <- function(x) {

  if (!inherits(x, "hy_topo")) x <- new_hy_topo(x)

  check_names(x, c(topo_sort, levelpath, levelpath_outlet_id), "hy_leveled")

  class(x) <- unique(c("hy_leveled", class(x)))

  x
}

#' Create hy_node from hy
#' @description fromnode/tonode graph: id, fromnode, tonode (unique id)
#' @param x hy object
#' @returns hy_node object
#' @noRd
new_hy_node <- function(x) {

  stopifnot(inherits(x, "hy"))

  check_names(x, c(id, fromnode, tonode), "hy_node")

  if (any(duplicated(x$id)))
    stop("hy_node requires unique id. Each catchment appears once; ",
         "non-dendritic topology is encoded through shared node ",
         "identifiers, not row duplication.")

  class(x) <- unique(c("hy_node", class(x)))

  x
}

#' Create hy_flownetwork
#' @description junction table: id, toid, upmain, downmain
#' @param x data.frame with required columns
#' @returns hy_flownetwork object
#' @noRd
new_hy_flownetwork <- function(x) {

  check_names(x, c(id, toid, upmain, downmain), "hy_flownetwork")

  dm <- x[x$downmain, ]

  if (any(duplicated(dm$id)))
    stop("multiple downmain connections per id")

  um <- x[x$upmain, ]

  if (any(duplicated(um$toid)))
    stop("multiple upmain connections per toid")

  class(x) <- unique(c("hy_flownetwork", class(x)))

  x
}

# ---- internal classification helper ----

#' Re-classify a hy object after dplyr operations strip its class
#' @param x data.frame that should be a hy subclass
#' @returns x with appropriate hy subclass restored
#' @noRd
classify_hy <- function(x) {

  # ensure base hy class
  if (!inherits(x, "hy")) class(x) <- c("hy", class(x))

  # strip any stale hy subclasses before re-classifying
  class(x) <- class(x)[!class(x) %in% c("hy_topo", "hy_leveled", "hy_node")]

  has_id    <- "id" %in% names(x)
  has_toid  <- "toid" %in% names(x)
  has_nodes <- all(c("fromnode", "tonode") %in% names(x))
  unique_id <- has_id && !any(duplicated(x$id))

  if (has_id && has_toid && unique_id) {
    x <- new_hy_topo(x)
    if (all(c("topo_sort", "levelpath", "levelpath_outlet_id") %in% names(x)))
      x <- new_hy_leveled(x)
  } else if (has_id && has_toid && !unique_id) {
    # non-dendritic edge list: has toid but duplicate ids
    # set hy_topo class directly (bypasses new_hy_topo uniqueness check)
    class(x) <- unique(c("hy_topo", class(x)))
  } else if (has_id && has_nodes && unique_id) {
    x <- new_hy_node(x)
  }

  x
}

#' Try to downcast hy_topo to hy_node
#' @description When data has both toid and fromnode/tonode, classify_hy
#' prioritizes toid (hy_topo). Node-based functions need this helper to
#' re-classify as hy_node when node columns are present.
#' @param x hy_topo object
#' @returns hy_node object if fromnode/tonode exist, otherwise unchanged x
#' @noRd
as_hy_node <- function(x) {

  if (all(c(fromnode, tonode) %in% names(x)) && !any(duplicated(x$id))) {
    class(x) <- class(x)[!class(x) %in% c("hy_topo", "hy_leveled")]
    class(x) <- unique(c("hy_node", class(x)))
  }

  x
}

#' Generate a guided dispatch error message
#' @param fn_name character. Name of the function that was called.
#' @param required_class character. The class the function dispatches on.
#' @param x object that was passed.
#' @param guidance character. Instruction on how to convert.
#' @noRd
hy_dispatch_error <- function(fn_name, required_class, x, guidance) {

  current <- hy_network_type(x)

  stop(fn_name, "() requires ", required_class, ".\n",
    "  Current input is: ", current, ".\n",
    "  ", guidance,
    call. = FALSE)
}

# ---- exported query helpers ----

#' What representation pattern does this network use?
#'
#' @description Returns the most specific hydroloom class. The class names map
#' to database / graph representation patterns:
#' \itemize{
#'   \item \code{"hy_leveled"}: enriched edge list (+ topo_sort, levelpath)
#'   \item \code{"hy_topo"}: self-referencing edge list (id/toid, dendritic)
#'   \item \code{"hy_node"}: fromnode/tonode graph (id/fromnode/tonode)
#'   \item \code{"hy_flownetwork"}: junction table (id/toid/upmain/downmain)
#'   \item \code{"hy"}: name-aligned base object
#' }
#' @param x object to query.
#' @returns character. Most specific hydroloom class name, or
#' \code{"not a hydroloom object"} if x has no hydroloom class.
#' @export
#' @examples
#' x <- sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom"))
#' hy_network_type(hy(x))
#'
hy_network_type <- function(x) {

  known <- c("hy_leveled", "hy_topo", "hy_node",
             "hy_flownetwork", "hy")

  found <- intersect(class(x), known)

  if (length(found) == 0) return("not a hydroloom object")

  found[1]
}

#' Is the network dendritic?
#'
#' @description Checks whether the network contains any divergences.
#' Returns \code{FALSE} if a \code{divergence} column exists with any
#' value of 2, or if the table has a \code{toid} column and duplicated
#' \code{id} values (indicating row duplication from divergences).
#' Otherwise returns \code{TRUE}.
#' @param x data.frame or hy object to test.
#' @param validate_data logical. If \code{FALSE}, only checks the
#' \code{divergence} column (fast). If \code{TRUE} (default), also
#' checks for duplicated ids.
#' @returns logical. TRUE if the network is dendritic.
#' @export
#' @examples
#' x <- sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom"))
#' is_dendritic(hy(x))
#'
is_dendritic <- function(x, validate_data = TRUE) {

  if (divergence %in% names(x) && any(x[[divergence]] == 2, na.rm = TRUE)) {
    return(FALSE)
  }

  if (validate_data && toid %in% names(x) && any(duplicated(x[[id]]))) {
    return(FALSE)
  }

  TRUE
}

#' What operations are available for this network?
#'
#' @description Returns a named logical vector indicating which hydroloom
#' functions can operate on the input object given its current class and
#' columns.
#'
#' Functions marked TRUE are directly callable. Use
#' \code{include_conversions = TRUE} to see what becomes available after
#' a single representation conversion.
#'
#' @param x object to query.
#' @param include_conversions logical. If TRUE, also marks functions reachable
#' via a single representation conversion.
#' @returns named logical vector.
#' @export
#' @examples
#' x <- sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom"))
#' hy_capabilities(hy(x))
#'
hy_capabilities <- function(x, include_conversions = FALSE) {

  is_topo    <- inherits(x, "hy_topo")
  is_leveled <- inherits(x, "hy_leveled")
  is_node    <- inherits(x, "hy_node")
  is_fn      <- inherits(x, "hy_flownetwork")
  has_nodes  <- all(c(fromnode, tonode) %in% names(x))
  has_div    <- divergence %in% names(x)
  has_geom   <- inherits(x, "sf")

  direct <- c(
    # representation conversions
    add_toids               = is_node,
    make_node_topology      = is_topo,
    to_flownetwork          = is_leveled && has_div,

    # topology operations (edge list)
    sort_network            = is_topo,
    add_topo_sort           = is_topo,
    add_levelpaths          = is_topo,
    add_pathlength          = is_topo,
    add_streamorder         = is_topo,
    accumulate_downstream   = is_topo,
    check_hy_graph          = is_topo,
    navigate_hydro_network  = is_topo,
    add_pfafstetter         = is_leveled,
    add_streamlevel         = is_leveled,

    # fromnode/tonode operations (node identifiers)
    add_divergence          = is_node,
    add_return_divergence   = is_node && has_div,
    subset_network          = is_node,

    # junction table operations
    navigate_network_dfs    = is_topo || is_fn,
    make_index_ids          = is_topo || is_fn,

    # geometry operations
    make_attribute_topology = has_geom,
    index_points_to_lines   = has_geom
  )

  if (!include_conversions) return(direct)

  after_convert <- direct

  if (is_node) {
    topo_fns <- c("sort_network", "add_topo_sort", "add_levelpaths",
                  "add_pathlength", "add_streamorder",
                  "accumulate_downstream", "check_hy_graph",
                  "navigate_hydro_network", "navigate_network_dfs",
                  "make_index_ids", "make_node_topology")
    after_convert[topo_fns] <- TRUE
  }

  if (is_topo && !is_node) {
    node_fns <- c("add_divergence", "add_return_divergence",
                  "subset_network", "add_toids")
    after_convert[node_fns] <- TRUE
  }

  after_convert
}

# ---- print methods ----

#' @export
print.hy_topo <- function(x, ...) {

  tier <- if (inherits(x, "hy_leveled")) "leveled edge list" else "edge list"
  dend <- if (is_dendritic(x, validate_data = FALSE)) "dendritic" else "non-dendritic"
  sorted <- if (topo_sort %in% names(x)) ", sorted" else ""

  cat(sprintf(
    "# hydroloom %s %s (self-referencing): %d features%s\n",
    dend, tier, length(unique(x$id)), sorted))

  NextMethod()
}

#' @export
print.hy_node <- function(x, ...) {

  dend <- if (is_dendritic(x, validate_data = FALSE)) "dendritic" else "non-dendritic"
  n_nodes <- length(unique(c(x$fromnode, x$tonode)))

  cat(sprintf(
    "# hydroloom %s fromnode/tonode graph: %d catchments, %d nexuses\n",
    dend, length(unique(x$id)), n_nodes))

  NextMethod()
}

#' @export
print.hy_flownetwork <- function(x, ...) {

  n_div <- sum(!x$downmain, na.rm = TRUE)

  cat(sprintf(
    "# hydroloom flow network (junction table): %d connections, %d diversions\n",
    nrow(x), n_div))

  NextMethod()
}
