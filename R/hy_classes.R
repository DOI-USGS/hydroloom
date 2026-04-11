##### hy_classes.R -- S3 class hierarchy constructors, query helpers, and print methods #####

# ---- class documentation pages ----

#' @title hy_topo: self-referencing edge list
#'
#' @description
#' A `hy_topo` object is a self-referencing edge list representing either
#' a catchment topology or a flowline topology: one row per feature,
#' carrying an `id` and a `toid` that points at the immediately downstream
#' feature. This is the main representation for downstream traversal,
#' topological sorting, and most accumulation algorithms in hydroloom.
#'
#' Catchment topology and flowline topology are functionally separate
#' graphs. A catchment carries a known local drainage area and is 1:1
#' with its flowpath (the linear realization of the catchment). A
#' flowline is a linear feature without a committed local drainage area
#' — it may coincide with a catchment's flowpath, but that relationship
#' is not guaranteed. `hy_topo` represents either graph; the two are
#' practically related but should not be conflated.
#'
#' `hy_topo` inherits from `hy`. The enriched subclass `hy_leveled` extends
#' `hy_topo` with topo-sort and levelpath columns, so methods written for
#' `hy_topo` apply to `hy_leveled` objects without modification.
#'
#' @details
#' `hy_topo` requires unique `id`. A divergence would need two downstream
#' connections from the same upstream feature — that is, two rows with
#' the same `id` — and `hy_topo` does not permit that. Networks with
#' divergences belong in [hy_node] (a bipartite graph through
#' `fromnode`/`tonode`) or in [hy_flownetwork] (a junction table that
#' annotates main and diverted paths). When the internal classifier called
#' from [hy()] sees a duplicated `id` in an `id`/`toid` table, it produces
#' an `hy_flownetwork` rather than an `hy_topo`.
#'
#' For the authoritative, programmatic view of which functions are callable
#' on a particular object, use [hy_capabilities()].
#'
#' @section Required columns:
#' \itemize{
#'   \item `id` — catchment or flowline identifier, unique across rows
#'   \item `toid` — `id` of the immediately downstream feature; network
#'     outlets carry a sentinel value
#' }
#'
#' See [hydroloom_name_definitions] for the canonical column definitions
#' and accepted aliases.
#'
#' @section Functions that operate on hy_topo:
#' \itemize{
#'   \item Topology and sorting: [sort_network()], [add_topo_sort()],
#'     [check_hy_graph()]
#'   \item Path enrichment: [add_levelpaths()], [add_pathlength()],
#'     [add_streamorder()]
#'   \item Accumulation and traversal: [accumulate_downstream()],
#'     [navigate_hydro_network()], [navigate_network_dfs()],
#'     [make_index_ids()]
#' }
#'
#' Call [hy_capabilities()] on a specific object to see which functions
#' are callable on it given its current columns.
#'
#' @section Conversions to other representations:
#' \itemize{
#'   \item To [hy_node] (bipartite graph): [make_node_topology()]
#'   \item To [hy_leveled] (enriched edge list): [add_levelpaths()],
#'     which adds `topo_sort`, `levelpath`, and `levelpath_outlet_id`
#'   \item To [hy_flownetwork] (junction table): [add_levelpaths()] then
#'     [to_flownetwork()]
#' }
#'
#' @seealso
#' [hy], [hy_leveled], [hy_node], [hy_flownetwork],
#' [hy_capabilities()], [hy_network_type()], [is_dendritic()],
#' [add_toids()], [sort_network()], [make_node_topology()]
#'
#' @examples
#' x <- sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom"))
#'
#' z <- add_toids(hy(x))
#'
#' hy_network_type(z)
#'
#' z
#'
#' @name hy_topo
NULL

#' @title hy_leveled: enriched self-referencing edge list
#'
#' @description
#' A `hy_leveled` object is a `hy_topo` carrying the additional columns
#' produced by stream leveling: `topo_sort`, `levelpath`, and
#' `levelpath_outlet_id`. These columns are what mainstem-aware operations
#' (Pfafstetter coding, stream level, junction-table conversion) need in
#' order to run.
#'
#' `hy_leveled` is a strict subclass of `hy_topo`, so every function that
#' accepts a `hy_topo` accepts a `hy_leveled` as well.
#'
#' @details
#' `hy_leveled` exists to mark a `hy_topo` as having been through
#' [add_levelpaths()] so downstream functions can dispatch on the presence
#' of leveling without re-checking column names. The leveling columns
#' encode mainstem path identity (`levelpath`), the outlet that closes
#' that path (`levelpath_outlet_id`), and the topological order along
#' the network (`topo_sort`).
#'
#' Like `hy_topo`, `hy_leveled` requires unique `id` and cannot represent
#' divergences as duplicated rows. Convert to [hy_flownetwork] via
#' [to_flownetwork()] to preserve main and diverted paths in junction-table
#' form.
#'
#' @section Required columns:
#' \itemize{
#'   \item `id` — catchment or flowline identifier, unique across rows
#'   \item `toid` — `id` of the immediately downstream feature
#'   \item `topo_sort` — topological sort order (NHDPlus hydrosequence)
#'   \item `levelpath` — mainstem path identifier
#'   \item `levelpath_outlet_id` — outlet `id` that closes each levelpath
#' }
#'
#' See [hydroloom_name_definitions] for the canonical column definitions.
#'
#' @section Functions that operate on hy_leveled:
#' All `hy_topo` methods, plus the leveling-aware operations:
#' \itemize{
#'   \item Mainstem coding: [add_pfafstetter()], [add_streamlevel()]
#'   \item Junction-table conversion: [to_flownetwork()]
#' }
#'
#' Call [hy_capabilities()] on a specific object for the authoritative
#' list given its current columns.
#'
#' @section Conversions to other representations:
#' \itemize{
#'   \item To [hy_node] (bipartite graph): [make_node_topology()]
#'   \item To [hy_flownetwork] (junction table): [to_flownetwork()],
#'     which uses `levelpath` to decide which connection at a divergence
#'     is the main path
#' }
#'
#' @seealso
#' [hy], [hy_topo], [hy_node], [hy_flownetwork],
#' [hy_capabilities()], [hy_network_type()],
#' [add_levelpaths()], [add_pfafstetter()], [to_flownetwork()]
#'
#' @examples
#' x <- sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom"))
#'
#' z <- add_levelpaths(add_toids(hy(x)),
#'                     name_attribute = "GNIS_ID",
#'                     weight_attribute = "arbolate_sum")
#'
#' hy_network_type(z)
#'
#' @name hy_leveled
NULL

#' @title hy_node: bipartite feature-and-nexus graph
#'
#' @description
#' A `hy_node` object is a bipartite graph over catchments or flowlines
#' and nexuses: each catchment (or flowline) is one row, carrying a
#' `fromnode` (the upstream nexus it leaves) and a `tonode` (the
#' downstream nexus it enters). Connectivity is recovered by matching
#' `tonode` to `fromnode` across rows. This is the representation that
#' natively supports divergences without duplicating rows — a divergence
#' is simply a nexus with more than one outgoing feature.
#'
#' As with `hy_topo`, a `hy_node` represents either a catchment topology
#' or a flowline topology. Catchments carry a known local drainage area
#' and are 1:1 with their flowpaths; flowlines are linear features
#' without a committed local drainage area. The two graphs are
#' practically related but functionally distinct.
#'
#' `hy_node` inherits from `hy`.
#'
#' @details
#' Because divergences are encoded through shared node identifiers
#' rather than row duplication, `hy_node` requires unique `id` even on
#' non-dendritic networks. The `fromnode`/`tonode` pair carries the
#' connectivity that an `id`/`toid` edge list cannot.
#'
#' `hy_node` is the entry point for hydroloom's divergence-aware
#' operations: [add_divergence()] flags main and diverted paths from
#' geometry, [add_return_divergence()] marks where diverted paths return
#' to the main, and [subset_network()] walks the bipartite graph during
#' subsetting.
#'
#' @section Required columns:
#' \itemize{
#'   \item `id` — catchment or flowline identifier, unique across rows
#'   \item `fromnode` — upstream nexus identifier
#'   \item `tonode` — downstream nexus identifier
#' }
#'
#' See [hydroloom_name_definitions] for the canonical column definitions.
#'
#' @section Functions that operate on hy_node:
#' \itemize{
#'   \item Divergence handling: [add_divergence()], [add_return_divergence()]
#'   \item Subsetting: [subset_network()]
#'   \item Edge-list construction: [add_toids()]
#' }
#'
#' Call [hy_capabilities()] on a specific object for the authoritative
#' list given its current columns.
#'
#' @section Conversions to other representations:
#' \itemize{
#'   \item To [hy_topo] (self-referencing edge list): [add_toids()].
#'     With `return_dendritic = TRUE` (the default) the resulting `toid`
#'     drops secondary paths at divergences; set the option to `FALSE`
#'     and route through [to_flownetwork()] to keep them.
#' }
#'
#' @seealso
#' [hy], [hy_topo], [hy_leveled], [hy_flownetwork],
#' [hy_capabilities()], [hy_network_type()],
#' [make_node_topology()], [add_toids()], [add_divergence()],
#' [subset_network()]
#'
#' @examples
#' x <- sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom"))
#'
#' z <- hy(x)
#'
#' hy_network_type(z)
#'
#' z
#'
#' @name hy_node
NULL

#' @title hy_flownetwork: non-dendritic junction table
#'
#' @description
#' A `hy_flownetwork` object is a junction table — a non-dendritic edge
#' list of `id` and `toid` rows where `id` may repeat. Each row records
#' one connection between an upstream catchment or flowline and a
#' downstream one, and optional `upmain`/`downmain` logical columns
#' annotate which of the connections at a divergence is the main path.
#' This is the only edge-list-shaped representation in hydroloom that
#' preserves divergences directly.
#'
#' Unlike the other class pages on this index, `hy_flownetwork` does
#' **not** inherit from `hy`. It is a separate junction table — it does
#' not carry the `orig_names` attribute and it does not pass [is.hy()].
#'
#' @details
#' `hy_flownetwork` is what hydroloom's internal classifier produces
#' (via [hy()]) when an `id`/`toid` table has duplicated `id` values,
#' and what [to_flownetwork()] produces from a `hy_leveled` input. The `upmain`/`downmain` annotation, when
#' present, lets divergence-aware operations choose a main path without
#' having to re-derive it from levelpath identity each time.
#'
#' Several hydroloom operations that were originally written against
#' `hy_topo` accept `hy_flownetwork` directly through delegate methods,
#' because the underlying algorithm already tolerates non-dendritic
#' input: [add_streamorder()], [accumulate_downstream()],
#' [get_bridge_flowlines()], [add_levelpaths()], and
#' [make_node_topology()] all work on `hy_flownetwork` without
#' conversion.
#'
#' @section Required columns:
#' \itemize{
#'   \item `id` — catchment or flowline identifier; may be non-unique
#'   \item `toid` — downstream `id`
#' }
#'
#' Optional:
#' \itemize{
#'   \item `upmain` — logical, `TRUE` for the main upstream connection
#'     at each junction
#'   \item `downmain` — logical, `TRUE` for the main downstream
#'     connection at each junction
#' }
#'
#' See [hydroloom_name_definitions] for the canonical column definitions.
#'
#' @section Functions that operate on hy_flownetwork:
#' \itemize{
#'   \item Native operations: [navigate_network_dfs()], [make_index_ids()]
#'   \item Delegated to the `hy_topo` algorithm: [add_streamorder()],
#'     [accumulate_downstream()], [get_bridge_flowlines()],
#'     [add_levelpaths()], [make_node_topology()]
#' }
#'
#' Call [hy_capabilities()] on a specific object for the authoritative
#' list given its current columns.
#'
#' @section Conversions to other representations:
#' \itemize{
#'   \item To [hy_node] (bipartite graph): [make_node_topology()]
#' }
#'
#' Going the other direction — from `hy_node` or `hy_leveled` into
#' `hy_flownetwork` — is what [to_flownetwork()] is for.
#'
#' @seealso
#' [hy], [hy_topo], [hy_leveled], [hy_node],
#' [hy_capabilities()], [hy_network_type()], [is_dendritic()],
#' [to_flownetwork()], [navigate_network_dfs()], [make_index_ids()]
#'
#' @examples
#' x <- sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom"))
#'
#' z <- to_flownetwork(x)
#'
#' hy_network_type(z)
#'
#' z
#'
#' @name hy_flownetwork
NULL

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
    stop("hy_node requires unique id. Each catchment or flowline ",
         "appears once; non-dendritic topology is encoded through ",
         "shared node identifiers, not row duplication.")

  class(x) <- unique(c("hy_node", class(x)))

  x
}

#' Create hy_flownetwork
#' @description Non-dendritic edge list. At minimum requires id and toid
#' (where id may be non-unique). Optionally includes upmain and downmain
#' logical columns that annotate main-path connectivity.
#' @param x data.frame with required columns
#' @returns hy_flownetwork object
#' @noRd
new_hy_flownetwork <- function(x) {

  check_names(x, c(id, toid), "hy_flownetwork")

  if (all(c(upmain, downmain) %in% names(x))) {

    dm <- x[x$downmain, ]

    if (any(duplicated(dm$id)))
      stop("multiple downmain connections per id")

    um <- x[x$upmain, ]

    if (any(duplicated(um$toid)))
      stop("multiple upmain connections per toid")

  }

  class(x) <- unique(c("hy_flownetwork", class(x)))

  x
}

# ---- internal classification helper ----

#' Re-classify a hy object based on its columns
#' @param x data.frame that should be a hy subclass
#' @returns x with appropriate hy subclass restored
#' @noRd
classify_hy <- function(x) {

  # ensure base hy class
  if (!inherits(x, "hy")) class(x) <- c("hy", class(x))

  # strip any stale hy subclasses before re-classifying
  class(x) <- class(x)[!class(x) %in%
    c("hy_topo", "hy_leveled", "hy_node", "hy_flownetwork")]

  has_id    <- "id" %in% names(x)
  has_toid  <- "toid" %in% names(x)
  has_nodes <- all(c("fromnode", "tonode") %in% names(x))
  unique_id <- has_id && !any(duplicated(x$id))

  if (has_id && has_toid && unique_id) {
    x <- new_hy_topo(x)
    if (all(c("topo_sort", "levelpath", "levelpath_outlet_id") %in% names(x)))
      x <- new_hy_leveled(x)
  } else if (has_id && has_toid && !unique_id) {
    # non-dendritic edge list -> flownetwork
    x <- new_hy_flownetwork(x)
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

# ---- guidance constants ----

hy_guidance_topo <- paste0(
  "Use add_toids() to build toid from fromnode/tonode, ",
  "or hy(x, add_topo = TRUE).")

hy_guidance_leveled <- paste0(
  "Use add_toids() then add_levelpaths() ",
  "to enrich the network.")

hy_guidance_node <- paste0(
  "Supply data with fromnode/tonode columns, ",
  "or use make_node_topology() to build them.")

# ---- dispatch helpers (reduce boilerplate in S3 methods) ----

#' Classify a bare hy and re-dispatch, or error with guidance
#' @param x hy object
#' @param fn_name character. Name of the calling function.
#' @param required_class character. Class the function needs.
#' @param guidance character. How to convert.
#' @param ... arguments forwarded to the re-dispatched call.
#' @noRd
hy_classify_and_redispatch <- function(x, fn_name, required_class,
  guidance, ...) {

  x <- classify_hy(x)

  net_type <- hy_network_type(x)

  if (!identical(net_type, "hy")) {

    # check that a method exists for this type to avoid infinite recursion
    method <- utils::getS3method(fn_name, net_type, optional = TRUE)

    if (!is.null(method))
      return(method(x, ...))

  }

  hy_dispatch_error(fn_name, required_class, x, guidance)
}

#' Standard .data.frame method: hy() -> dispatch -> hy_reverse()
#' @param x data.frame
#' @param fn_name character. Name of the function to call.
#' @param ... arguments forwarded to fn_name.
#' @noRd
hy_as_dataframe <- function(x, fn_name, ...) {

  x <- hy(x)

  orig_names <- attr(x, "orig_names")

  x <- match.fun(fn_name)(x, ...)

  attr(x, "orig_names") <- orig_names
  if (!inherits(x, "hy")) class(x) <- c("hy", class(x))

  hy_reverse(x)
}

#' Convert hy_node to hy_topo and re-dispatch
#' @param x hy_node object
#' @param fn_name character. Function to call after conversion.
#' @param ... arguments forwarded.
#' @noRd
hy_node_to_topo <- function(x, fn_name, ...) {

  message("converting hy_node to hy_topo via add_toids(). ",
    "For large networks, call add_toids() explicitly ",
    "to avoid repeated conversion.")

  match.fun(fn_name)(add_toids(x), ...)
}

#' Try to downcast hy_topo to hy_node, then re-dispatch or error
#' @param x hy_topo object
#' @param fn_name character. Function to call after downcast.
#' @param guidance character. Guidance if downcast fails.
#' @param ... arguments forwarded.
#' @noRd
hy_topo_to_node <- function(x, fn_name, guidance = hy_guidance_node,
  ...) {

  x <- as_hy_node(x)

  if (inherits(x, "hy_node"))
    return(match.fun(fn_name)(x, ...))

  hy_dispatch_error(fn_name, "hy_node", x, guidance)
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
    "# hydroloom %s fromnode/tonode graph: %d features, %d nexuses\n",
    dend, length(unique(x$id)), n_nodes))

  NextMethod()
}

#' @export
print.hy_flownetwork <- function(x, ...) {

  if ("downmain" %in% names(x)) {
    n_div <- sum(!x$downmain, na.rm = TRUE)
    cat(sprintf(
      "# hydroloom flow network (junction table): %d connections, %d diversions\n",
      nrow(x), n_div))
  } else {
    n_div <- sum(duplicated(x$id))
    cat(sprintf(
      "# hydroloom flow network: %d connections, %d non-dendritic edges\n",
      nrow(x), n_div))
  }

  NextMethod()
}
