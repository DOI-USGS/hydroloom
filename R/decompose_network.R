##### decompose_network.R -- network partition into hy_domain objects (Layer 2) #####
#
# Layer 2 surface: decompose_network() plus the accessors its test suite
# needs to round-trip back through the decomposition (get_domain_graph,
# get_domain_for_catchment).
#
# The Layer 1 constructor + validator live in R/decomposition.R; this
# file layers the partition machinery on top. Contract is pinned by
# tests/testthat/test_decomposition_partition.R.
#
# First-cut scope (see plan file jiggly-squishing-wreath.md):
#   - require hy_leveled input; error for hy_topo / hy_flownetwork
#   - one trunk per connected component, equal to the levelpath
#     containing that component's global outlet
#   - one compact per lateral inflow point on the trunk, carrying the
#     maximal upstream sub-network of that lateral
#   - synthetic nexus ids; domain_graph carries flow edges only
#
# Function args trunk_level / trunk_levelpaths / min_compact_size /
# outlet_ids / contained_basins are accepted for signature stability
# but only their defaults are honored in the Layer 2 cut.

#' Decompose a network into domains
#'
#' @description
#' Partitions a hydrologic network into `hy_domain` objects for
#' independent or parallel resolution. Each domain is an
#' HY_CatchmentAggregate with exactly one outlet nexus. Trunk domains
#' carry a major mainstem flowpath; compact domains are the maximal
#' upstream sub-network of a single lateral inflow point along the
#' trunk.
#'
#' @details
#' The Layer 2 implementation of `decompose_network()` commits to the
#' following rules:
#'
#' \itemize{
#'   \item Input must be `hy_leveled` -- the network must already carry
#'     `levelpath`, `levelpath_outlet_id`, and `topo_sort` columns.
#'     Call [add_levelpaths()] first.
#'   \item Exactly one trunk per connected component. The trunk
#'     levelpath is the levelpath that contains the component's global
#'     outlet (the row whose `id` matches its own `terminal_id` from
#'     [sort_network()] with `split = TRUE`).
#'   \item Each compact domain is the maximal upstream sub-network of
#'     a single "lateral inflow point" -- a non-trunk catchment whose
#'     `toid` is on the trunk. Compacts inherit whatever class
#'     `classify_hy()` returns on their slice (`hy_leveled` when the
#'     source is leveled and dendritic).
#'   \item Non-dendritic sources (`hy_flownetwork`) are deferred to a
#'     later layer and currently error.
#' }
#'
#' The arguments `trunk_level`, `trunk_levelpaths`, `min_compact_size`,
#' `outlet_ids`, `overrides`, and `contained_basins` are accepted for
#' signature stability with the design document but only their defaults
#' are honored in this version. `overrides` is passed through to the
#' returned `domain_decomposition$overrides` slot unchanged.
#'
#' @param x `hy_leveled` object (dendritic network already enriched
#'   with levelpaths).
#' @param trunk_level integer. Reserved for a stream-level trunk
#'   threshold; ignored in the current implementation.
#' @param trunk_levelpaths vector of levelpath ids. Reserved for
#'   explicit trunk selection; ignored in the current implementation.
#' @param min_compact_size numeric. Reserved for a minimum-area filter
#'   on compact domains; ignored in the current implementation.
#' @param outlet_ids vector. Reserved for forcing domain outlets at
#'   specific catchment ids; ignored in the current implementation.
#' @param overrides data.frame. Non-dendritic inter-domain transfer
#'   table; pass-through to `decomposition$overrides`.
#' @param contained_basins data.frame. Containment relations; reserved
#'   for a later layer and ignored here.
#' @returns object of class `domain_decomposition` with slots
#'   `domains`, `domain_graph`, `overrides`, `catchment_domain_index`,
#'   `nexus_registry`, and `source_network`.
#' @seealso [hy_domain()], [validate_decomposition()],
#'   [get_domain_graph()], [get_domain_for_catchment()].
#' @export
#' @examples
#' g <- sf::read_sf(system.file("extdata/walker.gpkg", package = "hydroloom"))
#'
#' h <- hy(g) |>
#'   add_toids() |>
#'   add_levelpaths(name_attribute = "GNIS_ID",
#'     weight_attribute = "arbolate_sum")
#'
#' d <- decompose_network(h)
#'
#' length(d$domains)
#'
decompose_network <- function(x,
                              trunk_level = 1,
                              trunk_levelpaths = NULL,
                              min_compact_size = NULL,
                              outlet_ids = NULL,
                              overrides = NULL,
                              contained_basins = NULL) {

  decompose_validate_input(x)

  if (nrow(x) == 0) {
    return(decompose_empty(x, overrides))
  }

  # Step 2 -- split the network into connected components. sort_network
  # with split = TRUE annotates each row with its terminal_id. We work
  # component-by-component so multi-component sources (or single
  # components, the common case) funnel through the same code path.
  sorted <- sort_network(x, split = TRUE)

  terminal_ids <- unique(sorted$terminal_id)

  domains       <- list()
  edges_list    <- list()
  nexuses_list  <- list()
  index_names   <- character(0)
  index_values  <- character(0)

  for (tid in terminal_ids) {

    component <- sorted[sorted$terminal_id == tid, , drop = FALSE]

    # Drop terminal_id now that it has served its splitting purpose.
    # Leaving it in would collide with a second sort_network() call on
    # any downstream slice (the validator runs one internally) because
    # left_join would append a new terminal_id as terminal_id.y.
    component$terminal_id <- NULL

    # classify_hy restores the hy_leveled stamp that sort_network's
    # join stripped off.
    component <- classify_hy(component)

    built <- decompose_build_component(component, tid)

    domains <- c(domains, built$domains)
    edges_list[[length(edges_list) + 1L]]   <- built$edges
    nexuses_list[[length(nexuses_list) + 1L]] <- built$nexuses

    index_names  <- c(index_names,  built$index_names)
    index_values <- c(index_values, built$index_values)
  }

  domain_graph <- bind_rows_or_empty(edges_list,
    cols = c("id", "toid", "nexus_id", "nexus_position", "relation_type"),
    types = list(character(0), character(0), character(0),
      numeric(0), character(0)))

  nexus_registry <- bind_rows_or_empty(nexuses_list,
    cols = c("nexus_id", "from_domain_id", "to_domain_id",
      "trunk_catchment_id", "aggregate_id_measure"),
    types = list(character(0), character(0), character(0),
      character(0), numeric(0)))

  catchment_domain_index <- setNames(index_values, index_names)

  out <- structure(
    list(
      domains                = domains,
      domain_graph           = domain_graph,
      overrides              = overrides,
      catchment_domain_index = catchment_domain_index,
      nexus_registry         = nexus_registry,
      source_network         = x
    ),
    class = "domain_decomposition"
  )

  # Paranoia: the algorithm is constructed to satisfy each invariant,
  # but running the validator once closes the loop on the test contract.
  res <- validate_decomposition(out)

  if (!res$valid) {
    stop("decompose_network produced an invalid decomposition:\n  ",
      paste(res$issues, collapse = "\n  "), call. = FALSE)
  }

  out
}

#' Validate decompose_network input
#' @param x object passed to decompose_network
#' @returns invisible(x) if valid; otherwise stops with guidance.
#' @noRd
decompose_validate_input <- function(x) {

  if (inherits(x, "hy_flownetwork")) {
    stop("decompose_network: input graph is non-dendritic (hy_flownetwork). ",
      "This commonly indicates duplicated ids -- divergences, loops, or ",
      "cycles. Non-dendritic decomposition is deferred to a later ",
      "implementation layer.",
      call. = FALSE)
  }

  if (!inherits(x, "hy_leveled")) {
    stop("decompose_network: input must be hy_leveled. ",
      "Current class: ", paste(class(x), collapse = "/"), ". ",
      "Use add_levelpaths() to enrich the network before decomposing.",
      call. = FALSE)
  }

  required <- c("id", "toid", "levelpath", "topo_sort",
    "levelpath_outlet_id")

  missing_cols <- setdiff(required, names(x))

  if (length(missing_cols) > 0) {
    stop("decompose_network: missing required columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE)
  }

  invisible(x)
}

#' Empty-network decomposition shortcut
#' @param x empty hy_leveled
#' @param overrides pass-through
#' @returns zero-domain domain_decomposition
#' @noRd
decompose_empty <- function(x, overrides) {

  structure(
    list(
      domains = list(),
      domain_graph = data.frame(
        id = character(0), toid = character(0),
        nexus_id = character(0), nexus_position = numeric(0),
        relation_type = character(0),
        stringsAsFactors = FALSE),
      overrides = overrides,
      catchment_domain_index = setNames(character(0), character(0)),
      nexus_registry = data.frame(
        nexus_id = character(0),
        from_domain_id = character(0),
        to_domain_id = character(0),
        trunk_catchment_id = character(0),
        aggregate_id_measure = numeric(0),
        stringsAsFactors = FALSE),
      source_network = x
    ),
    class = "domain_decomposition"
  )
}

#' Build one component's trunk and compacts
#'
#' @param component hy_leveled slice for a single terminal_id
#' @param terminal_id scalar outlet id of the component
#' @returns list with domains, edges, nexuses, and two parallel vectors
#'   for the catchment_domain_index.
#' @noRd
decompose_build_component <- function(component, terminal_id) {

  # Step 3a -- identify the trunk levelpath: the levelpath carrying the
  # component's global outlet row.
  outlet_row <- component[component$id == terminal_id, , drop = FALSE]

  if (nrow(outlet_row) != 1L) {
    stop("decompose_network: component with terminal_id '",
      terminal_id, "' does not have a unique outlet row",
      call. = FALSE)
  }

  trunk_lp <- outlet_row$levelpath

  trunk_mask <- component$levelpath == trunk_lp

  trunk_ids <- component$id[trunk_mask]

  # Step 3b -- build the trunk domain.
  trunk_slice <- component[trunk_mask, , drop = FALSE]
  trunk_slice <- classify_hy(trunk_slice)

  trunk_domain_id <- paste0("trunk_", terminal_id)
  trunk_outlet_nx <- paste0("nx_outlet_", terminal_id)

  # Collect lateral inlets up front so we can populate the trunk's
  # inlet_nexus_ids slot (for symmetry with hy_domain's documented
  # semantics -- the Layer 2 validator doesn't check this, but keeping
  # it populated makes later layers cheaper).
  lateral_seeds <- component$id[!trunk_mask &
    component$toid %in% trunk_ids]

  lateral_nexus_ids <- if (length(lateral_seeds) > 0L) {
    paste0("nx_",
      as.character(lateral_seeds), "_",
      as.character(component$toid[match(lateral_seeds, component$id)]))
  } else {
    character(0)
  }

  trunk_domain <- hy_domain(
    domain_id            = trunk_domain_id,
    domain_type          = "trunk",
    outlet_nexus_id      = trunk_outlet_nx,
    inlet_nexus_ids      = lateral_nexus_ids,
    trunk_domain_id      = NA_character_,
    containing_domain_id = NA_character_,
    catchments           = trunk_slice,
    topo_sort_offset     = 0L)

  # Initialize containers for this component.
  domains      <- list()
  domains[[trunk_domain_id]] <- trunk_domain

  edges <- vector("list", length(lateral_seeds))
  nexuses <- vector("list", length(lateral_seeds) + 1L)

  # Trunk outlet nexus: always present.
  nexuses[[1L]] <- data.frame(
    nexus_id             = trunk_outlet_nx,
    from_domain_id       = trunk_domain_id,
    to_domain_id         = NA_character_,
    trunk_catchment_id   = as.character(terminal_id),
    aggregate_id_measure = NA_real_,
    stringsAsFactors     = FALSE)

  # Index entries for every trunk catchment.
  index_names  <- as.character(trunk_ids)
  index_values <- rep(trunk_domain_id, length(trunk_ids))

  # Step 3c/3d -- build compacts one per lateral inflow point.
  if (length(lateral_seeds) > 0L) {

    residual <- component[!trunk_mask, , drop = FALSE]

    claimed <- character(0)

    for (i in seq_along(lateral_seeds)) {

      seed <- lateral_seeds[[i]]
      seed_chr <- as.character(seed)

      if (seed_chr %in% claimed) {
        # Already absorbed into an earlier compact via shared upstream
        # walk (shouldn't happen for well-formed dendritic residuals,
        # but guard anyway).
        next
      }

      up_ids <- decompose_collect_upstream(residual, seed)

      up_chr <- as.character(up_ids)

      new_ids <- up_chr[!up_chr %in% claimed]

      claimed <- c(claimed, new_ids)

      compact_slice <- component[as.character(component$id) %in% new_ids, ,
        drop = FALSE]

      # Re-route the seed row's toid from the trunk catchment it was
      # originally attached to, to the outlet sentinel. Without this,
      # sort_network() on the compact sees zero outlet rows because
      # the seed's original toid is not an id in the compact slice.
      out_sentinel <- get_outlet_value(compact_slice)
      compact_slice$toid[compact_slice$id == seed] <- out_sentinel

      compact_slice <- classify_hy(compact_slice)

      to_trunk_id <- component$toid[component$id == seed]

      compact_domain_id <- paste0("compact_", terminal_id, "_", seed)

      inter_nexus_id <- paste0("nx_", seed_chr, "_",
        as.character(to_trunk_id))

      compact_domain <- hy_domain(
        domain_id            = compact_domain_id,
        domain_type          = "compact",
        outlet_nexus_id      = inter_nexus_id,
        inlet_nexus_ids      = character(0),
        trunk_domain_id      = trunk_domain_id,
        containing_domain_id = NA_character_,
        catchments           = compact_slice,
        topo_sort_offset     = 0L)

      domains[[compact_domain_id]] <- compact_domain

      edges[[i]] <- data.frame(
        id               = compact_domain_id,
        toid             = trunk_domain_id,
        nexus_id         = inter_nexus_id,
        nexus_position   = NA_real_,
        relation_type    = "flow",
        stringsAsFactors = FALSE)

      nexuses[[i + 1L]] <- data.frame(
        nexus_id             = inter_nexus_id,
        from_domain_id       = compact_domain_id,
        to_domain_id         = trunk_domain_id,
        trunk_catchment_id   = as.character(to_trunk_id),
        aggregate_id_measure = NA_real_,
        stringsAsFactors     = FALSE)

      index_names  <- c(index_names, new_ids)
      index_values <- c(index_values, rep(compact_domain_id, length(new_ids)))
    }
  }

  list(
    domains      = domains,
    edges        = do.call(rbind, edges),
    nexuses      = do.call(rbind, nexuses),
    index_names  = index_names,
    index_values = index_values
  )
}

#' Collect all catchments upstream of (and including) a seed id
#'
#' @description
#' Iterative breadth-first walk over the residual edge list. Starts at
#' `seed`, repeatedly adds rows whose `toid` is in the current frontier.
#' Deliberately works on plain data.frame columns to avoid the
#' hy_node / hy_topo round-trip that `subset_network` performs -- the
#' residual is already a slice and we only need set-of-ids answers.
#'
#' @param residual data.frame with id, toid columns (the non-trunk
#'   rows of a component).
#' @param seed scalar catchment id to start from.
#' @returns vector of ids in the same type as residual$id, including seed.
#' @noRd
decompose_collect_upstream <- function(residual, seed) {

  if (nrow(residual) == 0L) return(seed)

  collected <- seed
  frontier  <- seed

  while (length(frontier) > 0L) {

    next_hop <- residual$id[residual$toid %in% frontier]

    next_hop <- next_hop[!next_hop %in% collected]

    if (length(next_hop) == 0L) break

    collected <- c(collected, next_hop)
    frontier  <- next_hop
  }

  collected
}

#' rbind a list of data.frames; return an empty skeleton if all NULL/empty
#'
#' @param parts list of data.frames (may contain NULLs).
#' @param cols column names for the empty skeleton.
#' @param types parallel list of typed empty vectors.
#' @returns data.frame.
#' @noRd
bind_rows_or_empty <- function(parts, cols, types) {

  parts <- parts[!vapply(parts, is.null, logical(1))]

  parts <- parts[vapply(parts, nrow, integer(1)) > 0L]

  if (length(parts) == 0L) {
    return(as.data.frame(setNames(types, cols),
      stringsAsFactors = FALSE))
  }

  do.call(rbind, parts)
}

#' Get the inter-domain edge list from a decomposition
#'
#' @description
#' Returns the inter-domain graph as a hydroloom edge list, filtered by
#' relation type. The default returns both flow and containment edges;
#' pass `relations = "flow"` to get the dendritic flow DAG only.
#'
#' The returned object is passed through `classify_hy()` so it carries
#' the most-specific hydroloom class (`hy_topo` when the inter-domain
#' graph is dendritic, `hy_flownetwork` when it is not). This lets
#' downstream hydroloom functions like [check_hy_graph()] and
#' [sort_network()] dispatch on it without extra conversion.
#'
#' @param decomposition object of class `domain_decomposition`.
#' @param relations character vector. Which `relation_type` values to
#'   include. Default is both `"flow"` and `"contained"`.
#' @returns hydroloom edge list (`hy_topo` or `hy_flownetwork`).
#' @seealso [decompose_network()].
#' @export
#' @examples
#' g <- sf::read_sf(system.file("extdata/walker.gpkg", package = "hydroloom"))
#'
#' h <- hy(g) |>
#'   add_toids() |>
#'   add_levelpaths(name_attribute = "GNIS_ID",
#'     weight_attribute = "arbolate_sum")
#'
#' d <- decompose_network(h)
#'
#' get_domain_graph(d, relations = "flow")
#'
get_domain_graph <- function(decomposition,
                             relations = c("flow", "contained")) {

  relations <- match.arg(relations, several.ok = TRUE)

  g <- decomposition$domain_graph

  if (is.null(g) || nrow(g) == 0L) {

    empty <- data.frame(
      id = character(0), toid = character(0),
      nexus_id = character(0), nexus_position = numeric(0),
      relation_type = character(0),
      stringsAsFactors = FALSE)

    return(classify_hy(empty))
  }

  out <- g[g$relation_type %in% relations, , drop = FALSE]

  classify_hy(out)
}

#' Look up the domain containing a catchment
#'
#' @description
#' Returns the `domain_id` of the domain that owns a given catchment in
#' a decomposition. Accepts a scalar or vector of catchment ids.
#'
#' @param decomposition object of class `domain_decomposition`.
#' @param catchment_id scalar or vector of catchment ids.
#' @returns character vector of domain ids, same length as
#'   `catchment_id`.
#' @seealso [decompose_network()].
#' @export
#' @examples
#' g <- sf::read_sf(system.file("extdata/walker.gpkg", package = "hydroloom"))
#'
#' h <- hy(g) |>
#'   add_toids() |>
#'   add_levelpaths(name_attribute = "GNIS_ID",
#'     weight_attribute = "arbolate_sum")
#'
#' d <- decompose_network(h)
#'
#' get_domain_for_catchment(d, h$id[1])
#'
get_domain_for_catchment <- function(decomposition, catchment_id) {

  idx <- decomposition$catchment_domain_index

  if (is.null(idx)) {
    stop("decomposition has no catchment_domain_index", call. = FALSE)
  }

  hit <- idx[as.character(catchment_id)]

  if (any(is.na(hit))) {
    stop("catchment id(s) not found in decomposition: ",
      paste(catchment_id[is.na(hit)], collapse = ", "),
      call. = FALSE)
  }

  unname(hit)
}
