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

#' Print a domain_decomposition
#'
#' @description
#' Two-mode S3 print method for `domain_decomposition`. The default
#' (cheap) form prints a fixed-size summary of slot counts: trunks,
#' compacts, catchments, edges, nexuses, and overrides. Cost is bounded
#' by the number of domains, so it stays fast on 50,000-catchment
#' decompositions. The full form prints a hierarchical tree summary
#' with per-block roll-up statistics and is intended for verifying the
#' shape of a freshly-built decomposition before running analysis on it.
#'
#' @details
#' The cheap form is the default because at the REPL most users want a
#' quick "did this come out the way I expected" check, not a full audit.
#' The footer line of the cheap form advertises how to call the full
#' form.
#'
#' The `width` argument is reserved for future column-wrapping support
#' and is currently ignored by both modes.
#'
#' @param x object of class `domain_decomposition`.
#' @param full logical. `FALSE` (default) prints the cheap summary;
#'   `TRUE` prints the full hierarchical tree.
#' @param ... ignored.
#' @param width integer. Reserved for future use; currently ignored.
#' @returns `x`, invisibly.
#' @export
print.domain_decomposition <- function(x, full = FALSE, ...,
                                       width = getOption("width")) {

  if (isTRUE(full)) {
    print_domain_decomposition_full(x, width = width)
  } else {
    print_domain_decomposition_cheap(x)
  }

  invisible(x)
}

#' Cheap-mode print: counts only, no per-domain iteration
#' @noRd
print_domain_decomposition_cheap <- function(x) {

  counts <- decomposition_counts(x)

  fmt <- function(n) format(n, big.mark = ",")

  cat(sprintf(
    "<domain_decomposition: %s trunks, %s compacts, %s catchments>\n",
    fmt(counts$n_trunks), fmt(counts$n_compacts), fmt(counts$n_catch)))

  cat(sprintf("  %-16s %s  (%s trunks, %s compacts)\n",
    "domains:",        fmt(counts$n_total),
    fmt(counts$n_trunks), fmt(counts$n_compacts)))

  cat(sprintf("  %-16s %s edges\n",
    "domain_graph:",   fmt(counts$n_edges)))

  cat(sprintf("  %-16s %s nexuses\n",
    "nexus_registry:", fmt(counts$n_nexus)))

  cat(sprintf("  %-16s %s rows\n",
    "overrides:",      fmt(counts$n_override)))

  cat(sprintf("  %-16s %s catchments\n",
    "source_network:", fmt(counts$n_catch)))

  cat("\n# Use print(x, full = TRUE) for the full tree summary\n")
}

#' Compute the headline counts used by both print modes.
#' Bounded by O(length(domains)); never opens a catchment table.
#' @noRd
decomposition_counts <- function(x) {

  if (length(x$domains) > 0L) {
    types <- vapply(x$domains,
      function(d) d$domain_type, character(1))
  } else {
    types <- character(0)
  }

  list(
    n_total    = length(x$domains),
    n_trunks   = sum(types == "trunk"),
    n_compacts = sum(types == "compact"),
    n_catch    = if (is.null(x$source_network)) 0L
                 else nrow(x$source_network),
    n_edges    = if (is.null(x$domain_graph)) 0L
                 else nrow(x$domain_graph),
    n_nexus    = if (is.null(x$nexus_registry)) 0L
                 else nrow(x$nexus_registry),
    n_override = if (is.null(x$overrides)) 0L
                 else nrow(x$overrides)
  )
}

#' Full-mode print: hierarchical tree with per-block roll-up stats.
#'
#' Layout follows the spec at decomposition_plan.md:580-836. Six
#' top-level slots in fixed order: source_network, domains (with
#' trunks/compacts roll-ups), domain_graph, nexus_registry,
#' catchment_domain_index, overrides. Slot names align at column 4,
#' type tags at column 26, counts right-aligned after.
#' @noRd
print_domain_decomposition_full <- function(x, width) {

  counts <- decomposition_counts(x)

  fmt <- function(n) format(n, big.mark = ",")

  # Header line.
  cat(sprintf(
    "<domain_decomposition: %s trunks, %s compacts, %s catchments>\n",
    fmt(counts$n_trunks), fmt(counts$n_compacts), fmt(counts$n_catch)))

  # Slot 1: source_network
  print_slot_line(
    branch     = "\u251c\u2500",
    name       = "source_network",
    type_tag   = paste0("<", primary_class(x$source_network), ">"),
    count_text = paste0(fmt(counts$n_catch), " rows"))

  # Slot 2: domains (with sub-tree of trunks + compacts roll-ups).
  print_slot_line(
    branch     = "\u251c\u2500",
    name       = "domains",
    type_tag   = "<list>",
    count_text = paste0(fmt(counts$n_total), " elements"))

  if (counts$n_total > 0L) {
    print_domains_rollup(x, counts, fmt)
  }

  # Slot 3: domain_graph
  print_slot_line(
    branch     = "\u251c\u2500",
    name       = "domain_graph",
    type_tag   = paste0("<", primary_class(x$domain_graph), ">"),
    count_text = paste0(fmt(counts$n_edges), " rows"))

  if (counts$n_edges > 0L) {
    print_domain_graph_block(x, fmt)
  }

  # Slot 4: nexus_registry
  print_slot_line(
    branch     = "\u251c\u2500",
    name       = "nexus_registry",
    type_tag   = paste0("<", primary_class(x$nexus_registry), ">"),
    count_text = paste0(fmt(counts$n_nexus), " rows"))

  # Slot 5: catchment_domain_index
  print_slot_line(
    branch     = "\u251c\u2500",
    name       = "catchment_domain_index",
    type_tag   = "<named character>",
    count_text = paste0(fmt(length(x$catchment_domain_index)),
      " entries"))

  # Slot 6: overrides
  override_tail <- if (counts$n_override == 0L) {
    "(none)"
  } else {
    print_override_breakdown(x$overrides)
  }

  print_slot_line(
    branch     = "\u2514\u2500",
    name       = "overrides",
    type_tag   = paste0("<", primary_class(x$overrides), ">"),
    count_text = paste0(fmt(counts$n_override), " rows   ", override_tail))

  # Footer hint with sample id (if any domain exists).
  cat("\n")

  if (counts$n_total > 0L) {

    sample_id <- names(x$domains)[[1L]]

    hint <- sprintf("# Use get_domain(x, \"%s\")", sample_id)

    if (counts$n_override > 0L) {
      hint <- paste0(hint, " or x$overrides for transfer details")
    }

    cat(hint, "\n", sep = "")

  } else {
    cat("# Empty decomposition (no domains)\n")
  }
}

#' Print one top-level slot line, padded so type tags and counts align.
#' @noRd
print_slot_line <- function(branch, name, type_tag, count_text) {

  cat(sprintf("%s %-22s %-18s %s\n",
    branch, name, type_tag, count_text))
}

#' Best-effort primary S3 class for type-tag display.
#'
#' Picks the first class that isn't a generic base — prefers
#' subclasses (e.g., "hy_leveled") over their parents.
#' @noRd
primary_class <- function(obj) {

  if (is.null(obj)) return("NULL")

  cls <- class(obj)

  # Drop the generic "data.frame" / "list" tail when a more specific
  # class is present, but fall back to it when nothing else is.
  specific <- setdiff(cls, c("data.frame", "list"))

  if (length(specific) > 0L) specific[[1L]] else cls[[1L]]
}

#' Domains roll-up sub-tree: trunks block + compacts block.
#' @noRd
print_domains_rollup <- function(x, counts, fmt) {

  types <- vapply(x$domains,
    function(d) d$domain_type, character(1))

  trunks   <- x$domains[types == "trunk"]
  compacts <- x$domains[types == "compact"]

  cat("\u2502  \u2502\n")

  if (length(trunks) > 0L) {

    branch <- if (length(compacts) > 0L) "\u251c\u2500" else "\u2514\u2500"

    cat(sprintf("\u2502  %s <%s trunk domains>\n",
      branch, fmt(length(trunks))))

    print_domain_block_attrs(trunks, fmt,
      cont_char = if (length(compacts) > 0L) "\u2502" else " ")
  }

  if (length(compacts) > 0L) {

    if (length(trunks) > 0L) cat("\u2502  \u2502\n")

    cat(sprintf("\u2502  \u2514\u2500 <%s compact domains>\n",
      fmt(length(compacts))))

    print_domain_block_attrs(compacts, fmt, cont_char = " ")
  }

  cat("\u2502\n")
}

#' Per-block attribute roll-up lines (catchments / area_sqkm /
#' stream_order / dendritic / topo_offset). Lines for missing optional
#' columns are omitted rather than printed as NAs.
#' @noRd
print_domain_block_attrs <- function(domains, fmt, cont_char) {

  prefix <- sprintf("\u2502  %s     ", cont_char)

  catch_counts <- vapply(domains,
    function(d) nrow(d$catchments), integer(1))

  print_attr_line(prefix, "catchments", catch_counts, fmt,
    show_total = TRUE)

  # area_sqkm: present only if every domain's catchments table has it.
  area_present <- all(vapply(domains,
    function(d) "area_sqkm" %in% names(d$catchments), logical(1)))

  if (area_present) {

    area_sums <- vapply(domains,
      function(d) sum(d$catchments$area_sqkm, na.rm = TRUE),
      numeric(1))

    print_attr_line(prefix, "area_sqkm", area_sums, fmt,
      show_total = TRUE, is_float = TRUE)
  }

  # stream_order: per-domain max, then min/median/max across domains.
  so_present <- all(vapply(domains,
    function(d) "stream_order" %in% names(d$catchments), logical(1)))

  if (so_present) {

    so_max <- vapply(domains,
      function(d) suppressWarnings(
        max(d$catchments$stream_order, na.rm = TRUE)),
      numeric(1))

    so_max[!is.finite(so_max)] <- NA_real_

    if (any(!is.na(so_max))) {
      print_attr_line(prefix, "stream_order", so_max, fmt,
        show_total = FALSE)
    }
  }

  # dendritic: read from attr() when set; otherwise infer from class
  # (hy_topo / hy_leveled are dendritic by invariant; hy_flownetwork
  # is non-dendritic by definition).
  dend <- vapply(domains,
    function(d) {
      a <- attr(d$catchments, "dendritic")
      if (!is.null(a)) return(isTRUE(a))
      if (inherits(d$catchments, "hy_flownetwork")) return(FALSE)
      TRUE
    },
    logical(1))

  n_dend <- sum(dend)
  n_nondend <- sum(!dend)

  if (n_nondend == 0L) {
    cat(sprintf("%s%-13s TRUE  (%s)\n",
      prefix, "dendritic", fmt(n_dend)))
  } else {

    # Count diversions across non-dendritic domains: rows with
    # duplicated id contribute one diversion each.
    n_div <- sum(vapply(domains[!dend],
      function(d) sum(duplicated(d$catchments$id)),
      integer(1)))

    cat(sprintf("%s%-13s TRUE  (%s)    FALSE  (%s, %s diversions total)\n",
      prefix, "dendritic", fmt(n_dend), fmt(n_nondend), fmt(n_div)))
  }
}

#' One attribute roll-up line: min, median, max, optionally total.
#' @noRd
print_attr_line <- function(prefix, name, values, fmt,
                            show_total = FALSE,
                            is_float = FALSE) {

  if (length(values) == 0L || all(is.na(values))) return(invisible())

  num <- function(v) {
    if (is_float) formatC(v, format = "f", digits = 1, big.mark = ",")
    else fmt(as.integer(v))
  }

  vmin <- num(min(values, na.rm = TRUE))
  vmed <- num(stats::median(values, na.rm = TRUE))
  vmax <- num(max(values, na.rm = TRUE))

  base <- sprintf("%s%-13s min %6s   median %6s   max %6s",
    prefix, name, vmin, vmed, vmax)

  if (show_total) {
    vsum <- num(sum(values, na.rm = TRUE))
    cat(sprintf("%s   total %s\n", base, vsum))
  } else {
    cat(base, "\n", sep = "")
  }
}

#' domain_graph sub-block (relation_type breakdown + nexus_position
#' summary). Skipped entirely when nexus_position is all-NA.
#' @noRd
print_domain_graph_block <- function(x, fmt) {

  g <- x$domain_graph

  cat("\u2502     ")

  rt <- table(g$relation_type)

  rt_text <- paste(sprintf("%s (%s)", names(rt), fmt(as.integer(rt))),
    collapse = "   ")

  cat(sprintf("%-13s %s\n", "relation_type", rt_text))

  if (!is.null(g$nexus_position) && any(!is.na(g$nexus_position))) {

    np <- g$nexus_position[!is.na(g$nexus_position)]

    cat(sprintf("\u2502     %-13s min %6.1f   median %6.1f   max %6.1f\n",
      "nexus_position",
      min(np), stats::median(np), max(np)))
  } else {
    cat(sprintf("\u2502     %-13s (not yet populated)\n",
      "nexus_position"))
  }

  cat("\u2502\n")
}

#' Tail string for the overrides slot line: counts by transfer_type.
#' @noRd
print_override_breakdown <- function(overrides) {

  if (is.null(overrides) || nrow(overrides) == 0L) return("(none)")

  if (!"transfer_type" %in% names(overrides)) {
    return(sprintf("(%d rows)", nrow(overrides)))
  }

  tt <- table(overrides$transfer_type)

  parts <- sprintf("%s %s", as.integer(tt), names(tt))

  paste0("(", paste(parts, collapse = ", "), ")")
}

#' Build one component's trunk(s) and compacts
#'
#' @param component hy_leveled slice for a single terminal_id.
#' @param terminal_id scalar outlet id of the component.
#' @param trunk_lps vector of levelpath ids that should become trunks.
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
