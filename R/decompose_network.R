##### decompose_network.R -- partition network into hy_domain objects #####
#
# decompose_network() plus accessors (get_domain_graph,
# get_domain_for_catchment) and print.domain_decomposition.
#
# The Layer 1 constructor + validator are defined in R/decomposition.R; this
# file layers the partition machinery on top. Contract is pinned by
# tests/testthat/test_decomposition_partition.R.
#
# Implemented:
#   - require hy_leveled input; error for hy_topo / hy_flownetwork
#   - trunk selection: single-outlet-levelpath default, trunk_threshold
#     metric-based multi-trunk, trunk_levelpaths explicit override
#   - one domain per lateral inflow point on a trunk, carrying the
#     maximal upstream sub-network of that lateral
#   - synthetic nexus ids; inter-domain edges derived from nexus_registry
#   - print method (cheap + full modes)

#' Decompose a network into domains
#'
#' @description
#' Partitions a hydrologic network into `hy_domain` objects for
#' independent or parallel computation. Each drainage basin is split
#' into domains along its main path; the main path itself is returned
#' separately as the basin's *extensive connectivity* overlay. See
#' [hy_domain()] and [domain_decomposition] for details.
#'
#' @details
#' **Input.** Input must be `hy_leveled` -- the network must already
#' carry `levelpath`, `levelpath_outlet_id`, and `topo_sort` columns.
#' Call [add_levelpaths()] to add. Non-dendritic sources
#' (`hy_flownetwork`) are not supported at this time.
#'
#' **Extensive connectivity selection.** Each drainage basin's extensive network is selected
#' from `trunk_metric`, `trunk_threshold`, and `trunk_levelpaths` (see
#' arguments). The extensive connectivity is materialized as the basin's
#' `domain_connectivity[[basin_id]]` overlay (a `hy_leveled`). The
#' `trunk_*` parameter names retain the word 'trunk' for hydrologic
#' continuity even though the resulting object is the basin's
#' extensive connectivity.
#'
#' @param x `hy_leveled` object (dendritic network already enriched
#'   with levelpaths).
#' @param trunk_metric character. Metric evaluated at each levelpath
#'   outlet to decide trunk eligibility. `"drainage_area"` reads
#'   `total_da_sqkm`; `"arbolate_sum"` reads `arbolate_sum`. Only
#'   consulted when `trunk_threshold` is non-NULL.
#' @param trunk_threshold numeric scalar or `NULL`. Value of
#'   `trunk_metric` at a levelpath outlet above which the levelpath is
#'   a trunk candidate. `NULL` (default) falls back to one trunk per
#'   drainage basin.
#' @param trunk_levelpaths vector of levelpath ids or `NULL`. When
#'   non-NULL, bypasses the threshold rule and forces these levelpaths
#'   to be trunks (the basin's terminal-outlet levelpath is always unioned
#'   in). Every id must exist in `x$levelpath`.
#' @param domain_breaks vector of catchment ids or `NULL`. When
#'   non-NULL, these trunk catchment ids define where the trunk is
#'   segmented into domain groups. Each break id becomes a
#'   segment terminal in addition to the auto-detected confluences and
#'   outlets. Breaks that are not trunk catchments in a given basin
#'   are silently ignored. When `NULL` (default), trunk segmentation
#'   is determined automatically from trunk confluences and (if
#'   available) bridge flowlines.
#' @param overrides data.frame. Non-dendritic inter-domain transfer
#'   table; pass-through to `decomposition$overrides`.
#' @returns a [domain_decomposition] object.
#' @seealso [domain_decomposition] for the returned object's slots,
#'   [hy_domain()] for the per-domain object, [validate_decomposition()],
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
                              trunk_metric = "drainage_area",
                              trunk_threshold = NULL,
                              trunk_levelpaths = NULL,
                              domain_breaks = NULL,
                              overrides = NULL) {

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

  missing_cols <- setdiff(
    c("id", "toid", "levelpath", "topo_sort", "levelpath_outlet_id"),
    names(x))

  if (length(missing_cols) > 0) {
    stop("decompose_network: missing required columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE)
  }

  x <- decompose_resolve_metric(x, trunk_metric, trunk_threshold,
    trunk_levelpaths)

  if (nrow(x) == 0) {
    return(structure(
      list(
        domains = list(),
        domain_connectivity = list(),
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
    ))
  }

  # Compute bridge ids from the non-dendritic network for trunk
  # segment boundary restriction. Confluences that are not bridges
  # (within diversion loops) do not create segment breaks.
  if (all(c("fromnode", "tonode") %in% names(x))) {
    nd_bridge_ids <- compute_nd_bridge_ids(x)
  } else {
    nd_bridge_ids <- NULL
  }

  if (!is.null(domain_breaks)) {
    domain_breaks <- as.character(domain_breaks)
    if (length(domain_breaks) == 0L) domain_breaks <- NULL
  }

  # Step 2 -- split the network into drainage basins. sort_network with
  # split = TRUE annotates each row with its terminal_id. We work
  # basin-by-basin so multi-basin sources (or single-basin, the
  # common case) funnel through the same code path.
  sorted <- sort_network(x, split = TRUE)

  terminal_ids <- unique(sorted$terminal_id)

  domains             <- list()
  domain_connectivity <- list()
  index_names         <- character(0)
  index_values        <- character(0)

  # Anchor the nexus accumulator with a typed empty stamp so the final
  # rbind yields the right shape even when no basin contributes
  # any nexuses. Edges are no longer materialized at decomposition
  # time; get_domain_graph() derives them from nexus_registry on demand.
  nexuses_list <- list(data.frame(
    nexus_id = character(0),
    from_domain_id = character(0),
    to_domain_id = character(0),
    trunk_catchment_id = character(0),
    aggregate_id_measure = numeric(0),
    stringsAsFactors = FALSE))

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

    trunk_ids <- select_trunk_ids(component, tid,
      trunk_metric, trunk_threshold, trunk_levelpaths)

    built <- decompose_build_component(component, tid, trunk_ids,
      nd_bridge_ids, domain_breaks)

    domains <- c(domains, built$domains)

    if (!is.null(built$connectivity)) {
      domain_connectivity[[as.character(tid)]] <- built$connectivity
    }

    nexuses_list[[length(nexuses_list) + 1L]] <- built$nexuses

    index_names  <- c(index_names,  built$index_names)
    index_values <- c(index_values, built$index_values)
  }

  nexus_registry <- do.call(rbind,
    nexuses_list[!vapply(nexuses_list, is.null, logical(1))])

  catchment_domain_index <- setNames(index_values, index_names)

  out <- structure(
    list(
      domains                = domains,
      domain_connectivity    = domain_connectivity,
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

#' Resolve and validate the metric column for trunk thresholding
#'
#' Runs once on the whole network before sort_network splits it into
#' components. Validates trunk_levelpaths entries, validates
#' trunk_threshold type, and ensures the metric column is present
#' (auto-computing total_da_sqkm from da_sqkm when possible).
#'
#' @param x hy_leveled network.
#' @param trunk_metric character. "drainage_area" or "arbolate_sum".
#' @param trunk_threshold numeric scalar or NULL.
#' @param trunk_levelpaths vector of levelpath ids or NULL.
#' @returns x, possibly with total_da_sqkm added.
#' @noRd
decompose_resolve_metric <- function(x, trunk_metric, trunk_threshold,
                                     trunk_levelpaths) {

  if (is.null(trunk_threshold) && is.null(trunk_levelpaths)) {
    return(x)
  }

  trunk_metric <- match.arg(trunk_metric,
    c("drainage_area", "arbolate_sum"))

  if (!is.null(trunk_levelpaths)) {

    unknown <- setdiff(trunk_levelpaths, unique(x$levelpath))

    if (length(unknown) > 0) {
      stop("decompose_network: trunk_levelpaths contains unknown ",
        "levelpath ids: ", paste(unknown, collapse = ", "),
        call. = FALSE)
    }
  }

  if (is.null(trunk_threshold)) {
    return(x)
  }

  if (!is.numeric(trunk_threshold) || length(trunk_threshold) != 1L ||
      !is.finite(trunk_threshold)) {
    stop("decompose_network: trunk_threshold must be a finite numeric ",
      "scalar.", call. = FALSE)
  }

  if (!"stream_calculator" %in% names(x)) {
    stop("decompose_network: trunk_threshold requires a ",
      "'stream_calculator' column to exclude diverted paths. ",
      "This column is typically provided by the source dataset ",
      "(e.g. NHDPlus StreamCalc).",
      call. = FALSE)
  }

  metric_col <- switch(trunk_metric,
    drainage_area = "total_da_sqkm",
    arbolate_sum  = "arbolate_sum")

  if (metric_col %in% names(x)) {
    return(x)
  }

  if (trunk_metric == "drainage_area") {

    if (!"da_sqkm" %in% names(x)) {
      stop("decompose_network: trunk_metric = \"drainage_area\" requires ",
        "either a 'total_da_sqkm' column or a 'da_sqkm' local-area ",
        "column on the input. Compute total_da_sqkm via:\n",
        "  x$total_da_sqkm <- accumulate_downstream(x, \"da_sqkm\")",
        call. = FALSE)
    }

    x$total_da_sqkm <- accumulate_downstream(x, "da_sqkm", quiet = TRUE)

    return(x)
  }

  stop("decompose_network: trunk_metric = \"arbolate_sum\" requires an ",
    "'arbolate_sum' column on the input. It is not auto-computed. Supply ",
    "it via add_levelpaths(weight_attribute = \"arbolate_sum\") or from ",
    "the source dataset's ArbolateSu column.",
    call. = FALSE)
}

#' Select trunk catchment ids for a single drainage basin
#'
#' Returns a character vector of catchment ids that belong in the
#' single trunk domain for this basin. Empty means the basin is too
#' small for a trunk (sub-threshold).
#'
#' @param component hy_leveled slice for a single drainage basin.
#' @param terminal_id scalar terminal outlet id of the basin.
#' @param trunk_metric character. "drainage_area" or "arbolate_sum".
#' @param trunk_threshold numeric scalar or NULL.
#' @param trunk_levelpaths vector of levelpath ids or NULL.
#' @returns character vector of catchment ids.
#' @noRd
select_trunk_ids <- function(component, terminal_id,
                             trunk_metric, trunk_threshold,
                             trunk_levelpaths) {

  outlet_row <- component[component$id == terminal_id, , drop = FALSE]

  if (nrow(outlet_row) != 1L) {
    stop("decompose_network: drainage basin with terminal_id '",
      terminal_id, "' does not have a unique outlet row",
      call. = FALSE)
  }

  outlet_lp <- outlet_row$levelpath

  # No-arg fallback: one trunk per basin = the outlet's levelpath.
  if (is.null(trunk_threshold) && is.null(trunk_levelpaths)) {
    return(as.character(component$id[component$levelpath == outlet_lp]))
  }

  # Explicit override path: all catchments on the forced levelpaths.
  if (!is.null(trunk_levelpaths)) {

    forced <- intersect(trunk_levelpaths, unique(component$levelpath))
    lps <- unique(c(forced, outlet_lp))

    return(as.character(component$id[component$levelpath %in% lps]))
  }

  # Threshold rule.
  metric_col <- switch(trunk_metric,
    drainage_area = "total_da_sqkm",
    arbolate_sum  = "arbolate_sum")

  # Basin too small for a trunk -- return empty so the component becomes
  # a single domain.
  outlet_metric <- outlet_row[[metric_col]]

  if (is.na(outlet_metric) || outlet_metric <= trunk_threshold) {
    return(character(0))
  }

  # All catchments whose metric exceeds the threshold, excluding
  # diverted paths (stream_calculator == 0).
  above <- component[[metric_col]] > trunk_threshold

  sc <- component$stream_calculator
  above <- above & !is.na(sc) & sc != 0

  as.character(component$id[above])
}

#' Domain decomposition object
#'
#' @description
#' A `domain_decomposition` is the wrapper object returned by
#' [decompose_network()]. It bundles a list of [hy_domain()] objects
#' with the basin-level extensive connectivity overlays and the
#' nexus metadata that recomposition needs.
#'
#' @details
#' The object is a plain S3 list with six slots:
#'
#' \describe{
#'   \item{`domains`}{named list of `hy_domain` objects, one per
#'     sub-network.}
#'   \item{`domain_connectivity`}{named list of `hy_leveled` overlays
#'     keyed by basin id. Each overlay is the basin's *extensive
#'     connectivity* — a `hy_leveled` view of the main path with
#'     `toid`s intact except at the basin outlet, which carries the
#'     outlet sentinel. Sub-threshold basins have no overlay.}
#'   \item{`overrides`}{non-dendritic inter-domain transfer table, or
#'     `NULL`.}
#'   \item{`catchment_domain_index`}{named character vector mapping
#'     each catchment id to its domain id.}
#'   \item{`nexus_registry`}{synthetic nexus identifiers and the
#'     domains they connect.}
#'   \item{`source_network`}{the original enriched input network.}
#' }
#'
#' Inter-domain topology is not stored as a slot; [get_domain_graph()]
#' is the canonical derived accessor and rebuilds the edge list from
#' `nexus_registry` on demand.
#'
#' @seealso [decompose_network()] for construction, [hy_domain()] for
#'   the per-domain object, [validate_decomposition()] for structural
#'   checks, [get_domain_graph()] and [get_domain_for_catchment()] for
#'   accessors, [print.domain_decomposition()] for the print method.
#' @name domain_decomposition
#' @aliases domain_decomposition
NULL

#' Print a domain_decomposition
#'
#' @description
#' Two-mode S3 print method for `domain_decomposition`. The default
#' (cheap) form prints a fixed-size summary of slot counts: basins,
#' domains, catchments, nexuses, and overrides. Cost is bounded by the
#' number of domains, so it stays fast on 50,000-catchment
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
    "<domain_decomposition: %s basins, %s domains, %s catchments>\n",
    fmt(counts$n_basins), fmt(counts$n_domains), fmt(counts$n_catch)))

  cat(sprintf("  %-21s %s\n",
    "domains:",             fmt(counts$n_domains)))

  cat(sprintf("  %-21s %s basins\n",
    "domain_connectivity:", fmt(counts$n_basins)))

  cat(sprintf("  %-21s %s nexuses\n",
    "nexus_registry:",      fmt(counts$n_nexus)))

  cat(sprintf("  %-21s %s rows\n",
    "overrides:",           fmt(counts$n_override)))

  cat(sprintf("  %-21s %s catchments\n",
    "source_network:",      fmt(counts$n_catch)))

  cat("\n# Use print(x, full = TRUE) for the full tree summary\n")
}

#' Compute the headline counts used by both print modes.
#' Bounded by O(length(domains)); never opens a catchment table.
#' @noRd
decomposition_counts <- function(x) {

  list(
    n_basins   = length(x$domain_connectivity),
    n_domains  = length(x$domains),
    n_catch    = if (is.null(x$source_network)) 0L
                 else nrow(x$source_network),
    n_nexus    = if (is.null(x$nexus_registry)) 0L
                 else nrow(x$nexus_registry),
    n_override = if (is.null(x$overrides)) 0L
                 else nrow(x$overrides)
  )
}

#' Full-mode print: hierarchical tree with per-block roll-up stats.
#'
#' Six top-level slots in fixed order: source_network, domains (with
#' single domain roll-up), domain_connectivity, nexus_registry,
#' catchment_domain_index, overrides. Slot names align at column 4,
#' type tags at column 26, counts right-aligned after.
#' @noRd
print_domain_decomposition_full <- function(x, width) {

  counts <- decomposition_counts(x)

  fmt <- function(n) format(n, big.mark = ",")

  # Header line.
  cat(sprintf(
    "<domain_decomposition: %s basins, %s domains, %s catchments>\n",
    fmt(counts$n_basins), fmt(counts$n_domains), fmt(counts$n_catch)))

  # Slot 1: source_network
  print_slot_line(
    branch     = "\u251c\u2500",
    name       = "source_network",
    type_tag   = paste0("<", primary_class(x$source_network), ">"),
    count_text = paste0(fmt(counts$n_catch), " rows"))

  # Slot 2: domains (with attribute roll-up across all domains).
  print_slot_line(
    branch     = "\u251c\u2500",
    name       = "domains",
    type_tag   = "<list>",
    count_text = paste0(fmt(counts$n_domains), " elements"))

  if (counts$n_domains > 0L) {
    print_domains_rollup(x, counts, fmt)
  }

  # Slot 3: domain_connectivity
  print_slot_line(
    branch     = "\u251c\u2500",
    name       = "domain_connectivity",
    type_tag   = "<list>",
    count_text = paste0(fmt(counts$n_basins), " elements"))

  if (counts$n_basins > 0L) {
    print_domain_connectivity_block(x, fmt)
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

  if (counts$n_domains > 0L) {

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

#' Domains roll-up sub-tree: single attribute roll-up across all domains.
#' @noRd
print_domains_rollup <- function(x, counts, fmt) {

  cat("\u2502  \u2502\n")
  cat(sprintf("\u2502  \u2514\u2500 <%s domains>\n", fmt(length(x$domains))))

  print_domain_block_attrs(x$domains, fmt, cont_char = " ")

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

#' domain_connectivity sub-block: per-basin overlay-size summary.
#' @noRd
print_domain_connectivity_block <- function(x, fmt) {

  conn <- x$domain_connectivity

  sizes <- vapply(conn, function(o) nrow(o), integer(1))

  prefix <- "\u2502     "

  print_attr_line(prefix, "catchments", sizes, fmt, show_total = TRUE)

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

#' Assign trunk catchments to segments between confluences
#'
#' A segment is a maximal linear chain of trunk catchments between two
#' trunk confluences (or between a headwater and the first confluence,
#' or the last confluence and the outlet). Returns a named character
#' vector mapping each trunk catchment id to its segment id (the
#' downstream confluence or outlet that terminates the segment).
#'
#' @param trunk_ids_chr character vector of trunk catchment ids.
#' @param trunk_toids_chr character vector of toid for each trunk
#'   catchment (parallel to trunk_ids_chr).
#' @param bridge_ids character vector of bridge flowline ids from
#'   the non-dendritic network, or NULL. When supplied, only
#'   confluences that are also bridges create segment breaks.
#' @returns named character vector: names = trunk catchment ids,
#'   values = segment id (confluence or outlet catchment id).
#' @noRd
trunk_segment_ids <- function(trunk_ids_chr, trunk_toids_chr,
                              bridge_ids = NULL,
                              extra_terminals = NULL) {

  # In-degree within the trunk subgraph.
  targets_in_trunk <- trunk_toids_chr[trunk_toids_chr %in% trunk_ids_chr]
  in_deg <- table(targets_in_trunk)
  confluences <- names(in_deg[in_deg >= 2L])

  # Restrict to bridge confluences when bridge ids are available.
  # Non-bridge confluences (within diversion loops) are absorbed
  # into the surrounding segment.
  if (!is.null(bridge_ids)) {
    confluences <- confluences[confluences %in% bridge_ids]
  }

  # Terminals: confluences + outlets (toid not in trunk).
  outlets <- trunk_ids_chr[!trunk_toids_chr %in% trunk_ids_chr]
  terminals <- union(confluences, outlets)

  # Layer user-supplied breaks on top of auto-detected terminals.
  if (!is.null(extra_terminals)) {
    terminals <- union(terminals, extra_terminals)
  }

  # Walk each trunk catchment downstream to the first terminal.
  seg <- setNames(rep(NA_character_, length(trunk_ids_chr)), trunk_ids_chr)
  toid_lookup <- setNames(trunk_toids_chr, trunk_ids_chr)

  for (tid in trunk_ids_chr) {

    cur <- tid
    while (!cur %in% terminals) {
      cur <- toid_lookup[[cur]]
    }
    seg[[tid]] <- cur
  }

  seg
}

#' Compute bridge flowline ids from the non-dendritic network
#'
#' Rebuilds non-dendritic edges from `fromnode`/`tonode` and runs
#' `get_bridge_flowlines()`. Used to restrict trunk segment breaks
#' to confluences that are also bridge flowlines.
#'
#' @param x hy_leveled with fromnode and tonode columns.
#' @returns character vector of bridge flowline ids.
#' @noRd
compute_nd_bridge_ids <- function(x) {

  from_lookup <- split(x$id, x$fromnode)

  nd_list <- lapply(seq_len(nrow(x)), function(i) {

    tn <- as.character(x$tonode[i])
    downstream <- from_lookup[[tn]]

    if (is.null(downstream) || length(downstream) == 0) {
      data.frame(id = x$id[i], toid = 0)
    } else {
      data.frame(id = rep(x$id[i], length(downstream)),
        toid = downstream)
    }
  })

  nd_edges <- do.call(rbind, nd_list)

  as.character(get_bridge_flowlines(nd_edges, quiet = TRUE))
}

#' Build one drainage basin's domains and extensive connectivity overlay
#'
#' @param component hy_leveled slice for a single drainage basin.
#' @param terminal_id scalar terminal outlet id of the basin.
#' @param trunk_ids character vector of catchment ids that lie on the
#'   basin's main path. Empty means the basin is sub-threshold (no
#'   main-path-based segmentation; the whole component is one domain).
#' @param nd_bridge_ids character vector of bridge flowline ids from
#'   the non-dendritic network, or NULL. When supplied, main-path
#'   segment breaks are restricted to confluences that are also bridges.
#' @returns list with `domains`, `connectivity`, `nexuses`, and two
#'   parallel vectors for the catchment_domain_index. `connectivity` is
#'   the basin's extensive connectivity overlay (a `hy_leveled` view of
#'   the main path with toids intact except for the basin outlet
#'   sentinel) or `NULL` when the basin is sub-threshold.
#' @noRd
decompose_build_component <- function(component, terminal_id,
                                      trunk_ids,
                                      nd_bridge_ids = NULL,
                                      domain_breaks = NULL) {

  # --- Zero-trunk shortcut: entire component is one domain. ---------

  if (length(trunk_ids) == 0L) {

    dom_id    <- paste0("domain_", terminal_id)
    outlet_nx <- paste0("nx_outlet_", terminal_id)

    out_sentinel <- get_outlet_value(component)
    component$toid[component$id == terminal_id] <- out_sentinel
    component <- classify_hy(component)

    dom <- hy_domain(
      domain_id            = dom_id,
      outlet_nexus_id      = outlet_nx,
      inlet_nexus_ids      = character(0),
      containing_domain_id = NA_character_,
      catchments           = component,
      topo_sort_offset     = 0L)

    nexus_row <- data.frame(
      nexus_id             = outlet_nx,
      from_domain_id       = dom_id,
      to_domain_id         = NA_character_,
      trunk_catchment_id   = as.character(terminal_id),
      aggregate_id_measure = NA_real_,
      stringsAsFactors     = FALSE)

    return(list(
      domains      = setNames(list(dom), dom_id),
      connectivity = NULL,
      nexuses      = nexus_row,
      index_names  = as.character(component$id),
      index_values = rep(dom_id, nrow(component))
    ))
  }

  # --- A. Trunk / residual split. ----------------------------------

  trunk_mask <- as.character(component$id) %in% trunk_ids
  residual   <- component[!trunk_mask, , drop = FALSE]

  # Original toid of every catchment in the component, before any
  # outlet-sentinel rewriting. Used to determine inter-domain handoff
  # targets and to drive recomposition's toid restoration.
  comp_toid_lookup <- setNames(
    as.character(component$toid), as.character(component$id))

  # --- B. Compute main-path segments. --------------------------------
  # A segment is a maximal linear chain of main-path catchments between
  # two confluences (or between a headwater/confluence and the outlet).
  # In the decomposed form, every segment becomes a domain -- including
  # segments with no lateral tributaries.

  trunk_ids_chr   <- as.character(component$id[trunk_mask])
  trunk_toids_chr <- as.character(component$toid[trunk_mask])

  if (!is.null(domain_breaks)) {
    seg_map <- trunk_segment_ids(trunk_ids_chr, trunk_toids_chr,
      extra_terminals = domain_breaks)
  } else {
    seg_map <- trunk_segment_ids(trunk_ids_chr, trunk_toids_chr,
      nd_bridge_ids)
  }

  segment_ids <- unique(unname(seg_map))

  # --- C. Build the basin's extensive connectivity overlay. ---------
  # An hy_leveled view of the main path with toids intact except for
  # the basin outlet, which carries the outlet sentinel.

  trunk_slice <- component[trunk_mask, , drop = FALSE]

  out_sentinel_trunk <- get_outlet_value(trunk_slice)
  trunk_slice$toid[trunk_slice$id == terminal_id] <- out_sentinel_trunk
  trunk_slice <- classify_hy(trunk_slice)

  # --- D. Build a domain for each segment. --------------------------

  residual_from_idx <- if (nrow(residual) > 0L) {
    split(residual$id, residual$toid)
  } else {
    list()
  }

  # Pre-compute per-segment lateral seeds so we don't re-scan residual
  # for each segment.
  seeds_per_segment <- if (nrow(residual) > 0L) {
    seed_targets <- as.character(residual$toid[
      as.character(residual$toid) %in% trunk_ids_chr])
    seed_ids     <- residual$id[
      as.character(residual$toid) %in% trunk_ids_chr]
    seed_segs    <- seg_map[seed_targets]
    split(as.character(seed_ids), seed_segs)
  } else {
    list()
  }

  nexuses_list <- list()
  inlet_by_dom <- list()
  seg_data     <- list()

  for (seg_id in segment_ids) {

    seg_id_chr <- as.character(seg_id)

    # Main-path catchments belonging to this segment.
    seg_trunk_ids <- names(seg_map)[seg_map == seg_id]

    # Lateral seeds draining into this segment (may be empty).
    seeds_in_seg <- seeds_per_segment[[seg_id_chr]]

    if (is.null(seeds_in_seg)) seeds_in_seg <- character(0)

    # Collect residual lateral catchments upstream of the seeds.
    lateral_ids <- character(0)
    for (seed in seeds_in_seg) {
      up <- decompose_collect_upstream(residual, seed, residual_from_idx)
      lateral_ids <- union(lateral_ids, as.character(up))
    }

    # Domain slice = laterals + main-path catchments in segment.
    dom_catch_ids <- c(lateral_ids, seg_trunk_ids)

    dom_slice <- component[
      as.character(component$id) %in% dom_catch_ids, , drop = FALSE]

    # Drop main-path rows' toids to the outlet sentinel. Each becomes a
    # local outlet of its own contributing sub-basin. Lateral rows keep
    # their natural toids -- they point to in-domain main-path rows.
    # Main-path membership is recoverable by intersecting the domain's
    # ids with the basin's domain_connectivity overlay; no marker
    # column is needed.
    cs_sentinel <- get_outlet_value(dom_slice)
    dom_slice$toid[
      as.character(dom_slice$id) %in% seg_trunk_ids] <- cs_sentinel

    dom_slice <- classify_hy(dom_slice)

    dom_id <- paste0("domain_", terminal_id, "_", seg_id)

    # Determine the segment's outflow target. The segment's
    # downstream-most main-path catchment is the segment id itself; its
    # original toid is where the domain hands off.
    seg_terminal_toid <- comp_toid_lookup[[seg_id_chr]]

    is_basin_outlet <- !(seg_terminal_toid %in% trunk_ids_chr)

    if (is_basin_outlet) {

      primary_nexus_id <- paste0("nx_", seg_id_chr, "_outlet")

      nexuses_list[[length(nexuses_list) + 1L]] <- data.frame(
        nexus_id             = primary_nexus_id,
        from_domain_id       = dom_id,
        to_domain_id         = NA_character_,
        trunk_catchment_id   = seg_id_chr,
        aggregate_id_measure = NA_real_,
        stringsAsFactors     = FALSE)

    } else {

      downstream_seg_id <- unname(seg_map[seg_terminal_toid])
      downstream_dom_id <- paste0("domain_", terminal_id,
        "_", downstream_seg_id)

      primary_nexus_id <- paste0("nx_", seg_id_chr,
        "_", seg_terminal_toid)

      nexuses_list[[length(nexuses_list) + 1L]] <- data.frame(
        nexus_id             = primary_nexus_id,
        from_domain_id       = dom_id,
        to_domain_id         = downstream_dom_id,
        trunk_catchment_id   = seg_terminal_toid,
        aggregate_id_measure = NA_real_,
        stringsAsFactors     = FALSE)

      inlet_by_dom[[downstream_dom_id]] <- c(
        inlet_by_dom[[downstream_dom_id]] %||% character(0),
        primary_nexus_id)
    }

    seg_data[[dom_id]] <- list(
      domain_id       = dom_id,
      outlet_nexus_id = primary_nexus_id,
      catchments      = dom_slice,
      catch_ids       = dom_catch_ids
    )
  }

  # --- E. Second pass: build hy_domain instances now that
  # inlet_by_dom is fully populated. --------------------------------

  domains      <- list()
  index_names  <- character(0)
  index_values <- character(0)

  for (dom_id in names(seg_data)) {

    info <- seg_data[[dom_id]]

    domains[[dom_id]] <- hy_domain(
      domain_id            = dom_id,
      outlet_nexus_id      = info$outlet_nexus_id,
      inlet_nexus_ids      = inlet_by_dom[[dom_id]] %||% character(0),
      containing_domain_id = NA_character_,
      catchments           = info$catchments,
      topo_sort_offset     = 0L)

    index_names  <- c(index_names, info$catch_ids)
    index_values <- c(index_values,
      rep(dom_id, length(info$catch_ids)))
  }

  # --- F. Return. --------------------------------------------------

  list(
    domains      = domains,
    connectivity = trunk_slice,
    nexuses      = do.call(rbind,
      nexuses_list[vapply(nexuses_list, is.data.frame, logical(1))]),
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
#' Uses a pre-built inverted index (toid -> id) for O(n) total work
#' instead of repeated `%in%` scans.
#'
#' @param residual data.frame with id, toid columns (the non-trunk
#'   rows of a drainage basin).
#' @param seed scalar catchment id to start from.
#' @param from_idx pre-built inverted index (output of
#'   `split(residual$id, residual$toid)`). Built once per component
#'   and reused across seeds.
#' @returns vector of ids in the same type as residual$id, including seed.
#' @noRd
decompose_collect_upstream <- function(residual, seed, from_idx) {

  if (nrow(residual) == 0L) return(seed)

  collected <- seed
  frontier  <- seed

  while (length(frontier) > 0L) {

    next_hop <- unlist(from_idx[as.character(frontier)], use.names = FALSE)

    if (is.null(next_hop) || length(next_hop) == 0L) break

    next_hop <- next_hop[!next_hop %in% collected]

    if (length(next_hop) == 0L) break

    collected <- c(collected, next_hop)
    frontier  <- next_hop
  }

  collected
}

#' Empty inter-domain edge data.frame
#'
#' Shared shape for the zero-row return from `get_domain_graph()` and
#' the placeholder for not-yet-wired containment edges.
#' @noRd
empty_graph_df <- function() {

  data.frame(
    id = character(0),
    toid = character(0),
    nexus_id = character(0),
    nexus_position = numeric(0),
    relation_type = character(0),
    stringsAsFactors = FALSE)
}

#' Get the inter-domain edge list from a decomposition
#'
#' @description
#' Derives the inter-domain graph from `decomposition$nexus_registry`
#' and returns it as a hydroloom edge list, filtered by relation type.
#' Each registry row whose `to_domain_id` is non-NA contributes one
#' flow edge from `from_domain_id` to `to_domain_id`. Containment edges
#' are not yet emitted. The default includes both flow and containment
#' relations; pass `relations = "flow"` to get the dendritic flow DAG
#' only.
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
#' @seealso [domain_decomposition] for the wrapper object,
#'   [decompose_network()].
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

  reg <- decomposition$nexus_registry

  if (is.null(reg) || nrow(reg) == 0L) {
    return(classify_hy(empty_graph_df()))
  }

  is_inter <- !is.na(reg$to_domain_id)

  flow <- data.frame(
    id             = reg$from_domain_id[is_inter],
    toid           = reg$to_domain_id[is_inter],
    nexus_id       = reg$nexus_id[is_inter],
    nexus_position = reg$aggregate_id_measure[is_inter],
    relation_type  = rep("flow", sum(is_inter)),
    stringsAsFactors = FALSE)

  # Containment edges land here once contained_basins is wired in
  # (Layer 7); for now there are none.
  contained <- empty_graph_df()

  combined <- rbind(flow, contained)

  out <- combined[combined$relation_type %in% relations, , drop = FALSE]

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
#' @seealso [domain_decomposition] for the wrapper object,
#'   [decompose_network()].
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
