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
#   - stem selection: single-outlet-levelpath default, stem_threshold
#     metric-based multi-stem, stem_levelpaths explicit override
#   - one domain per lateral inflow point on a stem, carrying the
#     maximal upstream sub-network of that lateral
#   - synthetic nexus ids; inter-domain edges derived from nexus_registry
#   - print method (cheap + full modes)

#' Decompose a network into domains
#'
#' @description
#' Partitions a hydrologic network into `hy_domain` objects for
#' independent or parallel computation. Each drainage basin is split
#' into domains along its extensive network; the extensive network
#' itself is returned separately as the basin's *domain_connectivity*
#' overlay. See [hy_domain()] and [domain_decomposition] for details.
#'
#' @details
#' **Input.** Input must be `hy_leveled` -- the network must already
#' carry `levelpath`, `levelpath_outlet_id`, and `topo_sort` columns.
#' Call [add_levelpaths()] to add. Non-dendritic sources
#' (`hy_flownetwork`) are not supported at this time.
#'
#' **Extensive network selection.** Each drainage basin's extensive
#' network is selected from `stem_metric`, `stem_threshold`, and
#' `stem_levelpaths` (see arguments). The basin's extensive connectivity
#' (the regional relationship that ties its drainage pieces together) is
#' materialized as the basin's `domain_connectivity[[basin_id]]` overlay
#' — a `hy_leveled` view of the extensive network. The `stem_*` naming
#' reflects the cross-scale view: a basin's outlet levelpath is a trunk,
#' but with a threshold or explicit override the selection can pull in
#' branches as well — every selected path is a *stem* in the cross-scale
#' sense (trunks and branches are both stems).
#'
#' **Containment.** This function does not detect containment. A
#' drainage basin that the caller wants treated as belonging inside
#' another -- typically an endorheic basin or a drainage-divide
#' remnant -- is partitioned here as an independent basin like any
#' other. After decomposition, the caller declares the relationship
#' with [set_containment()]; [recompose()] applies it when called
#' with `containment = "accumulate"`. The relationship is recorded on
#' the contained domain's `containing_domain_id` slot and surfaced by
#' [get_domain_graph()] with `relations = "contained"`. It does not
#' appear in `nexus_registry` because no flow crosses a hydro nexus
#' between the two basins.
#'
#' @param x `hy_leveled` object (dendritic network already enriched
#'   with levelpaths).
#' @param stem_metric character. Metric evaluated at each levelpath
#'   outlet to decide stem eligibility. `"drainage_area"` reads
#'   `total_da_sqkm`; `"arbolate_sum"` reads `arbolate_sum`. Only
#'   consulted when `stem_threshold` is non-NULL.
#' @param stem_threshold numeric scalar or `NULL`. Value of
#'   `stem_metric` at a levelpath outlet above which the levelpath is
#'   a stem candidate. `NULL` (default) falls back to one stem per
#'   drainage basin (the basin's outlet levelpath). When non-NULL,
#'   the input must carry a `stream_calculator` column; call
#'   [add_streamorder()] to add it if the source data does not
#'   already provide one (NHDPlus `StreamCalc`, canonicalized by
#'   [hy()], counts).
#' @param stem_levelpaths vector of levelpath ids or `NULL`. When
#'   non-NULL, bypasses the threshold rule and forces these levelpaths
#'   to be stems (the basin's terminal-outlet levelpath is always unioned
#'   in). Every id must exist in `x$levelpath`.
#' @param domain_breaks vector of catchment ids or `NULL`. When
#'   non-NULL, these stem catchment ids define where the stem network
#'   is segmented into domain groups. Each break id becomes a
#'   segment terminal in addition to the auto-detected confluences and
#'   outlets. Breaks that are not stem catchments in a given basin
#'   are silently ignored. When `NULL` (default), segmentation is
#'   determined automatically from stem confluences and (if available)
#'   bridge flowlines.
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
                              stem_metric = "drainage_area",
                              stem_threshold = NULL,
                              stem_levelpaths = NULL,
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

  x <- decompose_resolve_metric(x, stem_metric, stem_threshold,
    stem_levelpaths)

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
          stringsAsFactors = FALSE),
        source_network = x
      ),
      class = "domain_decomposition"
    ))
  }

  # Compute bridge ids from the non-dendritic network for stem
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

    stem_ids <- select_stem_ids(component, tid,
      stem_metric, stem_threshold, stem_levelpaths)

    built <- decompose_build_component(component, tid, stem_ids,
      nd_bridge_ids, domain_breaks)

    domains <- c(domains, built$domains)

    if (!is.null(built$connectivity)) {
      domain_connectivity[[as.character(tid)]] <- built$connectivity
    }

    nexuses_list[[length(nexuses_list) + 1L]] <- built$nexuses

    # NOTE (perf): c()-accumulation per basin is O(n^2) in the total
    # number of catchments. Fine at the 100-domain target; at finer
    # grain pre-allocate index_names / index_values to the known
    # total size (sum of nrow(catchments) across domains) or
    # collect into a list and unlist() once at the end of the loop.
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

#' Resolve and validate the metric column for stem thresholding
#'
#' Runs once on the whole network before sort_network splits it into
#' components. Validates stem_levelpaths entries, validates
#' stem_threshold type, and ensures the metric column is present
#' (auto-computing total_da_sqkm from da_sqkm when possible).
#'
#' @param x hy_leveled network.
#' @param stem_metric character. "drainage_area" or "arbolate_sum".
#' @param stem_threshold numeric scalar or NULL.
#' @param stem_levelpaths vector of levelpath ids or NULL.
#' @returns x, possibly with total_da_sqkm added.
#' @noRd
decompose_resolve_metric <- function(x, stem_metric, stem_threshold,
                                     stem_levelpaths) {

  if (is.null(stem_threshold) && is.null(stem_levelpaths)) {
    return(x)
  }

  stem_metric <- match.arg(stem_metric,
    c("drainage_area", "arbolate_sum"))

  if (!is.null(stem_levelpaths)) {

    unknown <- setdiff(stem_levelpaths, unique(x$levelpath))

    if (length(unknown) > 0) {
      stop("decompose_network: stem_levelpaths contains unknown ",
        "levelpath ids: ", paste(unknown, collapse = ", "),
        call. = FALSE)
    }
  }

  if (is.null(stem_threshold)) {
    return(x)
  }

  if (!is.numeric(stem_threshold) || length(stem_threshold) != 1L ||
      !is.finite(stem_threshold)) {
    stop("decompose_network: stem_threshold must be a finite numeric ",
      "scalar.", call. = FALSE)
  }

  if (!"stream_calculator" %in% names(x)) {
    stop("decompose_network: stem_threshold requires a ",
      "'stream_calculator' column to exclude diverted paths. ",
      "This column is typically provided by the source dataset ",
      "(e.g. NHDPlus StreamCalc).",
      call. = FALSE)
  }

  metric_col <- switch(stem_metric,
    drainage_area = "total_da_sqkm",
    arbolate_sum  = "arbolate_sum")

  if (metric_col %in% names(x)) {
    return(x)
  }

  if (stem_metric == "drainage_area") {

    if (!"da_sqkm" %in% names(x)) {
      stop("decompose_network: stem_metric = \"drainage_area\" requires ",
        "either a 'total_da_sqkm' column or a 'da_sqkm' local-area ",
        "column on the input. Compute total_da_sqkm via:\n",
        "  x$total_da_sqkm <- accumulate_downstream(x, \"da_sqkm\")",
        call. = FALSE)
    }

    x$total_da_sqkm <- accumulate_downstream(x, "da_sqkm", quiet = TRUE)

    return(x)
  }

  stop("decompose_network: stem_metric = \"arbolate_sum\" requires an ",
    "'arbolate_sum' column on the input. It is not auto-computed. Supply ",
    "it via add_levelpaths(weight_attribute = \"arbolate_sum\") or from ",
    "the source dataset's ArbolateSu column.",
    call. = FALSE)
}

#' Select stem catchment ids for a single drainage basin
#'
#' Returns a character vector of catchment ids that lie on the basin's
#' stem network. Empty means the basin is too small (sub-threshold) and
#' should become a single domain with no extensive network overlay.
#'
#' @param component hy_leveled slice for a single drainage basin.
#' @param terminal_id scalar terminal outlet id of the basin.
#' @param stem_metric character. "drainage_area" or "arbolate_sum".
#' @param stem_threshold numeric scalar or NULL.
#' @param stem_levelpaths vector of levelpath ids or NULL.
#' @returns character vector of catchment ids.
#' @noRd
select_stem_ids <- function(component, terminal_id,
                             stem_metric, stem_threshold,
                             stem_levelpaths) {

  outlet_row <- component[component$id == terminal_id, , drop = FALSE]

  if (nrow(outlet_row) != 1L) {
    stop("decompose_network: drainage basin with terminal_id '",
      terminal_id, "' does not have a unique outlet row",
      call. = FALSE)
  }

  outlet_lp <- outlet_row$levelpath

  # No-arg fallback: stem network is the basin's outlet levelpath.
  if (is.null(stem_threshold) && is.null(stem_levelpaths)) {
    return(as.character(component$id[component$levelpath == outlet_lp]))
  }

  # Explicit override path: all catchments on the forced levelpaths.
  if (!is.null(stem_levelpaths)) {

    forced <- intersect(stem_levelpaths, unique(component$levelpath))
    lps <- unique(c(forced, outlet_lp))

    return(as.character(component$id[component$levelpath %in% lps]))
  }

  # Threshold rule.
  metric_col <- switch(stem_metric,
    drainage_area = "total_da_sqkm",
    arbolate_sum  = "arbolate_sum")

  # Basin sub-threshold -- return empty so the component becomes a
  # single domain with no extensive network overlay.
  outlet_metric <- outlet_row[[metric_col]]

  if (is.na(outlet_metric) || outlet_metric <= stem_threshold) {
    return(character(0))
  }

  # All catchments whose metric exceeds the threshold, excluding
  # diverted paths (stream_calculator == 0).
  above <- component[[metric_col]] > stem_threshold

  sc <- component$stream_calculator
  above <- above & !is.na(sc) & sc != 0

  as.character(component$id[above])
}

#' Domain decomposition object
#'
#' @description
#' A `domain_decomposition` is the wrapper object returned by
#' [decompose_network()]. It bundles a list of [hy_domain()] objects
#' with the basin-level extensive network overlays and the
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
#'     network* — a `hy_leveled` view of the connecting flowlines with
#'     `toid`s intact except at the basin outlet, which carries the
#'     reserved outlet `toid` value. Sub-threshold basins have no
#'     overlay.}
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
#' @param x object of class `domain_decomposition`.
#' @param full logical. `FALSE` (default) prints the cheap summary;
#'   `TRUE` prints the full hierarchical tree.
#' @param ... ignored.
#' @returns `x`, invisibly.
#' @export
print.domain_decomposition <- function(x, full = FALSE, ...) {

  if (isTRUE(full)) {
    print_domain_decomposition_full(x)
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
print_domain_decomposition_full <- function(x) {

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

#' Assign stem catchments to segments between confluences
#'
#' A segment is a maximal linear chain of stem catchments between two
#' stem confluences (or between a headwater and the first confluence,
#' or the last confluence and the outlet). Returns a named character
#' vector mapping each stem catchment id to its segment id (the
#' downstream confluence or outlet that terminates the segment).
#'
#' @param stem_ids_chr character vector of stem catchment ids.
#' @param stem_toids_chr character vector of toid for each stem
#'   catchment (parallel to stem_ids_chr).
#' @param bridge_ids character vector of bridge flowline ids from
#'   the non-dendritic network, or NULL. When supplied, only
#'   confluences that are also bridges create segment breaks.
#' @returns named character vector: names = stem catchment ids,
#'   values = segment id (confluence or outlet catchment id).
#' @noRd
stem_segment_ids <- function(stem_ids_chr, stem_toids_chr,
                              bridge_ids = NULL,
                              extra_terminals = NULL) {

  # In-degree within the stem subgraph.
  targets_in_stem <- stem_toids_chr[stem_toids_chr %in% stem_ids_chr]
  in_deg <- table(targets_in_stem)
  confluences <- names(in_deg[in_deg >= 2L])

  # Restrict to bridge confluences when bridge ids are available.
  # Non-bridge confluences (within diversion loops) are absorbed
  # into the surrounding segment.
  if (!is.null(bridge_ids)) {
    confluences <- confluences[confluences %in% bridge_ids]
  }

  # Terminals: confluences + outlets (toid not in stem).
  outlets <- stem_ids_chr[!stem_toids_chr %in% stem_ids_chr]
  terminals_set <- union(confluences, outlets)

  # Layer user-supplied breaks on top of auto-detected terminals.
  if (!is.null(extra_terminals)) {
    terminals_set <- union(terminals_set, extra_terminals)
  }

  is_terminal <- stem_ids_chr %in% terminals_set

  # One-step lookup: for terminals, point at self (fixed point); for
  # non-terminals, point at toid (always in stem, since otherwise the
  # row would have been classified as an outlet and is_terminal would
  # be TRUE).
  next_hop <- ifelse(is_terminal, stem_ids_chr, stem_toids_chr)
  step <- setNames(next_hop, stem_ids_chr)

  # Path doubling: each pass squares the per-id traversal distance, so
  # ceil(log2(L)) iterations resolve every id to its segment terminal.
  # O(N log L) total vs. the per-id walk's O(N L).
  repeat {

    new_step <- setNames(step[step], names(step))

    if (identical(unname(new_step), unname(step))) break

    step <- new_step
  }

  step
}

#' Compute bridge flowline ids from the non-dendritic network
#'
#' Rebuilds non-dendritic edges from `fromnode`/`tonode` and runs
#' `get_bridge_flowlines()`. Used to restrict stem segment breaks
#' to confluences that are also bridge flowlines.
#'
#' @param x hy_leveled with fromnode and tonode columns.
#' @returns character vector of bridge flowline ids.
#' @noRd
compute_nd_bridge_ids <- function(x) {

  from_lookup <- split(x$id, x$fromnode)

  # Look up the downstream id list for every row in one shot rather
  # than per-row data.frame allocation -- the per-row form was the
  # decomposition's dominant constant factor at NHDPlus scale.
  downstream_per_row <- from_lookup[as.character(x$tonode)]

  lens <- lengths(downstream_per_row)

  has_down <- lens > 0L

  ids_with_down   <- rep(x$id[has_down], lens[has_down])
  toids_with_down <- unlist(downstream_per_row[has_down], use.names = FALSE)

  ids_outlets   <- x$id[!has_down]
  toids_outlets <- rep(0, length(ids_outlets))

  nd_edges <- data.frame(
    id   = c(ids_with_down, ids_outlets),
    toid = c(toids_with_down, toids_outlets),
    stringsAsFactors = FALSE)

  as.character(get_bridge_flowlines(nd_edges, quiet = TRUE))
}

#' Build one drainage basin's domains and extensive network overlay
#'
#' @param component hy_leveled slice for a single drainage basin.
#' @param terminal_id scalar terminal outlet id of the basin.
#' @param stem_ids character vector of catchment ids that lie on the
#'   basin's extensive network. Empty means the basin is sub-threshold
#'   (no extensive-network-based segmentation; the whole component is
#'   one domain).
#' @param nd_bridge_ids character vector of bridge flowline ids from
#'   the non-dendritic network, or NULL. When supplied, extensive-network
#'   segment breaks are restricted to confluences that are also bridges.
#' @returns list with `domains`, `connectivity`, `nexuses`, and two
#'   parallel vectors for the catchment_domain_index. `connectivity` is
#'   the basin's extensive network overlay (a `hy_leveled` view of
#'   the connecting flowlines with toids intact except for the basin
#'   outlet, which carries the reserved outlet `toid` value) or `NULL`
#'   when the basin is sub-threshold.
#' @noRd
decompose_build_component <- function(component, terminal_id,
                                      stem_ids,
                                      nd_bridge_ids = NULL,
                                      domain_breaks = NULL) {

  # --- Zero-stem shortcut: entire component is one domain. ---------

  if (length(stem_ids) == 0L) {

    domain_id    <- paste0("domain_", terminal_id)
    outlet_nx <- paste0("nx_outlet_", terminal_id)

    outlet_value <- get_outlet_value(component)
    component$toid[component$id == terminal_id] <- outlet_value
    component <- classify_hy(component)

    domain <- hy_domain(
      domain_id            = domain_id,
      outlet_nexus_id      = outlet_nx,
      inlet_nexus_ids      = character(0),
      containing_domain_id = NA_character_,
      catchments           = component)

    # NOTE (perf): each branch in this function builds a single-row
    # data.frame per segment and `do.call(rbind, ...)` concatenates
    # them at the end of the basin loop. data.frame() construction
    # dominates the cost at finer-grained decomposition (1000+
    # domains). Build parallel character / numeric vectors for the
    # nexus columns and assemble the data.frame once at the end of
    # the basin loop. Fine at the 100-domain target.
    nexus_row <- data.frame(
      nexus_id         = outlet_nx,
      from_domain_id   = domain_id,
      to_domain_id     = NA_character_,
      stringsAsFactors = FALSE)

    return(list(
      domains      = setNames(list(domain), domain_id),
      connectivity = NULL,
      nexuses      = nexus_row,
      index_names  = as.character(component$id),
      index_values = rep(domain_id, nrow(component))
    ))
  }

  # --- A. Stem / residual split. -----------------------------------

  stem_mask <- as.character(component$id) %in% stem_ids
  residual   <- component[!stem_mask, , drop = FALSE]

  # Original toid of every catchment in the component, before any
  # rewriting to the reserved outlet value. Used to determine
  # inter-domain handoff targets and to drive recomposition's toid
  # restoration.
  comp_toid_lookup <- setNames(
    as.character(component$toid), as.character(component$id))

  # --- B. Compute extensive network segments. ------------------------
  # A segment is a maximal linear chain of extensive network catchments
  # between two confluences (or between a headwater/confluence and the
  # outlet). In the decomposed form, every segment becomes a domain --
  # including segments with no lateral tributaries.

  stem_ids_chr   <- as.character(component$id[stem_mask])
  stem_toids_chr <- as.character(component$toid[stem_mask])

  if (!is.null(domain_breaks)) {
    seg_map <- stem_segment_ids(stem_ids_chr, stem_toids_chr,
      extra_terminals = domain_breaks)
  } else {
    seg_map <- stem_segment_ids(stem_ids_chr, stem_toids_chr,
      nd_bridge_ids)
  }

  segment_ids <- unique(unname(seg_map))

  # --- C. Build the basin's extensive network overlay. --------------
  # An hy_leveled view of the connecting flowlines with toids intact
  # except for the basin outlet, which carries the reserved outlet
  # toid value.

  stem_slice <- component[stem_mask, , drop = FALSE]

  stem_outlet_value <- get_outlet_value(stem_slice)
  stem_slice$toid[stem_slice$id == terminal_id] <- stem_outlet_value
  stem_slice <- classify_hy(stem_slice)

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
      as.character(residual$toid) %in% stem_ids_chr])
    seed_ids     <- residual$id[
      as.character(residual$toid) %in% stem_ids_chr]
    seed_segs    <- seg_map[seed_targets]
    split(as.character(seed_ids), seed_segs)
  } else {
    list()
  }

  nexuses_list <- list()
  inlet_by_domain <- list()
  seg_data     <- list()

  for (seg_id in segment_ids) {

    seg_id_chr <- as.character(seg_id)

    # Extensive network catchments belonging to this segment.
    seg_stem_ids <- names(seg_map)[seg_map == seg_id]

    # Lateral seeds draining into this segment (may be empty).
    seeds_in_seg <- seeds_per_segment[[seg_id_chr]]

    if (is.null(seeds_in_seg)) seeds_in_seg <- character(0)

    # Collect residual lateral catchments upstream of the seeds.
    # Build the per-seed walks into a list and dedupe once at the end --
    # repeated union() in the loop is the classic O(N^2) accumulator.
    if (length(seeds_in_seg) > 0L) {

      ups_list <- lapply(seeds_in_seg, function(seed) {
        as.character(decompose_collect_upstream(residual, seed,
          residual_from_idx))
      })

      lateral_ids <- unique(unlist(ups_list, use.names = FALSE))

    } else {
      lateral_ids <- character(0)
    }

    # Domain slice = laterals + extensive network catchments in segment.
    domain_catch_ids <- c(lateral_ids, seg_stem_ids)

    domain_slice <- component[
      as.character(component$id) %in% domain_catch_ids, , drop = FALSE]

    # Set extensive network rows' toids to the reserved outlet value.
    # Each becomes a local outlet of its own contributing sub-basin.
    # Lateral rows keep their natural toids -- they point to in-domain
    # extensive network rows. Extensive network membership is
    # recoverable by intersecting the domain's ids with the basin's
    # domain_connectivity overlay; no marker column is needed.
    domain_outlet_value <- get_outlet_value(domain_slice)
    domain_slice$toid[
      as.character(domain_slice$id) %in% seg_stem_ids] <- domain_outlet_value

    domain_slice <- classify_hy(domain_slice)

    domain_id <- paste0("domain_", terminal_id, "_", seg_id)

    # Determine the segment's outflow target. The segment's
    # downstream-most extensive network catchment is the segment id
    # itself; its original toid is where the domain hands off.
    seg_terminal_toid <- comp_toid_lookup[[seg_id_chr]]

    is_basin_outlet <- !(seg_terminal_toid %in% stem_ids_chr)

    if (is_basin_outlet) {

      primary_nexus_id <- paste0("nx_", seg_id_chr, "_outlet")

      nexuses_list[[length(nexuses_list) + 1L]] <- data.frame(
        nexus_id         = primary_nexus_id,
        from_domain_id   = domain_id,
        to_domain_id     = NA_character_,
        stringsAsFactors = FALSE)

    } else {

      downstream_seg_id <- unname(seg_map[seg_terminal_toid])
      downstream_domain_id <- paste0("domain_", terminal_id,
        "_", downstream_seg_id)

      primary_nexus_id <- paste0("nx_", seg_id_chr,
        "_", seg_terminal_toid)

      nexuses_list[[length(nexuses_list) + 1L]] <- data.frame(
        nexus_id         = primary_nexus_id,
        from_domain_id   = domain_id,
        to_domain_id     = downstream_domain_id,
        stringsAsFactors = FALSE)

      inlet_by_domain[[downstream_domain_id]] <- c(
        inlet_by_domain[[downstream_domain_id]] %||% character(0),
        primary_nexus_id)
    }

    seg_data[[domain_id]] <- list(
      domain_id       = domain_id,
      outlet_nexus_id = primary_nexus_id,
      catchments      = domain_slice,
      catch_ids       = domain_catch_ids
    )
  }

  # --- E. Second pass: build hy_domain instances now that
  # inlet_by_domain is fully populated. --------------------------------

  domains      <- list()
  index_names  <- character(0)
  index_values <- character(0)

  for (domain_id in names(seg_data)) {

    info <- seg_data[[domain_id]]

    domains[[domain_id]] <- hy_domain(
      domain_id            = domain_id,
      outlet_nexus_id      = info$outlet_nexus_id,
      inlet_nexus_ids      = inlet_by_domain[[domain_id]] %||% character(0),
      containing_domain_id = NA_character_,
      catchments           = info$catchments)

    index_names  <- c(index_names, info$catch_ids)
    index_values <- c(index_values,
      rep(domain_id, length(info$catch_ids)))
  }

  # --- F. Return. --------------------------------------------------

  list(
    domains      = domains,
    connectivity = stem_slice,
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
#' Uses a pre-built inverted index (toid -> id) for fast frontier
#' expansion, a position-based visited mask for O(1) membership, and a
#' pre-allocated collected buffer (sized to nrow(residual)) so the
#' walk stays linear in the visited set.
#'
#' @param residual data.frame with id, toid columns (the non-stem
#'   rows of a drainage basin).
#' @param seed scalar catchment id to start from.
#' @param from_idx pre-built inverted index (output of
#'   `split(residual$id, residual$toid)`). Built once per component
#'   and reused across seeds.
#' @returns vector of ids in the same type as residual$id, including seed.
#' @noRd
decompose_collect_upstream <- function(residual, seed, from_idx) {

  if (nrow(residual) == 0L) return(seed)

  res_ids   <- as.character(residual$id)
  pos_by_id <- setNames(seq_along(res_ids), res_ids)

  visited       <- logical(length(res_ids))
  collected_pos <- integer(length(res_ids))
  n_collected   <- 0L

  seed_pos <- pos_by_id[as.character(seed)]
  in_res   <- !is.na(seed_pos)

  if (any(in_res)) {

    sp <- seed_pos[in_res]

    visited[sp] <- TRUE
    collected_pos[seq_along(sp)] <- sp
    n_collected <- length(sp)

    frontier <- res_ids[sp]

    while (length(frontier) > 0L) {

      next_hop <- unlist(from_idx[frontier], use.names = FALSE)

      if (length(next_hop) == 0L) break

      nh_pos <- pos_by_id[as.character(next_hop)]
      nh_pos <- nh_pos[!is.na(nh_pos) & !visited[nh_pos]]

      if (length(nh_pos) == 0L) break

      visited[nh_pos] <- TRUE
      new_n <- n_collected + length(nh_pos)
      collected_pos[(n_collected + 1L):new_n] <- nh_pos
      n_collected <- new_n

      frontier <- res_ids[nh_pos]
    }
  }

  collected_in_res <- residual$id[collected_pos[seq_len(n_collected)]]

  # The original returns `seed` even when seed is not a residual row;
  # preserve that behavior so callers see no change in output shape.
  if (all(in_res)) collected_in_res
  else c(collected_in_res, seed[!in_res])
}

#' Empty inter-domain edge data.frame
#'
#' Shared shape used as the zero-row return from `get_domain_graph()`
#' when neither flow nor containment relationships contribute any
#' rows.
#' @noRd
empty_graph_df <- function() {

  data.frame(
    id = character(0),
    toid = character(0),
    nexus_id = character(0),
    relation_type = character(0),
    stringsAsFactors = FALSE)
}

#' Get the inter-domain edge list from a decomposition
#'
#' @description
#' Returns the inter-domain edge list as a hydroloom edge list (`hy_topo`
#' or `hy_flownetwork`). Two kinds of relationships appear in the result,
#' selected by `relations`:
#'
#' - `"flow"` -- pulled from `nexus_registry`. Each row whose
#'   `to_domain_id` is non-NA contributes one row to the result,
#'   recording where one domain hands off to the next at a hydro
#'   nexus. The result row carries that nexus's id in `nexus_id` and
#'   `relation_type = "flow"`.
#' - `"contained"` -- pulled from each domain's `containing_domain_id`
#'   slot. Each non-NA value contributes one row, from the contained
#'   domain to its container. Containment is declared post-decomposition
#'   via [set_containment()] and does not pass through a hydro nexus,
#'   so the result row carries `nexus_id = NA_character_` and
#'   `relation_type = "contained"`.
#'
#' The default returns both kinds; pass `relations = "flow"` for the
#' inter-domain flow graph only, or `relations = "contained"` for
#' containment relationships only.
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
#' @returns hydroloom edge list (`hy_topo` or `hy_flownetwork`) with
#'   columns `id`, `toid`, `nexus_id`, `relation_type`.
#' @seealso [domain_decomposition] for the wrapper object,
#'   [decompose_network()], [set_containment()].
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

  # ---- Flow edges from nexus_registry ---------------------------------

  reg <- decomposition$nexus_registry

  flow <- if (!is.null(reg) && nrow(reg) > 0L) {

    is_inter <- !is.na(reg$to_domain_id)

    data.frame(
      id            = reg$from_domain_id[is_inter],
      toid          = reg$to_domain_id[is_inter],
      nexus_id      = reg$nexus_id[is_inter],
      relation_type = rep("flow", sum(is_inter)),
      stringsAsFactors = FALSE)

  } else {
    empty_graph_df()
  }

  # ---- Containment edges from domain slots ----------------------------
  # One edge per non-NA containing_domain_id. No flow crosses a hydro
  # nexus between contained and containing, so nexus_id is NA.

  domains <- decomposition$domains %||% list()

  cont_pairs <- list()

  for (d in domains) {

    cd <- d$containing_domain_id

    if (length(cd) == 1 && !is.na(cd) && nzchar(cd))
      cont_pairs[[length(cont_pairs) + 1L]] <-
        c(d$domain_id, cd)

  }

  contained <- if (length(cont_pairs) > 0L) {

    pairs <- do.call(rbind, cont_pairs)

    data.frame(
      id            = pairs[, 1],
      toid          = pairs[, 2],
      nexus_id      = NA_character_,
      relation_type = rep("contained", nrow(pairs)),
      stringsAsFactors = FALSE)

  } else {
    empty_graph_df()
  }

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

#' Get a domain by id
#'
#' @description
#' Returns the [hy_domain()] object stored under `domain_id` in a
#' decomposition. Errors when `domain_id` is not a key of
#' `decomposition$domains`.
#'
#' @param decomposition object of class `domain_decomposition`.
#' @param domain_id character(1). Domain id to look up.
#' @returns object of class `hy_domain` — the named list with all six
#'   slots described in [hy_domain()].
#' @seealso [hy_domain()] for the per-domain object,
#'   [get_domain_connectivity()], [get_domain_for_catchment()],
#'   [get_domain_graph()].
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
#' get_domain(d, names(d$domains)[[1]])
#'
get_domain <- function(decomposition, domain_id) {

  domain_id <- as.character(domain_id)

  if (length(domain_id) != 1L)
    stop("get_domain: domain_id must be a single id, got length ",
      length(domain_id), call. = FALSE)

  domains <- decomposition$domains %||% list()

  if (!domain_id %in% names(domains))
    stop("get_domain: unknown domain id '", domain_id, "'",
      call. = FALSE)

  domains[[domain_id]]
}

#' Get a basin's extensive network overlay
#'
#' @description
#' Returns the `hy_leveled` overlay stored under `basin_id` in
#' `decomposition$domain_connectivity`, or — when `basin_id` is `NULL`
#' (the default) — the full named list of overlays. The overlay is the
#' basin's *extensive network*: a `hy_leveled` view of the connecting
#' flowlines with `toid`s intact except at the basin outlet, which
#' carries the reserved outlet `toid` value.
#'
#' @param decomposition object of class `domain_decomposition`.
#' @param basin_id character(1) or `NULL`. Basin id to look up.
#'   `NULL` (default) returns the full named list.
#' @returns single `hy_leveled` overlay when `basin_id` is supplied;
#'   named list of overlays when `basin_id` is `NULL`.
#' @seealso [hy_domain()] for the dual-ownership rule that motivates
#'   the overlay, [domain_decomposition], [get_domain()],
#'   [get_domain_graph()].
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
#' get_domain_connectivity(d, names(d$domain_connectivity)[[1]])
#'
get_domain_connectivity <- function(decomposition, basin_id = NULL) {

  conn <- decomposition$domain_connectivity %||% list()

  if (is.null(basin_id)) return(conn)

  basin_id <- as.character(basin_id)

  if (length(basin_id) != 1L)
    stop("get_domain_connectivity: basin_id must be a single id, got length ",
      length(basin_id), call. = FALSE)

  if (!basin_id %in% names(conn))
    stop("get_domain_connectivity: unknown basin id '", basin_id, "'",
      call. = FALSE)

  conn[[basin_id]]
}

#' Get the nexus registry from a decomposition
#'
#' @description
#' Returns `decomposition$nexus_registry` -- the table that records each
#' synthetic hydro nexus produced by [decompose_network()] and which
#' domains it connects. One row per nexus; the columns are `nexus_id`,
#' `from_domain_id`, and `to_domain_id` (NA at basin outlets where no
#' downstream domain receives the handoff).
#'
#' @param decomposition object of class `domain_decomposition`.
#' @returns data.frame.
#' @seealso [domain_decomposition], [get_domain_graph()].
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
#' head(get_nexus_registry(d))
#'
get_nexus_registry <- function(decomposition) {

  if (!inherits(decomposition, "domain_decomposition"))
    stop("get_nexus_registry: decomposition must be a ",
      "domain_decomposition.", call. = FALSE)

  decomposition$nexus_registry

}

#' Get the overrides table from a decomposition
#'
#' @description
#' Returns `decomposition$overrides` -- the non-dendritic inter-domain
#' transfer table passed through from [decompose_network()]. `NULL`
#' when no overrides were supplied.
#'
#' @param decomposition object of class `domain_decomposition`.
#' @returns data.frame or `NULL`.
#' @seealso [domain_decomposition], [decompose_network()].
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
#' get_overrides(d)
#'
get_overrides <- function(decomposition) {

  if (!inherits(decomposition, "domain_decomposition"))
    stop("get_overrides: decomposition must be a ",
      "domain_decomposition.", call. = FALSE)

  decomposition$overrides

}

#' Test whether a domain is a leaf
#'
#' @description
#' A domain is a *leaf* when no upstream domain feeds into it — its
#' `inlet_nexus_ids` slot is empty.
#'
#' @param decomposition object of class `domain_decomposition`.
#' @param domain_id character(1). Domain id to test.
#' @returns logical(1).
#' @seealso [is_stem_domain()], [is_root_domain()], [hy_domain()].
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
#' is_leaf_domain(d, names(d$domains)[[1]])
#'
is_leaf_domain <- function(decomposition, domain_id) {

  d <- get_domain(decomposition, domain_id)

  length(d$inlet_nexus_ids) == 0L
}

#' Test whether a domain is the root of its basin
#'
#' @description
#' A domain is the *root* when its outlet nexus has no downstream
#' domain — the registry row for its `outlet_nexus_id` carries
#' `to_domain_id = NA`. The root is the basin's most-downstream domain.
#'
#' @inheritParams is_leaf_domain
#' @returns logical(1).
#' @seealso [is_leaf_domain()], [is_stem_domain()], [hy_domain()].
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
#' is_root_domain(d, names(d$domains)[[1]])
#'
is_root_domain <- function(decomposition, domain_id) {

  # NOTE (perf): linear scan of nexus_registry per call. Fine at
  # the 100-domain target (10^4 work even when called inside a
  # vapply() over every domain), but the same scan happens here
  # and in `is_leaf_domain` / `is_stem_domain`. Drop-in fix is to
  # precompute one named lookup (e.g. outgoing_by_nexus) and have
  # all three predicates read from it.
  d <- get_domain(decomposition, domain_id)

  out_nx <- d$outlet_nexus_id

  if (is.null(out_nx) || length(out_nx) == 0L) return(FALSE)

  reg <- decomposition$nexus_registry

  if (is.null(reg) || nrow(reg) == 0L) return(TRUE)

  outgoing <- reg[reg$nexus_id == out_nx & !is.na(reg$to_domain_id),
    , drop = FALSE]

  nrow(outgoing) == 0L
}

#' Test whether a domain is a stem
#'
#' @description
#' A domain is a *stem* when it is neither a leaf nor a root — both
#' upstream domains feed into it and its outlet hands off to a
#' downstream domain.
#'
#' @inheritParams is_leaf_domain
#' @returns logical(1).
#' @seealso [is_leaf_domain()], [is_root_domain()], [hy_domain()].
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
#' is_stem_domain(d, names(d$domains)[[1]])
#'
is_stem_domain <- function(decomposition, domain_id) {

  !is_leaf_domain(decomposition, domain_id) &&
    !is_root_domain(decomposition, domain_id)
}
