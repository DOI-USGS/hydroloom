##### decomposition.R -- hy_domain constructor + structural validator (Layer 1) #####
#
# Layer 1 surface only: hy_domain() and validate_decomposition().
#
# decompose_network(), recompose(), get_domain_graph(), accumulate_domains(),
# inject_lateral(), and friends will land in their own files as the broader
# decomposition implementation rolls out. The Layer 1 contract is pinned by
# tests/testthat/test_decomposition_class.R; do not change slot names or
# error keywords without updating that file.

#' Construct a hy_domain
#'
#' @description
#' A `hy_domain` is the unit of independent computation in a network
#' decomposition. Every `hy_domain` is compact — a partitioned piece
#' of a drainage basin that bundles the segment's lateral tributaries
#' with the main-path rows flowing through the segment (the latter
#' in their decomposed form, with reserved-`toid` values that mark each
#' as a local outlet). The basin's *extensive connectivity* — a
#' `hy_leveled` view of the main path with `toid`s intact — is stored
#' separately in `domain_decomposition$domain_connectivity`.
#'
#' @details
#' The `catchments` slot may be `hy_topo` (or `hy_leveled`) for
#' dendritic internal connectivity, or `hy_flownetwork` to preserve
#' internal divergences.
#'
#' **Compact and extensive duality.** The main-path rows in a
#' domain's `catchments` carry the reserved outlet `toid` value (the
#' value `get_outlet_value()` returns), so each becomes a local outlet
#' of its own contributing sub-basin. Those same ids appear, with
#' `toid`s intact, in the basin's `domain_connectivity[[basin_id]]`
#' overlay so the basin's main path stays addressable end-to-end. With
#' the two ownerships kept distinct, per-domain processing runs in
#' parallel and recomposition lands in a single pass. Main-path
#' membership inside a domain is recoverable by intersecting the
#' domain's `catchments$id` with the parent basin's
#' connectivity-overlay `id`.
#'
#' **Decomposed and recomposed modes.** The decomposed form is the
#' default mode: each main-path row is an outlet of its own
#' contributing sub-basin, so a single
#' [accumulate_downstream()][accumulate_downstream] call on the
#' domain's catchments produces, for every main-path catchment in the
#' segment, the locally-incremental drainage area (or any other
#' accumulable) that belongs there. To switch to recomposed mode, join
#' `source_network[, c("id", "toid")]` onto the domain's catchments
#' where `toid` carries the reserved outlet value, replacing it with
#' the original `toid`; the segment is a connected sub-basin again. See
#' `vignette("domain_decomposition")` for the full framing.
#'
#' The constructor returns a plain S3 list. Slot mutation after
#' construction is permitted; downstream invariants are re-checked by
#' [validate_decomposition()].
#'
#' @param domain_id character(1). Unique identifier for this domain.
#' @param outlet_nexus_id character(1). Identifier of the outlet hydro
#'   nexus where this domain discharges.
#' @param inlet_nexus_ids character. Hydro nexus ids where upstream
#'   domains feed into this one. `character(0)` for leaf domains;
#'   populated for stem and root domains.
#' @param containing_domain_id character(1). For contained (e.g.
#'   endorheic) domains, the id of the enclosing domain.
#'   `NA_character_` if not contained.
#' @param catchments hydroloom object carrying the domain's catchment
#'   network. Must be `hy_topo`, `hy_leveled`, or `hy_flownetwork`.
#' @param topo_sort_offset integer(1). Global topo_sort base enabling
#'   cross-domain ordering after recomposition.
#' @returns object of class `hy_domain` — a list with the six named
#'   slots above.
#' @export
#' @examples
#' lev <- hy(data.frame(
#'   id = 1:3, toid = c(2L, 3L, 0L),
#'   topo_sort = 3:1, levelpath = c(1L, 1L, 1L),
#'   levelpath_outlet_id = c(3L, 3L, 3L)))
#'
#' hy_domain(
#'   domain_id = "T1",
#'   outlet_nexus_id = "n_out",
#'   inlet_nexus_ids = character(0),
#'   containing_domain_id = NA_character_,
#'   catchments = lev,
#'   topo_sort_offset = 0L)
#'
hy_domain <- function(domain_id,
                      outlet_nexus_id,
                      inlet_nexus_ids,
                      containing_domain_id,
                      catchments,
                      topo_sort_offset) {

  if (!inherits(catchments, "hy_topo") &&
      !inherits(catchments, "hy_flownetwork"))
    stop("hy_domain: catchments must be hy_topo, hy_leveled, or ",
      "hy_flownetwork. Current class: ",
      paste(class(catchments), collapse = "/"),
      call. = FALSE)

  structure(
    list(
      domain_id            = domain_id,
      outlet_nexus_id      = outlet_nexus_id,
      inlet_nexus_ids      = inlet_nexus_ids,
      containing_domain_id = containing_domain_id,
      catchments           = catchments,
      topo_sort_offset     = topo_sort_offset
    ),
    class = "hy_domain"
  )
}

#' Validate a domain decomposition
#'
#' @description
#' Runs structural checks against a `domain_decomposition` object and
#' returns a list of `valid` (logical) and `issues` (character vector
#' of human-readable problem descriptions).
#'
#' @details
#' Structural checks, run in order:
#'
#' \enumerate{
#'   \item **Outlet count** — each basin's `domain_connectivity`
#'     overlay resolves to exactly one outlet sub-network via
#'     [sort_network()] with `split = TRUE`. Domains may have multiple
#'     outlets by design and are not checked.
#'   \item **Coverage / partition** — every `source_network` id
#'     appears in exactly one domain's `catchments` slot.
#'   \item **Connectivity membership** — each domain's rows that carry
#'     the reserved outlet `toid` value (other than genuine basin
#'     outlets, whose `source_network` `toid` carries the same reserved
#'     value) appear, with `toid`s intact, in some basin's
#'     `domain_connectivity` overlay. See [hy_domain()] for the
#'     dual-ownership rule behind this duplication.
#'   \item **Inter-domain cycle** — the derived domain graph
#'     ([get_domain_graph()] with `relations = "flow"`) is acyclic;
#'     checked by delegating to [check_hy_graph()].
#'   \item **Nexus existence** — every `nexus_id` referenced by a
#'     derived domain-graph edge is registered in `nexus_registry`.
#'   \item **Containment resolution** — every non-NA
#'     `containing_domain_id` resolves to a key of `decomposition$domains`.
#'   \item **Override references** — every row in `overrides` (when
#'     present) names a known source/sink domain via `id`/`toid` and a
#'     known source/sink nexus via `source_nexus_id`/`sink_nexus_id`.
#' }
#'
#' @param decomposition object of class `domain_decomposition`.
#' @returns list with elements `valid` (logical scalar) and `issues`
#'   (character vector — empty when `valid` is TRUE).
#' @seealso [domain_decomposition] for the object's slots,
#'   [hy_domain()] for the per-domain object, [decompose_network()].
#' @export
#' @examples
#' lev <- hy(data.frame(
#'   id = 1:3, toid = c(2L, 3L, 0L),
#'   topo_sort = 3:1, levelpath = c(1L, 1L, 1L),
#'   levelpath_outlet_id = c(3L, 3L, 3L)))
#'
#' domain <- hy_domain(
#'   domain_id = "T1",
#'   outlet_nexus_id = "n_out",
#'   inlet_nexus_ids = character(0),
#'   containing_domain_id = NA_character_,
#'   catchments = lev,
#'   topo_sort_offset = 0L)
#'
#' d <- structure(
#'   list(
#'     domains = list(T1 = domain),
#'     domain_connectivity = list(),
#'     overrides = NULL,
#'     catchment_domain_index = setNames(rep("T1", 3), c("1", "2", "3")),
#'     nexus_registry = data.frame(nexus_id = "n_out"),
#'     source_network = lev),
#'   class = "domain_decomposition")
#'
#' validate_decomposition(d)
#'
validate_decomposition <- function(decomposition) {

  issues <- character(0)

  domains <- decomposition$domains %||% list()
  conn    <- decomposition$domain_connectivity %||% list()

  # ---- Check 1: outlet count per basin connectivity overlay -----------
  # Each basin's connectivity overlay must resolve to exactly one outlet
  # via sort_network(split = TRUE). Domains may carry multiple outlets
  # by design (lateral subgroups whose toids have been set to the
  # reserved outlet value), so they are not checked here.

  for (basin_id in names(conn)) {

    catch <- conn[[basin_id]]

    if (is.null(catch) || nrow(catch) == 0) next

    n_out <- tryCatch(
      suppressWarnings({
        sorted <- sort_network(catch, split = TRUE)
        length(unique(sorted$terminal_id))
      }),
      error = function(e) NA_integer_)

    if (is.na(n_out)) {

      issues <- c(issues, sprintf(
        "basin '%s': could not determine outlet count (sort_network failed)",
        basin_id))

    } else if (n_out != 1L) {

      issues <- c(issues, sprintf(
        "basin '%s': expected exactly one outlet, found %d",
        basin_id, n_out))

    }

  }

  # ---- Check 2: coverage / partition -----------------------------------
  # Every source id appears in exactly one domain's catchments.

  src <- decomposition$source_network

  if (!is.null(src) && "id" %in% names(src)) {

    domain_catch_ids <- unlist(
      lapply(domains, function(d) d$catchments$id),
      use.names = FALSE)

    src_ids <- src$id

    missing_ids <- setdiff(src_ids, domain_catch_ids)

    if (length(missing_ids) > 0) {

      issues <- c(issues, sprintf(
        "coverage: %d source catchments not assigned to any domain",
        length(missing_ids)))

    }

    dup_ids <- domain_catch_ids[duplicated(domain_catch_ids)]

    if (length(dup_ids) > 0) {

      issues <- c(issues, sprintf(
        "coverage: %d catchment ids appear in more than one domain",
        length(unique(dup_ids))))

    }

  }

  # ---- Check 3: connectivity membership -------------------------------
  # Each domain's rows that carry the reserved outlet toid value
  # (other than genuine basin outlets, whose source_network toid
  # carries the same reserved value) must appear, with toids intact,
  # in some basin's domain_connectivity overlay.

  if (!is.null(src) && all(c("id", "toid") %in% names(src)) &&
      length(domains) > 0L) {

    conn_ids_pool <- unlist(
      lapply(conn, function(o) as.character(o$id)),
      use.names = FALSE)

    src_toid_by_id <- setNames(as.character(src$toid), as.character(src$id))
    src_outlet_value <- as.character(get_outlet_value(src))

    missing_total <- 0L

    for (d in domains) {

      catch <- d$catchments

      if (is.null(catch) || nrow(catch) == 0L) next

      catch_outlet_value <- as.character(get_outlet_value(catch))
      is_seg <- as.character(catch$toid) == catch_outlet_value

      if (!any(is_seg)) next

      seg_ids   <- as.character(catch$id[is_seg])
      src_toids <- src_toid_by_id[seg_ids]

      is_genuine_outlet <- !is.na(src_toids) & src_toids == src_outlet_value

      detoid_ids <- seg_ids[!is_genuine_outlet]

      if (length(detoid_ids) == 0L) next

      missing <- setdiff(detoid_ids, conn_ids_pool)
      missing_total <- missing_total + length(missing)
    }

    if (missing_total > 0L) {

      issues <- c(issues, sprintf(
        paste0("connectivity: %d reserved-toid rows not present ",
          "in any domain_connectivity overlay"),
        missing_total))

    }

  }

  # ---- Check 4: inter-domain cycle (derived graph) --------------------

  g_flow <- tryCatch(
    get_domain_graph(decomposition, relations = "flow"),
    error = function(e) NULL)

  if (!is.null(g_flow) && nrow(g_flow) > 0 &&
      all(c("id", "toid") %in% names(g_flow))) {

    chk <- tryCatch(
      check_hy_graph(g_flow[, c("id", "toid")]),
      error = function(e) e)

    if (!isTRUE(chk)) {

      issues <- c(issues,
        "domain_graph cycle: flow edges contain a cycle (failed check_hy_graph)")

    }

  }

  # ---- Check 5: nexus existence in derived domain graph ---------------

  g_all <- tryCatch(
    get_domain_graph(decomposition),
    error = function(e) NULL)

  if (!is.null(g_all) && nrow(g_all) > 0 && "nexus_id" %in% names(g_all)) {

    reg_ids <- decomposition$nexus_registry$nexus_id %||% character(0)

    unknown <- setdiff(g_all$nexus_id, reg_ids)

    if (length(unknown) > 0) {

      issues <- c(issues, sprintf(
        "domain_graph nexus unknown: %s not present in nexus_registry",
        paste(shQuote(unknown), collapse = ", ")))

    }

  }

  # ---- Check 6: containment id resolves --------------------------------

  for (d in domains) {

    cd <- d$containing_domain_id

    if (length(cd) == 1 && !is.na(cd) && nzchar(cd)) {

      if (!cd %in% names(domains)) {

        issues <- c(issues, sprintf(
          "domain '%s': containing_domain_id '%s' not in decomposition$domains",
          d$domain_id, cd))

      }

    }

  }

  # ---- Check 7: overrides reference known domains and nexuses ----------

  ov <- decomposition$overrides

  if (!is.null(ov) && is.data.frame(ov) && nrow(ov) > 0) {

    domain_keys <- names(domains)
    reg_ids     <- decomposition$nexus_registry$nexus_id %||% character(0)

    if ("id" %in% names(ov)) {

      bad_src <- setdiff(ov$id, domain_keys)

      if (length(bad_src) > 0) {

        issues <- c(issues, sprintf(
          "override unknown source domain: %s not present in decomposition$domains",
          paste(shQuote(bad_src), collapse = ", ")))

      }

    }

    if ("toid" %in% names(ov)) {

      bad_sink <- setdiff(ov$toid, domain_keys)

      if (length(bad_sink) > 0) {

        issues <- c(issues, sprintf(
          "override unknown sink domain: %s not present in decomposition$domains",
          paste(shQuote(bad_sink), collapse = ", ")))

      }

    }

    if ("source_nexus_id" %in% names(ov)) {

      bad_src_nx <- setdiff(ov$source_nexus_id, reg_ids)

      if (length(bad_src_nx) > 0) {

        issues <- c(issues, sprintf(
          "override unknown source nexus: %s not present in nexus_registry",
          paste(shQuote(bad_src_nx), collapse = ", ")))

      }

    }

    if ("sink_nexus_id" %in% names(ov)) {

      bad_sink_nx <- setdiff(ov$sink_nexus_id, reg_ids)

      if (length(bad_sink_nx) > 0) {

        issues <- c(issues, sprintf(
          "override unknown sink nexus: %s not present in nexus_registry",
          paste(shQuote(bad_sink_nx), collapse = ", ")))

      }

    }

  }

  list(valid = length(issues) == 0L, issues = issues)
}
