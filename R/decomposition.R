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
#' decomposition. It bundles a domain's catchment network with the
#' connectivity metadata that recomposition needs.
#'
#' @details
#' Two domain types are supported:
#'
#' * **Trunk** domains are realized by a major mainstem flowpath. They
#'   require a `hy_leveled` `catchments` slot with toids intact, so the
#'   connected mainstem can be drawn, named, and walked end-to-end. A
#'   trunk catchment is also present, in its decomposed form, in the
#'   surrounding compact (the row with its `toid` set to the outlet
#'   sentinel); the trunk's catchments slot is a duplicate view used
#'   for visualization and explicit naming.
#' * **Compact** domains carry the segment's lateral tributaries plus
#'   the trunk-mainstem rows that flow through the segment. The
#'   trunk-mainstem rows have a sentinel `toid`, so each is a local
#'   outlet of its own contributing sub-basin. Membership in the
#'   trunk-mainstem set is recoverable from the parent trunk domain's
#'   `catchments$id`. The slot may be `hy_topo` (or `hy_leveled`) for
#'   dendritic internal connectivity, or `hy_flownetwork` to preserve
#'   internal divergences.
#'
#' **Trunk and compact ownership.** The trunk is not itself a catchment.
#' It carries accumulated values from one compact domain to the next. The
#' catchments along the trunk's mainstem are, by catchment aggregate
#' semantics (HY_Features), part of the surrounding compact domain's drainage
#' area. The implementation puts them in the compact domain's `catchments`
#' slot (with sentinel toids) and also duplicates them in the trunk's
#' `catchments` slot (with toids intact) so the connected mainstem
#' stays addressable for drawing and naming. With the two ownerships
#' kept distinct, per-compact processing runs in parallel and
#' recomposition lands in a single pass.
#'
#' **Decomposed and recomposed modes.** The decomposed compact form
#' is the default mode: each trunk-mainstem row is an outlet of its own
#' contributing sub-basin, so a single
#' [accumulate_downstream()][accumulate_downstream] call on the
#' compact domain's catchments produces, for every trunk catchment in the
#' segment, the locally-incremental drainage area (or any other
#' accumulable) that belongs there. To switch to recomposed mode, join
#' `source_network[, c("id", "toid")]` onto the compact domain's catchments
#' where `toid` is the outlet sentinel, replacing it with the original
#' value; the segment is a connected sub-basin again. See
#' `vignette("domain_decomposition")` for the full framing.
#'
#' Recomposition restores the trunk-mainstem rows' toids in each
#' compact from `source_network`, reconnecting the mainstem.
#'
#' The constructor returns a plain S3 list. Slot mutation after
#' construction is permitted; downstream invariants are re-checked by
#' [validate_decomposition()].
#'
#' @param domain_id character(1). Unique identifier for this domain.
#' @param domain_type character(1). Either `"trunk"` or `"compact"`.
#' @param outlet_nexus_id character(1). Identifier of the outlet hydro
#'   nexus where this domain discharges.
#' @param inlet_nexus_ids character. For trunk domains, the nexus ids where
#'   compact domains inject lateral inflow along the mainstem. Always
#'   `character(0)` for compact domains — they have no upstream domain
#'   connections; they connect only to their parent trunk.
#' @param trunk_domain_id character(1). For compact domains, the id of
#'   the receiving trunk; for trunks, self or `NA_character_`.
#' @param containing_domain_id character(1). For contained (e.g. endorheic)
#'   domains, the id of the enclosing domain. `NA_character_` if not
#'   contained.
#' @param catchments hydroloom object carrying the domain's catchment
#'   network. Must be `hy_leveled` for trunks; `hy_topo`, `hy_leveled`,
#'   or `hy_flownetwork` for compact domains.
#' @param topo_sort_offset integer(1). Global topo_sort base enabling
#'   cross-domain ordering after recomposition.
#' @returns object of class `hy_domain` — a list with the eight named
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
#'   domain_type = "trunk",
#'   outlet_nexus_id = "n_out",
#'   inlet_nexus_ids = character(0),
#'   trunk_domain_id = NA_character_,
#'   containing_domain_id = NA_character_,
#'   catchments = lev,
#'   topo_sort_offset = 0L)
#'
hy_domain <- function(domain_id,
                      domain_type,
                      outlet_nexus_id,
                      inlet_nexus_ids,
                      trunk_domain_id,
                      containing_domain_id,
                      catchments,
                      topo_sort_offset) {

  if (!domain_type %in% c("trunk", "compact"))
    stop("hy_domain: domain_type must be 'trunk' or 'compact', got '",
      domain_type, "'", call. = FALSE)

  if (identical(domain_type, "trunk") && !inherits(catchments, "hy_leveled"))
    stop("hy_domain: trunk domain catchments must be hy_leveled. ",
      "Current class: ", paste(class(catchments), collapse = "/"),
      ". Use add_levelpaths() to enrich the network before wrapping ",
      "it in a trunk hy_domain.",
      call. = FALSE)

  if (identical(domain_type, "compact") &&
      !inherits(catchments, "hy_topo") &&
      !inherits(catchments, "hy_flownetwork"))
    stop("hy_domain: compact domain catchments must be hy_topo, ",
      "hy_leveled, or hy_flownetwork. Current class: ",
      paste(class(catchments), collapse = "/"),
      call. = FALSE)

  structure(
    list(
      domain_id            = domain_id,
      domain_type          = domain_type,
      outlet_nexus_id      = outlet_nexus_id,
      inlet_nexus_ids      = inlet_nexus_ids,
      trunk_domain_id      = trunk_domain_id,
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
#' Layer 1 structural checks, run in order:
#'
#' \enumerate{
#'   \item **Trunk class invariant** — every trunk domain's `catchments`
#'     slot inherits from `hy_leveled`. Re-checked here because slot
#'     mutation after construction is permitted.
#'   \item **Outlet count** — each trunk domain's `catchments`
#'     resolves to exactly one outlet sub-network via [sort_network()]
#'     with `split = TRUE`. Compact domains may have multiple outlets.
#'   \item **Coverage / partition** — every `source_network` id appears
#'     in exactly one compact domain's catchments slot. The
#'     trunk-subset sub-check then confirms every trunk row is also
#'     present in a compact domain with its `toid` set to the outlet sentinel
#'     returned by `get_outlet_value()`. See [hy_domain()] for the
#'     ownership rules behind this duplication.
#'   \item **Inter-domain cycle** — `domain_graph` flow edges form an
#'     acyclic graph; checked by delegating to [check_hy_graph()].
#'   \item **Nexus existence** — every `nexus_id` referenced by a
#'     `domain_graph` edge is registered in `nexus_registry`.
#'   \item **Containment resolution** — every non-NA
#'     `containing_domain_id` resolves to a key of `decomposition$domains`.
#'   \item **Override references** — every row in `overrides` (when
#'     present) names a known source/sink domain via `id`/`toid` and a
#'     known source/sink nexus via `source_nexus_id`/`sink_nexus_id`.
#' }
#'
#' Mass-balance checks (compact-domain outflows match trunk lateral
#' inflows) and at-scale closed-basin counts are deferred until
#' `recompose()` is implemented; the corresponding negative oracles
#' are pinned by Layer 5 and Layer 9 of the decomposition test scaffold.
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
#' trunk <- hy_domain(
#'   domain_id = "T1", domain_type = "trunk",
#'   outlet_nexus_id = "n_out", inlet_nexus_ids = character(0),
#'   trunk_domain_id = NA_character_, containing_domain_id = NA_character_,
#'   catchments = lev, topo_sort_offset = 0L)
#'
#' d <- structure(
#'   list(
#'     domains = list(T1 = trunk),
#'     domain_graph = data.frame(id = character(0), toid = character(0),
#'       relation_type = character(0)),
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

  # ---- Check 1: trunk class invariant ----------------------------------

  for (d in domains) {

    if (identical(d$domain_type, "trunk") &&
        !inherits(d$catchments, "hy_leveled")) {

      issues <- c(issues, sprintf(
        "domain '%s': trunk catchments must be hy_leveled (class is %s)",
        d$domain_id, paste(class(d$catchments), collapse = "/")))

    }

  }

  # ---- Check 2: outlet count per domain --------------------------------
  # Trunk domains must have exactly one outlet. Compact domains may have
  # multiple outlets (disconnected tributary groups draining into
  # different trunk catchments along the same trunk segment).

  for (d in domains) {

    if (d$domain_type != "trunk") next

    catch <- d$catchments

    if (is.null(catch) || nrow(catch) == 0) next

    n_out <- tryCatch(
      suppressWarnings({
        sorted <- sort_network(catch, split = TRUE)
        length(unique(sorted$terminal_id))
      }),
      error = function(e) NA_integer_)

    if (is.na(n_out)) {

      issues <- c(issues, sprintf(
        "domain '%s': could not determine outlet count (sort_network failed)",
        d$domain_id))

    } else if (n_out != 1L) {

      issues <- c(issues, sprintf(
        "domain '%s': expected exactly one outlet, found %d",
        d$domain_id, n_out))

    }

  }

  # ---- Check 3: coverage / partition -----------------------------------
  # Compacts form the partition: every source id appears in exactly one
  # compact domain's catchments. Trunk catchments are checked in the trunk-subset
  # sub-check below (they duplicate compact-resident trunk-feature rows).

  src <- decomposition$source_network

  if (!is.null(src) && "id" %in% names(src)) {

    is_compact <- vapply(domains,
      function(d) identical(d$domain_type, "compact"), logical(1))

    compact_catch_ids <- unlist(
      lapply(domains[is_compact], function(d) d$catchments$id),
      use.names = FALSE)

    src_ids <- src$id

    missing_ids <- setdiff(src_ids, compact_catch_ids)

    if (length(missing_ids) > 0) {

      issues <- c(issues, sprintf(
        "coverage: %d source catchments not assigned to any compact domain",
        length(missing_ids)))

    }

    dup_ids <- compact_catch_ids[duplicated(compact_catch_ids)]

    if (length(dup_ids) > 0) {

      issues <- c(issues, sprintf(
        "coverage: %d catchment ids appear in more than one compact domain",
        length(unique(dup_ids))))

    }

    # Trunk-subset sub-check: every trunk catchment must also appear in
    # some compact domain, with its compact-side `toid` set to the outlet
    # sentinel (i.e. detoid'd into the decomposed compact form).
    trunk_domains <- domains[!is_compact &
      vapply(domains, function(d) identical(d$domain_type, "trunk"),
        logical(1))]

    if (length(trunk_domains) > 0L) {

      # Per-id toid lookup across compact domains (last write wins on
      # duplicates -- the partition coverage check above flags those
      # separately). Sentinel: "" for character ids, 0 for numeric.
      compact_toid_lookup <- list()

      for (d in domains[is_compact]) {

        catch <- d$catchments

        if (is.null(catch) || nrow(catch) == 0L) next

        ids <- as.character(catch$id)

        for (i in seq_along(ids)) {
          compact_toid_lookup[[ids[i]]] <- catch$toid[i]
        }
      }

      id_is_char <- is.character(src$id)

      missing_trunk <- character(0)
      bad_toid      <- character(0)

      for (td in trunk_domains) {

        for (tid in as.character(td$catchments$id)) {

          tval <- compact_toid_lookup[[tid]]

          if (is.null(tval)) {
            missing_trunk <- c(missing_trunk, tid)
            next
          }

          is_sentinel <- !is.na(tval) && (
            if (id_is_char) identical(as.character(tval), "")
            else identical(as.numeric(tval), 0))

          if (!is_sentinel) bad_toid <- c(bad_toid, tid)
        }
      }

      if (length(missing_trunk) > 0L) {

        issues <- c(issues, sprintf(
          paste0("trunk-subset: %d trunk catchment ids do not appear in ",
            "any compact domain's catchments"),
          length(missing_trunk)))
      }

      if (length(bad_toid) > 0L) {

        issues <- c(issues, sprintf(
          paste0("trunk-subset: %d trunk catchment rows in compact domains have ",
            "non-sentinel toid"),
          length(bad_toid)))
      }

    }

  }

  # ---- Check 4: inter-domain cycle -------------------------------------

  g <- decomposition$domain_graph

  if (!is.null(g) && nrow(g) > 0 && "relation_type" %in% names(g)) {

    flow <- g[g$relation_type == "flow", , drop = FALSE]

    if (nrow(flow) > 0 && all(c("id", "toid") %in% names(flow))) {

      chk <- tryCatch(
        check_hy_graph(flow[, c("id", "toid")]),
        error = function(e) e)

      if (!isTRUE(chk)) {

        issues <- c(issues,
          "domain_graph cycle: flow edges contain a cycle (failed check_hy_graph)")

      }

    }

  }

  # ---- Check 5: nexus existence in domain_graph edges ------------------

  if (!is.null(g) && nrow(g) > 0 && "nexus_id" %in% names(g)) {

    reg_ids <- decomposition$nexus_registry$nexus_id %||% character(0)

    unknown <- setdiff(g$nexus_id, reg_ids)

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
