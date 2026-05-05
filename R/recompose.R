##### recompose.R -- recompose a domain decomposition by accumulating downstream #####
#
# Narrow scope per the 2026-04-29 recompose-scope decision: a single
# numeric variable on `decomposition$source_network`, accumulated through
# the fabric in two passes. Bit-for-bit reference implementation is
# `accumulate_downstream(source_network, var)`.
#
# An optional third step adds contained-basin contributions: when
# `containment = "accumulate"`, each contained basin's accumulated
# value at its outlet is added at the containing domain's outlet
# (the most-downstream row of the containing domain's segment of the
# extensive network) and routed downstream from there through the
# containing basin's extensive network. For containing basins that
# do not have an extensive-network overlay (sub-threshold basins),
# the value is added at the basin outlet row.

#' Recompose a domain decomposition by accumulating an attribute downstream
#'
#' @description
#' Accumulates a numeric variable on `decomposition$source_network`
#' through the decomposed fabric and returns the source network with
#' the per-row downstream-accumulated value populated.
#'
#' @details
#' Two passes. The decomposed form is engineered so each pass is a
#' single [accumulate_downstream()][accumulate_downstream] call.
#'
#' Pass 1 -- per-domain accumulation produces, for every extensive
#' network row, the locally-incremental value (own + all laterals
#' draining transitively to that row). Lateral rows receive their full
#' upstream-cumulative within the domain, which equals the basin-wide
#' cumulative for that row since laterals never cross a domain
#' boundary.
#'
#' Pass 2 -- per-basin extensive-network accumulation walks the
#' basin's `domain_connectivity` overlay end-to-end, seeded with the
#' locally-incremental values from pass 1, to produce the basin-wide
#' cumulative at every extensive network row.
#'
#' Sub-threshold basins (no `domain_connectivity` overlay) carry their
#' single domain's pass 1 values straight through.
#'
#' **Containment.** With `containment = "accumulate"` (opt-in), each
#' contained basin's accumulated value at its outlet is added at the
#' containing domain's outlet -- the most-downstream row of the
#' containing domain's segment of the extensive network -- and
#' routed downstream from there through the containing basin's
#' extensive network. Transitive containment (A inside B inside C)
#' is processed from the most-contained outward, so A's contribution
#' is added to B before B's value is read, and B's combined value
#' is added to C. With the default `containment = "ignore"` a
#' contained basin's accumulated value stops at its own outlet --
#' the correct behavior for a true endorheic basin and the only
#' behavior before this argument was added. See [set_containment()]
#' for how containment is declared.
#'
#' @param decomposition A `domain_decomposition` returned by
#'   [decompose_network()].
#' @param var character(1). Name of a numeric column on
#'   `decomposition$source_network` to accumulate downstream through
#'   the fabric.
#' @param containment character(1). One of `"ignore"` (default -- a
#'   contained basin's accumulated value stops at its own outlet) or
#'   `"accumulate"` (the contained basin's accumulated value is
#'   added at the containing domain's outlet and routed downstream
#'   through the containing basin's extensive network).
#' @returns The `source_network` tibble with the per-row
#'   downstream-accumulated value of `var` populated. Class and row
#'   order match the input `source_network`.
#' @seealso [accumulate_downstream()], [decompose_network()],
#'   [set_containment()].
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
#' rec <- recompose(d, "da_sqkm")
#'
#' head(rec[, c("id", "da_sqkm")])
#'
recompose <- function(decomposition, var,
                      containment = c("ignore", "accumulate")) {

  if (!inherits(decomposition, "domain_decomposition"))
    stop("recompose: decomposition must be a domain_decomposition.",
      call. = FALSE)

  containment <- match.arg(containment)

  var <- as.character(var)

  if (length(var) != 1L || is.na(var))
    stop("recompose: var must be a single column name.", call. = FALSE)

  src <- decomposition$source_network

  if (is.null(src) || !var %in% names(src))
    stop("recompose: var '", var, "' not found on source_network.",
      call. = FALSE)

  if (!is.numeric(src[[var]]))
    stop("recompose: var '", var, "' must be numeric.", call. = FALSE)

  if (length(decomposition$domains) == 0L) {
    src[[var]] <- numeric(0)
    return(src)
  }

  # ---- pass 1: per-domain accumulate ----------------------------------
  # accumulate_downstream() returns values aligned with x$id (it
  # left-joins by id at the end), so the result lines up with
  # catchments$id row-for-row. Domains partition source ids
  # (validator's coverage check), so the concatenated lookup is
  # unique by id.
  #
  # NOTE (perf): per-domain accumulation is independent by
  # construction (the decomposed form is engineered for it). The
  # MVP is intentionally sequential per the recompose plan; a
  # drop-in `future.apply::future_lapply` would parallelize this
  # and the equivalent per-basin loop below. Flip when wall-clock
  # asks.

  per_domain <- lapply(decomposition$domains, function(d) {

    catch <- d$catchments

    list(
      ids = as.character(catch$id),
      val = accumulate_downstream(catch, var, quiet = TRUE))

  })

  # NOTE (perf): pass1_lookup and pass2_lookup are named numeric
  # vectors keyed by source id. At 10^7 rows the names alone are
  # roughly 160 MB. Streaming each basin's overlay output directly
  # into the final `vals` vector via match() (integer-position
  # indexing, no names) would roughly halve peak memory. No
  # algorithmic change. Worth it once the at-scale benchmark
  # asks.
  pass1_lookup <- setNames(
    unlist(lapply(per_domain, `[[`, "val"), use.names = FALSE),
    unlist(lapply(per_domain, `[[`, "ids"), use.names = FALSE))

  # ---- pass 2: per-basin extensive-network accumulate ------------------

  per_basin <- lapply(
    names(decomposition$domain_connectivity),
    function(basin_id) {

      overlay <- decomposition$domain_connectivity[[basin_id]]

      ids_chr <- as.character(overlay$id)

      overlay[[var]] <- pass1_lookup[ids_chr]

      list(
        ids = ids_chr,
        val = accumulate_downstream(overlay, var, quiet = TRUE))

    })

  pass2_lookup <- setNames(
    unlist(lapply(per_basin, `[[`, "val"), use.names = FALSE),
    unlist(lapply(per_basin, `[[`, "ids"), use.names = FALSE))

  # ---- containment contributions (optional) ---------------------------
  # Iterates through the contained-by-containing pairs from the most-
  # contained outward (so transitive containment threads through
  # correctly). Each contained basin's outlet value is added at the
  # containing domain's outlet -- the most-downstream row of the
  # containing domain's segment of the extensive network -- and
  # routed downstream from there through the containing basin's
  # extensive network. For sub-threshold containing basins (no
  # extensive-network overlay), the value is added at the basin
  # outlet row directly.

  if (containment == "accumulate") {

    contributed <- recompose_apply_containment(decomposition,
      pass1_lookup, pass2_lookup)

    pass1_lookup <- contributed$pass1
    pass2_lookup <- contributed$pass2

  }

  # ---- assembly --------------------------------------------------------

  src_ids <- as.character(src$id)

  vals <- pass1_lookup[src_ids]

  is_extnet <- src_ids %in% names(pass2_lookup)

  vals[is_extnet] <- pass2_lookup[src_ids[is_extnet]]

  # Coverage check: every source id must have been seen by some
  # domain's pass 1. The validator's coverage rule should preclude an
  # NA here, but assert it explicitly so that misuse fails loudly
  # rather than propagating NAs into a downstream dataset.
  if (any(is.na(vals)))
    stop("recompose: pass 1 did not cover all source ids -- ",
      sum(is.na(vals)), " row(s) ended with NA. ",
      "Run validate_decomposition() to find the gap.",
      call. = FALSE)

  out <- src
  out[[var]] <- unname(vals)

  out
}

#' Apply containment contributions to the pass 1 / pass 2 lookups
#'
#' Iterates through the contained-by-containing pairs (one per non-NA
#' `containing_domain_id`) from the most-contained outward. For each
#' contained domain, computes the contained basin's outlet value
#' (from `pass2` if the basin has an extensive-network overlay,
#' otherwise from `pass1`) and adds it to the containing side of the
#' lookup -- on every overlay row at or downstream of the containing
#' domain's outlet, or at the containing basin's outlet row when the
#' basin is sub-threshold.
#'
#' Iterating from innermost outward means a sequence A in B in C
#' produces correct values: A's contribution is added to B before
#' B is read, so when B's outlet value is consulted it already
#' carries A's contribution.
#'
#' @param decomposition object of class `domain_decomposition`.
#' @param pass1 named numeric vector keyed by source id (per-domain
#'   accumulate output).
#' @param pass2 named numeric vector keyed by source id (per-basin
#'   extensive-network overlay accumulate output -- only populated
#'   for extensive-network rows in basins with overlays).
#' @returns list with named elements `pass1` and `pass2`, both
#'   updated in place.
#' @noRd
recompose_apply_containment <- function(decomposition, pass1, pass2) {

  cont_pairs <- list()

  for (d in decomposition$domains) {

    cd <- d$containing_domain_id

    if (length(cd) == 1L && !is.na(cd) && nzchar(cd))
      cont_pairs[[length(cont_pairs) + 1L]] <-
        c(d$domain_id, cd)

  }

  if (length(cont_pairs) == 0L) {
    return(list(pass1 = pass1, pass2 = pass2))
  }

  cont_edges <- as.data.frame(
    do.call(rbind, cont_pairs),
    stringsAsFactors = FALSE)

  names(cont_edges) <- c("id", "toid")

  # Innermost-outward order: sort_network returns rows in
  # upstream-to-downstream order, so iterating in row order processes
  # each contained domain before its container. For A inside B
  # inside C, A's contribution is added to B before B is read.
  sorted <- sort_network(cont_edges)

  proc_order <- seq_len(nrow(sorted))

  # Pre-compute source id -> basin id lookup once. The basin id is
  # the terminal_id of the row's outlet sub-network, which matches
  # the keys of decomposition$domain_connectivity (and, for sub-
  # threshold basins, the basin outlet's id).
  src <- decomposition$source_network

  src_sorted <- sort_network(src, split = TRUE)

  basin_by_id <- setNames(
    as.character(src_sorted$terminal_id),
    as.character(src_sorted$id))

  src_outlet <- as.character(get_outlet_value(src))

  src_toid_by_id <- setNames(
    as.character(src$toid),
    as.character(src$id))

  for (i in proc_order) {

    contained_id  <- sorted$id[i]
    containing_id <- sorted$toid[i]

    contained_dom  <- decomposition$domains[[contained_id]]
    containing_dom <- decomposition$domains[[containing_id]]

    # ---- contained basin outlet value -------------------------------

    c_basin_id <- basin_by_id[
      as.character(contained_dom$catchments$id[1])]

    c_overlay <- decomposition$domain_connectivity[[c_basin_id]]

    c_val <- if (!is.null(c_overlay)) {
      pass2[c_basin_id]
    } else {
      pass1[c_basin_id]
    }

    if (is.na(c_val)) next

    # ---- containing domain's outlet extensive-network row ----------
    # TODO (api): when set_containment() gains an optional connection
    # point (see its TODO and dev/multi_outlet_root_domains.md), read
    # the per-pair selection here and use it instead of the
    # always-the-outlet default below. The default is well-defined
    # for single-outlet domains and undefined for multi-outlet roots.

    p_outlet_id <- domain_outlet_extensive_network_id(containing_dom,
      src_toid_by_id)

    p_basin_id <- basin_by_id[
      as.character(containing_dom$catchments$id[1])]

    p_overlay <- decomposition$domain_connectivity[[p_basin_id]]

    if (!is.null(p_overlay)) {

      # Walk the basin overlay downstream from p_outlet_id; add c_val
      # to pass2 at every visited row.
      downstream <- walk_overlay_downstream(p_overlay, p_outlet_id)

      pass2[downstream] <- pass2[downstream] + c_val

    } else {

      # Sub-threshold containing basin: pass1 at the basin outlet row
      # already holds the basin total; just add c_val there.
      pass1[as.character(p_outlet_id)] <-
        pass1[as.character(p_outlet_id)] + c_val

    }

  }

  list(pass1 = pass1, pass2 = pass2)
}

#' Find a domain's outlet extensive-network row id
#'
#' The outlet of a domain is the single extensive-network row whose
#' original source-network `toid` points outside the domain --
#' either to the next-downstream domain or to the source-network
#' outlet value. All other extensive-network rows in the same
#' domain point to extensive-network rows inside the domain (within
#' the same segment).
#'
#' @param domain hy_domain.
#' @param src_toid_by_id named character vector keyed by source id.
#' @returns character(1) catchment id.
#' @noRd
domain_outlet_extensive_network_id <- function(domain, src_toid_by_id) {

  catch <- domain$catchments

  catch_outlet_value <- as.character(get_outlet_value(catch))

  is_ext <- as.character(catch$toid) == catch_outlet_value

  ext_ids <- as.character(catch$id[is_ext])

  if (length(ext_ids) == 1L) return(ext_ids)

  catch_ids <- as.character(catch$id)

  for (id in ext_ids) {

    src_toid <- src_toid_by_id[id]

    if (is.na(src_toid) || !src_toid %in% catch_ids) return(id)

  }

  # Fallback: shouldn't happen for a well-formed decomposition.
  ext_ids[1]

}

#' Walk a basin overlay downstream from a starting id
#'
#' Follows `toid` links in the overlay until the reserved outlet
#' value is reached. Returns the visited ids in walk order, including
#' the start.
#'
#' The overlay is `hy_leveled` (dendritic), so each row has at most
#' one downstream neighbor; the walk is unambiguous.
#'
#' @param overlay hy_leveled basin overlay.
#' @param start_id character(1) starting catchment id.
#' @returns character vector of visited ids (start first).
#' @noRd
walk_overlay_downstream <- function(overlay, start_id) {

  ids <- as.character(overlay$id)

  toid_by_id <- setNames(as.character(overlay$toid), ids)

  outlet_value <- as.character(get_outlet_value(overlay))

  collected <- character(0)

  node <- as.character(start_id)

  while (!is.na(node) && nzchar(node) && node %in% ids) {

    collected <- c(collected, node)

    next_node <- toid_by_id[[node]]

    if (is.na(next_node) || next_node == outlet_value) break

    node <- next_node

  }

  collected

}
