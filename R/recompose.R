##### recompose.R -- recompose a domain decomposition by accumulating downstream #####
#
# Narrow scope per the 2026-04-29 recompose-scope decision: a single
# numeric variable on `decomposition$source_network`, accumulated through
# the fabric in two passes. Bit-for-bit reference implementation is
# `accumulate_downstream(source_network, var)`.

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
#' @param decomposition A `domain_decomposition` returned by
#'   [decompose_network()].
#' @param var character(1). Name of a numeric column on
#'   `decomposition$source_network` to accumulate downstream through
#'   the fabric.
#' @returns The `source_network` tibble with the per-row
#'   downstream-accumulated value of `var` populated. Class and row
#'   order match the input `source_network`.
#' @seealso [accumulate_downstream()], [decompose_network()].
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
recompose <- function(decomposition, var) {

  if (!inherits(decomposition, "domain_decomposition"))
    stop("recompose: decomposition must be a domain_decomposition.",
      call. = FALSE)

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

  per_domain <- lapply(decomposition$domains, function(d) {

    catch <- d$catchments

    list(
      ids = as.character(catch$id),
      val = accumulate_downstream(catch, var, quiet = TRUE))

  })

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

  # ---- assembly --------------------------------------------------------

  src_ids <- as.character(src$id)

  vals <- pass1_lookup[src_ids]

  is_extnet <- src_ids %in% names(pass2_lookup)

  vals[is_extnet] <- pass2_lookup[src_ids[is_extnet]]

  out <- src
  out[[var]] <- unname(vals)

  out
}
