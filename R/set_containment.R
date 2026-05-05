##### set_containment.R -- declare containment between domains post-decomposition #####
#
# Setter that records `containing_domain_id` on one or more domains in
# an existing `domain_decomposition`. The relationship is supplied by
# the caller, not detected: `decompose_network()` treats every
# disconnected component as an independent drainage basin, and this
# function tells the package that one of those basins (typically an
# endorheic basin or a drainage-divide remnant) is to be treated as
# belonging inside another. Containment is recorded on the contained
# domain and not in `nexus_registry`, since no flow crosses a hydro
# nexus between the two.

#' Declare containment between domains
#'
#' @description
#' Records that one domain in an existing `domain_decomposition` is
#' contained by another. In HY_Features terms a domain corresponds to
#' a catchment aggregate -- a set of catchments grouped together that
#' is itself a catchment. Declaring containment says one aggregate
#' (typically an endorheic basin, a drainage-divide remnant, or any
#' other isolated component) belongs inside another. The relationship
#' is supplied by the caller, not detected: `decompose_network()`
#' partitions disconnected components into independent basins, and
#' this function tells the package which of those basins are to be
#' treated as contained.
#'
#' @details
#' [recompose()] applies `containing_domain_id` when called with
#' `containment = "accumulate"`: the contained basin's accumulated
#' value at its outlet is added at the containing domain's outlet
#' (the most-downstream row of the containing domain's segment of
#' the extensive network) and routed downstream from there through
#' the containing basin's extensive network. The default
#' `containment = "ignore"` leaves a contained basin's accumulated
#' value at its own outlet, which is correct for a true endorheic
#' basin. [get_domain_graph()] surfaces every non-NA
#' `containing_domain_id` as one row in its returned edge list, with
#' `nexus_id = NA` and `relation_type = "contained"`. Containment
#' does not appear in `nexus_registry` because no flow crosses a
#' hydro nexus between the two domains.
#'
#' Containment is transitive: if A is contained by B and B is
#' contained by C, [recompose()] processes A before B and B before C
#' so that C ends up carrying the combined contributions of all
#' three. The `containing_domain_id` slot itself is scalar -- one
#' direct container per domain. A domain may not be contained by
#' itself, and the sequence of containers may not loop back on
#' itself; [validate_decomposition()] catches both.
#'
#' Multiple pairs may be set in one call: `contained` and `containing`
#' must be the same length, or `containing` may be length 1 and is
#' then recycled across all `contained` ids.
#'
#' @param decomposition object of class `domain_decomposition`.
#' @param contained character. One or more domain ids (keys of
#'   `decomposition$domains`) to mark as contained.
#' @param containing character. The domain ids each `contained` belongs
#'   inside. Length 1 (recycled) or the same length as `contained`.
#' @returns the updated `domain_decomposition`. Errors if any id is
#'   unknown, if a domain would be contained by itself, or if the
#'   resulting decomposition fails [validate_decomposition()].
#' @seealso [domain_decomposition], [decompose_network()],
#'   [validate_decomposition()], [get_domain_graph()], [recompose()].
#' @export
#' @examples
#' src <- hy(data.frame(
#'   id = 1:5, toid = c(2L, 3L, 0L, 5L, 0L),
#'   topo_sort = c(3L, 2L, 1L, 2L, 1L),
#'   levelpath = c(1L, 1L, 1L, 2L, 2L),
#'   levelpath_outlet_id = c(3L, 3L, 3L, 5L, 5L)))
#'
#' d <- structure(
#'   list(
#'     domains = list(
#'       T1 = hy_domain("T1", "n_t1", character(0), NA_character_,
#'         hy(src[1:3, ])),
#'       E1 = hy_domain("E1", "n_e1", character(0), NA_character_,
#'         hy(src[4:5, ]))),
#'     domain_connectivity = list(),
#'     overrides = NULL,
#'     catchment_domain_index = c(setNames(rep("T1", 3), 1:3),
#'       setNames(rep("E1", 2), 4:5)),
#'     nexus_registry = data.frame(
#'       nexus_id = c("n_t1", "n_e1"),
#'       from_domain_id = c("T1", "E1"),
#'       to_domain_id = c(NA_character_, NA_character_),
#'       stringsAsFactors = FALSE),
#'     source_network = src),
#'   class = "domain_decomposition")
#'
#' d <- set_containment(d, contained = "E1", containing = "T1")
#'
#' get_domain_graph(d, relations = "contained")
#'
set_containment <- function(decomposition, contained, containing) {

  # TODO (api): the contained basin's contribution is currently
  # injected at the containing domain's outlet -- the most-downstream
  # extensive-network row of the containing domain's segment. Add an
  # optional argument that lets the caller specify any topological
  # connection point within the containing domain. Required for
  # multi-outlet roots (no unique outlet; see
  # dev/multi_outlet_root_domains.md) and useful for above-divide
  # cases where a contained basin enters the containing one upstream
  # of the outlet.

  if (!inherits(decomposition, "domain_decomposition"))
    stop("set_containment: decomposition must be a domain_decomposition.",
      call. = FALSE)

  contained  <- as.character(contained)
  containing <- as.character(containing)

  if (length(containing) == 1L && length(contained) > 1L)
    containing <- rep(containing, length(contained))

  if (length(contained) != length(containing))
    stop("set_containment: contained and containing must be same length ",
      "(or containing length 1 to recycle).", call. = FALSE)

  if (length(contained) == 0L) return(decomposition)

  if (any(is.na(contained) | !nzchar(contained)) ||
      any(is.na(containing) | !nzchar(containing)))
    stop("set_containment: contained and containing must be non-empty ",
      "domain ids.", call. = FALSE)

  if (any(contained == containing))
    stop("set_containment: a domain cannot contain itself.",
      call. = FALSE)

  keys <- names(decomposition$domains) %||% character(0)

  unknown <- setdiff(c(contained, containing), keys)

  if (length(unknown) > 0L)
    stop("set_containment: unknown domain id(s): ",
      paste(shQuote(unknown), collapse = ", "), call. = FALSE)

  for (i in seq_along(contained))
    decomposition$domains[[contained[i]]]$containing_domain_id <-
      containing[i]

  v <- validate_decomposition(decomposition)

  if (!v$valid)
    stop("set_containment: post-set validation failed:\n  ",
      paste(v$issues, collapse = "\n  "), call. = FALSE)

  decomposition

}

#' Look up a domain's containing domain
#'
#' @description
#' Returns the `containing_domain_id` slot for one or more domains in a
#' decomposition. Containment is declared via [set_containment()];
#' domains without a container return `NA_character_`.
#'
#' @param decomposition object of class `domain_decomposition`.
#' @param domain_id scalar or vector of domain ids (keys of
#'   `decomposition$domains`).
#' @returns character vector of containing domain ids, same length as
#'   `domain_id`. `NA_character_` for domains that have no container.
#' @seealso [set_containment()], [get_domain_graph()],
#'   [domain_decomposition].
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
#' get_containing_domain(d, names(d$domains)[1])
#'
get_containing_domain <- function(decomposition, domain_id) {

  if (!inherits(decomposition, "domain_decomposition"))
    stop("get_containing_domain: decomposition must be a ",
      "domain_decomposition.", call. = FALSE)

  domain_id <- as.character(domain_id)

  keys <- names(decomposition$domains) %||% character(0)

  unknown <- setdiff(domain_id, keys)

  if (length(unknown) > 0L)
    stop("get_containing_domain: unknown domain id(s): ",
      paste(shQuote(unknown), collapse = ", "), call. = FALSE)

  vapply(domain_id, function(id) {

    cd <- decomposition$domains[[id]]$containing_domain_id

    if (length(cd) == 0L) NA_character_ else as.character(cd)[1L]

  }, character(1L), USE.NAMES = FALSE)

}
