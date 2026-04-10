#' @title Dissolve Polygons
#' @description Dissolves (unions) a set of polygons, optionally by group,
#'   fills interior holes below a configurable area threshold, and returns
#'   clean, valid polygon geometry. Designed for merging catchments, HUC
#'   boundaries, or other polygon coverages where sliver gaps and pinhole
#'   holes are artifacts rather than real features.
#' @param polys sf data.frame. Input polygons with POLYGON or MULTIPOLYGON
#'   geometry.
#' @param group_id character or NULL. Column name to dissolve by. When NULL,
#'   all polygons are dissolved into a single geometry. Default NULL.
#' @param max_hole_area numeric. Maximum hole area (in square meters when
#'   using a projected CRS) below which holes are removed. Use \code{0} to
#'   keep all holes, or \code{Inf} to remove all holes. Default \code{Inf}.
#' @param gap_tolerance numeric. Buffer distance (in CRS units, typically
#'   meters) for the expand-contract step that absorbs sliver gaps between
#'   input polygons. Use \code{0} to skip. Default \code{0}.
#' @param single_polygon logical. If TRUE, extract the largest polygon from
#'   each MULTIPOLYGON result, guaranteeing single POLYGON output per row.
#'   Default FALSE.
#' @param work_crs anything accepted by \code{\link[sf]{st_crs}}, or NULL.
#'   When non-NULL, input is transformed to this CRS for processing and back
#'   to the original CRS on return. When NULL, the input CRS is used as-is.
#'   Default \code{5070} (CONUS Albers Equal Area).
#' @returns sf data.frame with dissolved, hole-filled polygon(s).
#' @note For very large inputs (10,000+ polygons), pre-dissolving with
#'   \code{terra::aggregate()} may be faster. The \code{geos} package is
#'   used automatically when installed for faster union operations.
#' @export
#' @examples
#' library(sf)
#'
#' # Three adjacent squares
#' p1 <- st_polygon(list(rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0))))
#' p2 <- st_polygon(list(rbind(c(1, 0), c(2, 0), c(2, 1), c(1, 1), c(1, 0))))
#' p3 <- st_polygon(list(rbind(c(2, 0), c(3, 0), c(3, 1), c(2, 1), c(2, 0))))
#' polys <- st_sf(
#'   grp = c("a", "a", "b"),
#'   geometry = st_sfc(p1, p2, p3, crs = 5070)
#' )
#'
#' # Dissolve all into one polygon
#' dissolve_polygons(polys, work_crs = NULL)
#'
#' # Dissolve by group
#' dissolve_polygons(polys, group_id = "grp", work_crs = NULL)
#'
dissolve_polygons <- function(polys,
                              group_id = NULL,
                              max_hole_area = Inf,
                              gap_tolerance = 0,
                              single_polygon = FALSE,
                              work_crs = 5070) {

  # -- 1. Validate inputs --
  if (!inherits(polys, "sf"))
    stop("polys must be an sf data.frame")

  gtypes <- as.character(sf::st_geometry_type(polys, by_geometry = FALSE))
  if (!gtypes %in% c("POLYGON", "MULTIPOLYGON", "GEOMETRY"))
    stop("polys must have POLYGON or MULTIPOLYGON geometry, got: ", gtypes)

  if (!is.null(group_id)) {
    if (!group_id %in% names(polys))
      stop("group_id column '", group_id, "' not found in polys")
  }

  # -- 2. CRS handling --
  orig_crs <- sf::st_crs(polys)

  if (!is.null(work_crs)) {
    work_crs <- sf::st_crs(work_crs)
    polys <- sf::st_transform(polys, work_crs)
  }

  # -- 3. Dissolve --
  merged <- dissolve_union(polys, group_id)

  # -- 4. Validate --
  merged <- check_valid(merged)

  # -- 5. Gap absorption --
  if (gap_tolerance > 0) {
    sf::st_geometry(merged) <- sf::st_buffer(
      sf::st_geometry(merged), gap_tolerance)
    sf::st_geometry(merged) <- sf::st_buffer(
      sf::st_geometry(merged), -gap_tolerance)
    merged <- check_valid(merged)
  }

  # -- 6. Fill holes --
  if (max_hole_area > 0) {
    crs <- sf::st_crs(merged)
    sf::st_geometry(merged) <- sf::st_sfc(
      lapply(sf::st_geometry(merged), remove_small_holes,
             max_hole_area = max_hole_area, crs = crs),
      crs = crs)
  }

  # -- 7. Force single polygon --
  if (single_polygon) {
    crs <- sf::st_crs(merged)
    sf::st_geometry(merged) <- sf::st_sfc(
      lapply(sf::st_geometry(merged), keep_largest_polygon, crs = crs),
      crs = crs)
  }

  # -- 8. Final validation and reproject --
  out_prj <- if (!is.null(work_crs)) orig_crs else sf::st_crs(merged)
  merged <- check_valid(merged, out_prj = out_prj)

  merged
}

#' Perform the union/dissolve step
#' @noRd
dissolve_union <- function(polys, group_id) {

  use_geos <- requireNamespace("geos", quietly = TRUE)

  if (is.null(group_id)) {
    geom <- union_geom(sf::st_geometry(polys), use_geos)
    sf::st_sf(geometry = geom)
  } else {
    # Split by group, union each, recombine
    groups <- unique(polys[[group_id]])
    results <- lapply(groups, function(g) {
      sub <- polys[polys[[group_id]] == g, ]
      geom <- union_geom(sf::st_geometry(sub), use_geos)
      out <- sf::st_sf(geometry = geom)
      out[[group_id]] <- g
      out
    })
    do.call(rbind, results)
  }
}

#' Union an sfc geometry collection
#' @noRd
union_geom <- function(geom, use_geos = FALSE) {

  if (use_geos) {
    g <- geos::as_geos_geometry(geom)
    g <- geos::geos_make_collection(g)
    g <- geos::geos_unary_union(g)
    sf::st_as_sfc(g)
  } else {
    use_s2 <- sf::sf_use_s2()
    sf::sf_use_s2(FALSE)
    on.exit(sf::sf_use_s2(use_s2))
    sf::st_sfc(sf::st_union(geom), crs = sf::st_crs(geom))
  }
}

#' Remove holes from a single geometry below an area threshold
#' @noRd
remove_small_holes <- function(geom, max_hole_area, crs) {

  if (sf::st_is_empty(geom)) return(geom)

  # Handle MULTIPOLYGON: process each component
  if (inherits(geom, "MULTIPOLYGON")) {
    parts <- lapply(geom, function(p) {
      remove_holes_from_polygon(p, max_hole_area, crs)
    })
    return(sf::st_multipolygon(parts))
  }

  # Single POLYGON
  sf::st_polygon(remove_holes_from_polygon(geom, max_hole_area, crs))
}

#' Remove holes from a single polygon ring list
#' @param rings list of coordinate matrices (outer ring + hole rings)
#' @noRd
remove_holes_from_polygon <- function(rings, max_hole_area, crs) {

  if (length(rings) <= 1) return(rings)

  # Fast path: remove all holes
  if (is.infinite(max_hole_area)) return(rings[1])

  # Build sfc of hole-ring polygons for batch area computation
  hole_polys <- sf::st_sfc(
    lapply(rings[-1], function(r) sf::st_polygon(list(r))),
    crs = crs)

  areas <- as.numeric(sf::st_area(hole_polys))

  keep <- c(TRUE, areas >= max_hole_area)

  rings[keep]
}

#' Extract the largest polygon from a geometry
#' @noRd
keep_largest_polygon <- function(geom, crs) {

  if (sf::st_is_empty(geom)) return(geom)

  # Already a single POLYGON
  if (inherits(geom, "POLYGON")) return(geom)

  # MULTIPOLYGON: find largest component
  parts <- sf::st_cast(sf::st_sfc(geom, crs = crs), "POLYGON")

  if (length(parts) == 1) return(parts[[1]])

  areas <- as.numeric(sf::st_area(parts))
  parts[[which.max(areas)]]
}
