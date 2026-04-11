#' @title Dissolve Polygons
#' @description Dissolves (unions) a set of polygons, optionally by group,
#'   fills interior holes below a configurable area threshold, and returns
#'   clean, valid polygon geometry. Designed for merging catchments, HUC
#'   boundaries, or other polygon coverages where sliver gaps and pinhole
#'   holes are artifacts rather than real features.
#' @param polys sf data.frame or sfc geometry. Input polygons with POLYGON or
#'   MULTIPOLYGON geometry. When sfc is provided, \code{group_id} is ignored
#'   and the return value is sfc.
#' @param ... additional arguments passed to methods.
#' @returns sf data.frame or sfc (matching input class) with dissolved,
#'   hole-filled polygon(s).
#' @note For very large inputs (10,000+ polygons), pre-dissolving with
#'   \code{terra::aggregate()} may be faster. The \code{geos} package is
#'   used automatically when installed for faster union operations.
#' @export
#' @examples
#' # Three adjacent squares
#' p1 <- sf::st_polygon(list(rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0))))
#' p2 <- sf::st_polygon(list(rbind(c(1, 0), c(2, 0), c(2, 1), c(1, 1), c(1, 0))))
#' p3 <- sf::st_polygon(list(rbind(c(2, 0), c(3, 0), c(3, 1), c(2, 1), c(2, 0))))
#' polys <- sf::st_sf(
#'   grp = c("a", "a", "b"),
#'   geometry = sf::st_sfc(p1, p2, p3, crs = 5070)
#' )
#'
#' # Dissolve all into one polygon
#' dissolve_polygons(polys, work_crs = NULL)
#'
#' # Dissolve by group
#' dissolve_polygons(polys, group_id = "grp", work_crs = NULL)
#'
#' # sfc input returns sfc
#' dissolve_polygons(sf::st_geometry(polys), work_crs = NULL)
#'
#' # Dissolve tributary basins with attribute summarisation
#' cats <- sf::read_sf(system.file("extdata/walker_cats.gpkg", package = "hydroloom"))
#' flines <- sf::read_sf(system.file("extdata/walker.gpkg", package = "hydroloom"))
#'
#' # chosen manually for demonstration
#' outlets <- c(5329365, 5329313, 5329303)
#' 
#' # Navigate upstream from each outlet to define basins
#' basins <- navigate_network_dfs(flines, outlets, reset = FALSE, direction = "up")
#'
#' # Label each catchment with its basin outlet
#' cats$basin <- NA_character_
#' for (i in seq_along(basins)) {
#'   cats$basin[cats$featureid %in% unlist(basins[[i]])] <- as.character(outlets[i])
#' }
#' cats <- cats[!is.na(cats$basin), ]
#'
#' # Join stream names for summarisation
#' cats <- dplyr::left_join(cats,
#'   sf::st_drop_geometry(dplyr::select(flines, COMID, GNIS_NAME)),
#'   by = c("featureid" = "COMID"))
#'
#' # Most common non-empty name in a group
#' most_common <- function(x) {
#'   x <- x[!is.na(x) & x != " "]
#'   if (length(x) == 0) return(NA_character_)
#'   names(sort(table(x), decreasing = TRUE))[1]
#' }
#'
#' result <- dissolve_polygons(cats, group_id = "basin",
#'   .fns = list(areasqkm = sum, GNIS_NAME = most_common))
#' dplyr::select(result, basin, GNIS_NAME, areasqkm)
#'
#' plot(sf::st_geometry(result), col = sf::sf.colors(nrow(result)))
#'
dissolve_polygons <- function(polys, ...) {
  UseMethod("dissolve_polygons")
}

#' @export
dissolve_polygons.default <- function(polys, ...) {
  stop("polys must be sf or sfc")
}

#' @rdname dissolve_polygons
#' @param group_id character or NULL. Column name to dissolve by. When NULL,
#'   all polygons are dissolved into a single geometry. Ignored for sfc input.
#'   Default NULL.
#' @param .fns named list of summary functions, or NULL. When non-NULL,
#'   each element name must match a column in \code{polys} and the
#'   corresponding function is applied to that column within each group
#'   (or across all rows when \code{group_id} is NULL). Requires
#'   \code{group_id} when more than one group is desired.
#'   Example: \code{list(AreaSqKM = sum)}.
#'   Default NULL (geometry only).
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
#' @export
dissolve_polygons.sf <- function(polys,
                                 group_id = NULL,
                                 .fns = NULL,
                                 max_hole_area = Inf,
                                 gap_tolerance = 0,
                                 single_polygon = FALSE,
                                 work_crs = 5070,
                                 ...) {

  if(!is.null(group_id)) {
    if(!group_id %in% names(polys))
      stop("group_id column '", group_id, "' not found in polys")
  }

  if (!is.null(.fns)) {
    if (!is.list(.fns) || is.null(names(.fns)))
      stop(".fns must be a named list")

    missing_cols <- setdiff(names(.fns), names(polys))
    if (length(missing_cols))
      stop(".fns column(s) not found in polys: ",
           paste(missing_cols, collapse = ", "))
  }

  groups <- if (!is.null(group_id)) polys[[group_id]] else NULL

  geom <- dissolve_polygons.sfc(
    sf::st_geometry(polys),
    max_hole_area = max_hole_area,
    gap_tolerance = gap_tolerance,
    single_polygon = single_polygon,
    work_crs = work_crs,
    groups = groups)

  if (!is.null(.fns)) {

    attrs <- summarise_attrs(polys, group_id, .fns,
                             group_order = names(geom))
    out <- sf::st_sf(attrs, geometry = geom)

  } else if (is.null(group_id)) {

    out <- sf::st_sf(geometry = geom)

  } else {

    out <- sf::st_sf(geometry = geom)
    out[[group_id]] <- names(geom)

  }

  out
}

#' @rdname dissolve_polygons
#' @param groups character or factor vector, or NULL. Optional grouping vector
#'   the same length as \code{polys}. When non-NULL, polygons are dissolved
#'   per group and the result has one geometry per unique group value. Typically
#'   passed internally by the sf method; users should use \code{group_id}
#'   instead.
#' @export
dissolve_polygons.sfc <- function(polys,
                                  max_hole_area = Inf,
                                  gap_tolerance = 0,
                                  single_polygon = FALSE,
                                  work_crs = 5070,
                                  ...,
                                  groups = NULL) {


  use_s2 <- sf::sf_use_s2()
  suppressMessages(sf::sf_use_s2(FALSE))
  on.exit(suppressMessages(sf::sf_use_s2(use_s2)))

  gtypes <- as.character(sf::st_geometry_type(polys, by_geometry = FALSE))
  if (!gtypes %in% c("POLYGON", "MULTIPOLYGON", "GEOMETRY"))
    stop("polys must have POLYGON or MULTIPOLYGON geometry, got: ", gtypes)

  orig_crs <- sf::st_crs(polys)

  if(!is.null(work_crs)) {
    work_crs <- sf::st_crs(work_crs)
    polys <- sf::st_transform(polys, work_crs)
  }

  use_geos <- requireNamespace("geos", quietly = TRUE)
  out_prj <- if (!is.null(work_crs)) orig_crs else sf::st_crs(polys)

  if (is.null(groups)) {

    polys <- dissolve_impl(
      union_geom(polys, use_geos),
      gap_tolerance, max_hole_area, single_polygon, out_prj)

  } else {

    ugroups <- unique(groups)
    parts <- lapply(ugroups, function(g) {
      dissolve_impl(
        union_geom(polys[groups == g], use_geos),
        gap_tolerance, max_hole_area, single_polygon, out_prj)
    })
    polys <- do.call(c, parts)
    names(polys) <- ugroups

  }

  polys
}

#' Post-union dissolve pipeline: validate, gap fill, hole fill, single poly
#' @noRd
dissolve_impl <- function(geom, gap_tolerance, max_hole_area,
                          single_polygon, out_prj) {

  geom <- check_valid(geom)

  if (gap_tolerance > 0) {
    geom <- sf::st_buffer(geom, gap_tolerance)
    geom <- sf::st_buffer(geom, -gap_tolerance)
    geom <- check_valid(geom)
  }

  if (max_hole_area > 0) {
    crs <- sf::st_crs(geom)
    geom <- sf::st_sfc(
      lapply(geom, remove_small_holes,
             max_hole_area = max_hole_area, crs = crs),
      crs = crs)
  }

  if (single_polygon) {
    crs <- sf::st_crs(geom)
    geom <- sf::st_sfc(
      lapply(geom, keep_largest_polygon, crs = crs),
      crs = crs)
  }

  check_valid(geom, out_prj = out_prj)
}

#' Union an sfc geometry collection
#' @noRd
union_geom <- function(geom, use_geos = FALSE) {

  if(use_geos) {
    g <- geos::as_geos_geometry(geom)
    g <- geos::geos_make_collection(g)
    g <- geos::geos_unary_union(g)
    sf::st_as_sfc(g)
  } else {
    sf::st_sfc(sf::st_union(geom), crs = sf::st_crs(geom))
  }
}

#' Remove holes from a single geometry below an area threshold
#' @noRd
remove_small_holes <- function(geom, max_hole_area, crs) {

  if(sf::st_is_empty(geom)) return(geom)

  # Handle MULTIPOLYGON: process each component
  if(inherits(geom, "MULTIPOLYGON")) {
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

  if(length(rings) <= 1) return(rings)

  # Fast path: remove all holes
  if(is.infinite(max_hole_area)) return(rings[1])

  # Build sfc of hole-ring polygons for batch area computation
  hole_polys <- sf::st_sfc(
    lapply(rings[-1], function(r) sf::st_polygon(list(r))),
    crs = crs)

  areas <- as.numeric(sf::st_area(hole_polys))

  keep <- c(TRUE, areas >= max_hole_area)

  rings[keep]
}

#' Summarise attributes using .fns within groups
#' @param group_order character. Unique group values in geometry order (from
#'   names of the sfc result). Used to align rows with geometries.
#' @noRd
summarise_attrs <- function(polys, group_id, .fns, group_order = NULL) {

  df <- sf::st_drop_geometry(polys)

  summaries <- lapply(names(.fns), function(col) {
    fn <- .fns[[col]]
    if (!is.null(group_id)) {
      tapply(df[[col]], df[[group_id]], fn)
    } else {
      fn(df[[col]])
    }
  })

  names(summaries) <- names(.fns)

  if (!is.null(group_id)) {

    out <- data.frame(summaries, stringsAsFactors = FALSE)
    out[[group_id]] <- rownames(out)
    rownames(out) <- NULL

    if (!is.null(group_order))
      out <- out[match(group_order, out[[group_id]]), , drop = FALSE]

    out <- out[, c(group_id, names(.fns)), drop = FALSE]
    rownames(out) <- NULL

  } else {

    out <- data.frame(summaries, stringsAsFactors = FALSE)

  }

  out
}

#' Extract the largest polygon from a geometry
#' @noRd
keep_largest_polygon <- function(geom, crs) {

  if(sf::st_is_empty(geom)) return(geom)

  # Already a single POLYGON
  if(inherits(geom, "POLYGON")) return(geom)

  # MULTIPOLYGON: find largest component
  parts <- sf::st_cast(sf::st_sfc(geom, crs = crs), "POLYGON")

  if(length(parts) == 1) return(parts[[1]])

  areas <- as.numeric(sf::st_area(parts))
  parts[[which.max(areas)]]
}
