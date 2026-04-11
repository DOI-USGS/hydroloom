#' @title Check and Repair Geometry Validity
#' @description Validates sf geometry, repairs invalid features, unifies
#'   geometry types, and optionally reprojects. Handles GEOMETRYCOLLECTION
#'   artifacts from \code{st_make_valid()} by extracting the dominant
#'   geometry type.
#' @param x sf data.frame, sfc geometry, or NULL.
#' @param ... additional arguments passed to methods.
#' @returns Same class as input: sf data.frame, sfc, or NULL.
#' @export
#' @examples
#' library(sf)
#' p1 <- st_polygon(list(
#'   rbind(c(0, 0), c(10, 0), c(10, 10), c(0, 10), c(0, 0)),
#'   rbind(c(2, 2), c(2, 8), c(8, 8), c(8, 2), c(2, 2))
#' ))
#' x <- st_sf(geometry = st_sfc(p1, crs = 5070))
#' result <- check_valid(x)
#'
check_valid <- function(x, ...) {
  UseMethod("check_valid")
}

#' @export
check_valid.default <- function(x, ...) {
  if (is.null(x)) return(NULL)
  stop("check_valid requires sf or sfc input")
}

#' @rdname check_valid
#' @param out_prj crs. Target CRS for the output, passed to
#'   \code{\link[sf]{st_transform}}. Defaults to the CRS of \code{x}.
#' @export
check_valid.sf <- function(x, out_prj = sf::st_crs(x), ...) {

  return_now <- FALSE

  x <- sf::st_zm(x)

  if (!all(sf::st_is_valid(x))) {

    if (interactive())
      message("Found invalid geometry, attempting to fix.")

    orig_type <- unique(as.character(sf::st_geometry_type(x)))

    orig_type <- orig_type[grepl("POLY|LINE", orig_type)]

    x <- tryCatch({
      sf::st_make_valid(x)
    }, error = function(e) {
      warning("Error trying to make geometry valid. Returning invalid geometry.")
      return_now <<- TRUE
      x
    })

    if (return_now) return(x)

    tryCatch({

      if (!all(sf::st_geometry_type(x) == orig_type)) {
        if (any(grepl("^GEOMETRY", sf::st_geometry_type(x)))) {

          sf::st_geometry(x) <-
            sf::st_sfc(lapply(sf::st_geometry(x), fix_g_type,
                              type = gsub("^MULTI", "", orig_type),
                              orig_type = orig_type),
                       crs = sf::st_crs(x))

          x <- sf::st_cast(x, orig_type)

        }
      }
    }, error = function(e) {
      warning("Error while trying to unify geometry type. ",
              "\nReturning geometry as is.")
      return_now <<- TRUE
      x
    })

    if (return_now) return(x)

  }

  if (any(grepl("POLYGON", class(sf::st_geometry(x))))) {
    use_s2 <- sf::sf_use_s2()
    suppressMessages(sf::sf_use_s2(FALSE))
    on.exit(suppressMessages(sf::sf_use_s2(use_s2)), add = TRUE)
    suppressMessages(suppressWarnings(
      x <- sf::st_buffer(x, 0)))
  }

  if (as.character(sf::st_geometry_type(x, by_geometry = FALSE)) ==
     "MULTIPOLYGON") {
    if (all(vapply(sf::st_geometry(x), length, integer(1)) == 1L)) {
      try(suppressWarnings(x <- sf::st_cast(x, "POLYGON")))
    }
  }

  if (as.character(sf::st_geometry_type(x, by_geometry = FALSE)) ==
     "MULTILINESTRING") {
    if (all(vapply(sf::st_geometry(x), length, integer(1)) == 1L)) {
      try(suppressWarnings(x <- sf::st_cast(x, "LINESTRING")))
    }
  }

  if (sf::st_crs(x) != sf::st_crs(out_prj)) {
    x <- sf::st_transform(x, out_prj)
  }

  types <- as.character(sf::st_geometry_type(x, by_geometry = TRUE))

  if (any(grepl("^GEOME", types))) {
    unq <- unique(as.character(
      sf::st_geometry_type(x, by_geometry = TRUE)))

    cast_to <- unq[which.max(tabulate(match(types, unq)))]

    if (any(grepl("^MULTI", unq)) & !grepl("^MULTI", cast_to)) {
      cast_to <- paste0("MULTI", cast_to)
    }

    tryCatch(x <- suppressWarnings(sf::st_cast(x, cast_to)),
             error = function(e) {
               warning("\n\n Failed to unify output geometry type. \n\n",
                       e,
                       "\n Dropping non-", cast_to, " geometries. \n")
             })

    r <- nrow(x)

    x <- x[sf::st_geometry_type(x, by_geometry = TRUE) == cast_to, ]

    if (r != nrow(x)) {
      x <- sf::st_cast(x, cast_to)
    }
  }

  suppressWarnings(x <- sf::st_simplify(x, dTolerance = 0))

  x
}

#' @rdname check_valid
#' @export
check_valid.sfc <- function(x, out_prj = sf::st_crs(x), ...) {

  crs <- sf::st_crs(x)
  x <- sf::st_zm(x)

  if (!all(sf::st_is_valid(x))) {

    if (interactive())
      message("Found invalid geometry, attempting to fix.")

    orig_type <- unique(as.character(sf::st_geometry_type(x)))
    orig_type <- orig_type[grepl("POLY|LINE", orig_type)]

    x <- tryCatch(
      sf::st_make_valid(x),
      error = function(e) {
        warning("Error trying to make geometry valid. Returning invalid geometry.")
        x
      })

    if (!all(sf::st_geometry_type(x) == orig_type)) {
      if (any(grepl("^GEOMETRY", sf::st_geometry_type(x)))) {

        x <- sf::st_sfc(
          lapply(x, fix_g_type,
                 type = gsub("^MULTI", "", orig_type),
                 orig_type = orig_type),
          crs = crs)

        x <- sf::st_cast(x, orig_type)
      }
    }
  }

  if (any(grepl("POLYGON", class(x)))) {
    use_s2 <- sf::sf_use_s2()
    suppressMessages(sf::sf_use_s2(FALSE))
    on.exit(suppressMessages(sf::sf_use_s2(use_s2)), add = TRUE)
    suppressMessages(suppressWarnings(
      x <- sf::st_buffer(x, 0)))
  }

  gtype <- as.character(sf::st_geometry_type(x, by_geometry = FALSE))

  if (gtype == "MULTIPOLYGON") {
    if (all(vapply(x, length, integer(1)) == 1L))
      try(suppressWarnings(x <- sf::st_cast(x, "POLYGON")))
  }

  if (sf::st_crs(x) != sf::st_crs(out_prj))
    x <- sf::st_transform(x, out_prj)

  types <- as.character(sf::st_geometry_type(x, by_geometry = TRUE))

  if (any(grepl("^GEOME", types))) {
    unq <- unique(types)
    cast_to <- unq[which.max(tabulate(match(types, unq)))]

    if (any(grepl("^MULTI", unq)) & !grepl("^MULTI", cast_to))
      cast_to <- paste0("MULTI", cast_to)

    tryCatch(
      x <- suppressWarnings(sf::st_cast(x, cast_to)),
      error = function(e) {
        warning("\n\n Failed to unify output geometry type. \n\n", e)
      })

    keep <- sf::st_geometry_type(x, by_geometry = TRUE) == cast_to
    if (!all(keep)) x <- x[keep]
  }

  suppressWarnings(x <- sf::st_simplify(x, dTolerance = 0))

  x
}

#' @noRd
fix_g_type <- function(g, type = "POLYGON", orig_type = "MULTIPOLYGON") {

  tryCatch({

    if (sf::st_is_empty(g)) {

      get_empty(type)

    } else if (grepl("^MULTI|^GEOM", sf::st_geometry_type(g))) {

      sf::st_cast(
        sf::st_sfc(g[grepl(type, vapply(g, sf::st_geometry_type, character(1)))]),
        orig_type)

    } else {

      sf::st_cast(g, orig_type)

    }
  }, error = function(e) {

    sf::st_sfc(g)

  })

}

#' @noRd
get_empty <- function(type) {
  switch(type,
    POLYGON          = sf::st_polygon(),
    MULTIPOLYGON     = sf::st_multipolygon(),
    LINESTRING       = sf::st_linestring(),
    MULTILINESTRING  = sf::st_multilinestring(),
    POINT            = sf::st_point(),
    MULTIPOINT       = sf::st_multipoint(),
    stop("unexpected geometry type"))
}
