
matcher <- function(coords, points, search_radius, max_matches = 1) {

  max_match_ <- ifelse(nrow(coords) < 1000, nrow(coords), 1000)

  matched <- nn2(data = coords[, 1:2],
                 query = matrix(points[, c("X", "Y")], ncol = 2),
                 k = ifelse(max_matches > 1, max_match_, 1),
                 searchtype = "radius",
                 radius = search_radius)

  matched <- data.frame(nn.idx = as.integer(matched$nn.idx),
                        nn.dists = as.numeric(matched$nn.dists),
                        point_id = rep(1:nrow(points), ncol(matched$nn.idx)))

  matched <- left_join(matched, mutate(data.frame(L1 = coords[, "L1"]),
                                       index = seq_len(nrow(coords))),
                       by = c("nn.idx" = "index"))

  matched <- filter(matched, .data$nn.dists <= search_radius)

  # First get rid of duplicate nodes on the same line.
  matched <- group_by(matched, .data$L1, .data$point_id) |>
    filter(.data$nn.dists == min(.data$nn.dists)) |>
    ungroup()

  # Now limit to max matches per point
  matched <- group_by(matched, .data$point_id) |>
    filter(row_number() <= max_matches) |>
    ungroup() |>
    as.data.frame()

  matched
}

check_search_radius <- function(search_radius, points) {

  if(is.null(search_radius)) {
    if(st_is_longlat(points)) {
      search_radius <- set_units(0.01, "degrees")
    } else {
      search_radius <- set_units(200, "m")

      units(search_radius) <- as_units(
        st_crs(points, parameters = TRUE)$ud_unit)
    }
  }

  if(!inherits(search_radius, "units")) {
    warning("search_radius units not set, trying units of points CRS.")
    units(search_radius) <- as_units(
      st_crs(points, parameters = TRUE)$ud_unit)
  }

  search_radius
}

match_crs <- function(x, y, warn_text = "") {
  if (st_crs(x) != st_crs(y)) {
    warning(warn_text)
    x <- st_transform(x, st_crs(y))
  }
  x
}

make_singlepart <- function(x, warn_text = "", stop_on_real_multi = FALSE) {
  check <- nrow(x)

  gt <- st_geometry_type(x, by_geometry = FALSE)

  x <- st_zm(x)

  if(grepl("^MULTI", gt)) {
    x <- st_cast(x, gsub("^MULTI", "", gt), warn = FALSE)
  }

  if (nrow(x) != check) {
    if(stop_on_real_multi) stop("Multipart geometries not supported.")
    warning(warn_text)
  }

  x
}

# utility function
get_fl <- function(hydro_location, net) {
  if(hydro_location$aggregate_id_measure == 100) {
    filter(net,
           .data$aggregate_id == hydro_location$aggregate_id &
             .data$aggregate_id_to_measure == hydro_location$aggregate_id_measure)
  } else {
    filter(net,
           .data$aggregate_id == hydro_location$aggregate_id &
             .data$aggregate_id_from_measure <= hydro_location$aggregate_id_measure &
             .data$aggregate_id_to_measure > hydro_location$aggregate_id_measure)
  }
}


add_index <- function(x) {
  x |>
    as.data.frame() |>
    mutate(index = seq_len(nrow(x)))
}

add_len <- function(x) {
  x |>
    mutate(len  = sqrt( ( (.data$X - (lag(.data$X))) ^ 2) +
                          ( ( (.data$Y - (lag(.data$Y))) ^ 2)))) |>
    mutate(len = replace_na(.data$len, 0)) |>
    mutate(len = cumsum(.data$len)) |>
    mutate(id_measure = 100 - (100 * .data$len / max(.data$len)))
}

interp_meas <- function(m, x1, y1, x2, y2) {
  list(x1 + (m / 100) * (x2 - x1),
       y1 + (m / 100) * (y2 - y1))
}

#' @title Index Points to Lines
#' @description given an sf point geometry column, return id, aggregate_id
#' (e.g. reachcode), and aggregate id measure for each point.
#' @inheritParams add_levelpaths
#' @param points sf or sfc of type POINT in analysis projection. NOTE: x will
#' be projected to the projection of the points layer.
#' @param search_radius units distance for the nearest neighbor search
#' to extend in analysis projection. If missing or NULL, and points are in a lon
#' lat projection, a default of 0.01 degree is used, otherwise 200 m is used.
#' Conversion to the linear unit used by the provided crs of points is attempted.
#' See RANN nn2 documentation for more details.
#' @param precision numeric the resolution of measure precision in the output in meters.
#' @param max_matches numeric the maximum number of matches to return if multiple are
#' found in search_radius
#' @returns data.frame with five columns, point_id, id, aggregate_id,
#' aggregate_id_measure, and offset. point_id is the row or list element in the
#' point input.
#' @details
#' Note 1: Inputs are cast into LINESTRINGS. Because of this, the measure output
#' of inputs that are true multipart lines may be in error.
#'
#' Note 2: This algorithm finds the nearest node in the input flowlines to
#' identify which flowline the point should belong to. As a second pass,
#' it can calculate the measure to greater precision than the nearest flowline
#' geometry node.
#'
#' Note 3: Offset is returned in units consistent with the projection of
#' the input points.
#'
#' Note 4: See `dfMaxLength` input to sf::st_segmentize() for details of
#' handling of precision parameter.
#'
#' Note 5: "from" is downstream -- 0 is the outlet "to" is upstream -- 100 is the inlet
#' @name index_points_to_lines
#' @export
#' @examples
#'
#' \donttest{
#' if(require(nhdplusTools)) {
#' source(system.file("extdata", "sample_flines.R", package = "nhdplusTools"))
#'
#' point <- sf::st_sfc(sf::st_point(c(-76.87479, 39.48233)),
#'                     crs = 4326)
#'
#' index_points_to_lines(sample_flines, point)
#'
#' point <- sf::st_transform(point, 5070)
#'
#' index_points_to_lines(sample_flines, point,
#'                       search_radius = units::set_units(200, "m"))
#'
#' index_points_to_lines(sample_flines, point, precision = 30)
#'
#' index_points_to_lines(sample_flines,
#'                       sf::st_sfc(list(sf::st_point(c(-76.86934, 39.49328)),
#'                                       sf::st_point(c(-76.91711, 39.40884)),
#'                                       sf::st_point(c(-76.88081, 39.36354))),
#'                                  crs = 4326),
#'                       search_radius = units::set_units(0.2, "degrees"),
#'                       max_matches = 10)
#'
#'  }
#'  }
#'
index_points_to_lines <- function(x, points,
                                  search_radius = NULL,
                                  precision = NA,
                                  max_matches = 1) {

  UseMethod("index_points_to_lines")

}

#' @name index_points_to_lines
#' @export
index_points_to_lines.data.frame <- function(x, points,
                                  search_radius = NULL,
                                  precision = NA,
                                  max_matches = 1) {

  x <- hy(x)

  matched <- index_points_to_lines(x, points,
                                   search_radius = search_radius,
                                   precision = precision,
                                   max_matches = max_matches)

  rename_indexed(x, matched)

}

#' @name index_points_to_lines
#' @export
index_points_to_lines.hy <- function(x, points,
                                     search_radius = NULL,
                                     precision = NA,
                                     max_matches = 1) {

  # TODO: handle for aggregate or not?
  check_names(x, c(id), "index_points_to_lines")

  in_crs <- st_crs(points)

  search_radius <- check_search_radius(search_radius, points)

  if(!is.na(precision)) {
    if(requireNamespace("geos", quietly = TRUE)) {
      point_buffer <- geos::geos_buffer(geos::as_geos_geometry(sf::st_geometry(points)),
                                        distance = search_radius)

      point_buffer <- sf::st_as_sfc(point_buffer)
    } else {
      point_buffer <- st_buffer(points, search_radius)
    }
  }

  if(units(search_radius) == units(as_units("degrees"))) {
    if(st_is_longlat(in_crs) & search_radius > set_units(1, "degree")) {
      warning("search radius is large for lat/lon input, are you sure?")
    }
  }

  x <- match_crs(x, points,
                 paste("crs of lines and points don't match.",
                       "attempting st_transform of lines"))

  search_radius <- as.numeric(search_radius) # everything in same units now

  if(!is.na(precision)) {

    x <- x[lengths(st_intersects(x, point_buffer, sparse = TRUE)) > 0, ]

  }

  x <- select(x, any_of(c(id, aggregate_id,
                        aggregate_id_from_measure, aggregate_id_to_measure))) |>
    mutate(index = seq_len(nrow(x)))

  fline_atts <- st_drop_geometry(x)

  if(st_geometry_type(x, by_geometry = FALSE) != "LINESTRING") {
    warning("converting to LINESTRING, this may be slow, check results")
  }

  suppressWarnings(x <- st_cast(x, "LINESTRING", warn = FALSE))

  if(!"XY" %in% class(st_geometry(x)[[1]])) {
    warning("dropping z coordinates, this may be slow")
    x <- st_zm(x)
  }

  if (nrow(x) != nrow(fline_atts)) {

    x <- summarise(group_by(select(x, "index"),
                            .data$index),
                   do_union = FALSE)

    x <- left_join(x, fline_atts, by = "index")

    multi <- lengths(st_geometry(x)) > 1

    if(any(multi)) {
      warning(paste0("Attempting to combine multipart lines into single ",
                     "part lines. Check results!!"))

      st_geometry(x)[multi] <- lapply(st_geometry(x)[multi], function(x) {
        st_linestring(do.call(rbind, x))
      })

      x  <- st_zm(st_cast(x, "LINESTRING", warn = FALSE))
    }
  }

  points <- st_coordinates(points)

  if(!is.na(precision)) {

    # upstream to downstream order.
    x <- st_coordinates(x)

    # Geometry nodes are in downstream to upstream order.
    x <- as.data.frame(x) |>
      st_as_sf(coords = c("X", "Y"),
                   crs = in_crs) |>
      group_by(.data$L1) |>
      summarise(do_union = FALSE)

    x <- x |>
      mutate(index = seq_len(nrow(x))) |>
      st_cast("LINESTRING", warn = FALSE) |>
      st_segmentize(dfMaxLength = as_units(precision, "m"))

    fline_atts <- right_join(fline_atts,
                             select(st_drop_geometry(x),
                                    "L1", precision_index = "index"),
                             by = c("index" = "L1"))

    # downstream to upstream order
    x <- st_coordinates(x)


    matched <- matcher(x, points, search_radius, max_matches = max_matches) |>
      left_join(select(fline_atts, id, "precision_index"),
                by = c("L1" = "precision_index"))

    matched <- mutate(matched, nn.dists = ifelse(.data$nn.dists > search_radius,
                                                 NA, .data$nn.dists))
  } else {

    x <- st_coordinates(x)


    matched <- matcher(x, points, search_radius, max_matches = max_matches) |>
      left_join(select(fline_atts, id, "index"),
                by = c("L1" = "index"))

    matched <- mutate(matched, nn.dists = ifelse(.data$nn.dists > search_radius,
                                                 NA, .data$nn.dists))

  }

  x <- x |>
    add_index() |>
    filter(.data$L1 %in% matched$L1) |>
    left_join(select(matched, all_of(c("L1", id))), by = "L1", relationship = "many-to-many") |>
    left_join(select(fline_atts, -"index"), by = id, relationship = "many-to-many")

  matched <- select(matched, point_id, node = "nn.idx", offset = "nn.dists", id)

  if(aggregate_id_from_measure %in% names(fline_atts)) {
    x <- x |>
      group_by(.data$L1) |>
      add_len() |>
      mutate(aggregate_id_measure = round(
        .data$aggregate_id_from_measure +
          (.data$aggregate_id_to_measure - .data$aggregate_id_from_measure) *
          (.data$id_measure / 100),
        digits = 4)) |>
      ungroup() |> distinct()

    select_vec <- c("index", aggregate_id, aggregate_id_measure)
    select_vec2 <- c(point_id, id, aggregate_id, aggregate_id_measure, offset)

  } else {

    select_vec <- c("index", aggregate_id)
    select_vec2 <- c(point_id, id, aggregate_id, offset)

  }

  matched <- left_join(matched,
                       distinct(select(x, all_of(select_vec))),
                       by = c("node" = "index")) |>
    select(all_of(select_vec2))

  matched
}

#' @title Index Points to Waterbodies
#' @description given an sf point geometry column, return waterbody id, and
#' COMID of dominant artificial path
#' @param waterbodies sf data.frame of type POLYGON or MULTIPOLYGON including
#' a "wbid" attribute.
#' @param points sfc of type POINT
#' @param flines sf data.frame (optional) of type LINESTRING or MULTILINESTRING including
#' id, wbid, and topo_sort attributes. If omitted, only waterbody indexes are returned.
#' @param search_radius units class with a numeric value indicating how far to
#' search for a waterbody boundary in units of provided projection. Set units with
#' \link[units]{set_units}.
#' @returns data.frame with columns, `COMID`, `in_wb_COMID`, `near_wb_COMID`,
#' `near_wb_dist`, and `outlet_fline_COMID`.
#' Distance is in units of provided projection.
#' @export
#' @examples
#'
#' source(system.file("extdata/sample_data.R", package = "nhdplusTools"))
#'
#' waterbodies <- sf::st_transform(
#'   sf::read_sf(sample_data, "NHDWaterbody"), 5070)
#'
#' points <- sf::st_transform(
#'   sf::st_sfc(sf::st_point(c(-89.356086, 43.079943)),
#'              crs = 4326), 5070)
#'
#' index_points_to_waterbodies(waterbodies, points,
#'                     search_radius = units::set_units(500, "m"))
#'
index_points_to_waterbodies <- function(waterbodies, points, flines = NULL,
                                        search_radius = NULL) {

  rename_comid <- FALSE
  if("COMID" %in% names(waterbodies)) {
    waterbodies <- rename(waterbodies, any_of(c(wbid = "COMID")))
    rename_comid <- TRUE
  }

  check_names(waterbodies, wbid, "index_points_to_waterbodies")

  points <- st_geometry(points)

  search_radius <- as.numeric(check_search_radius(search_radius, points))

  points <- st_sf(id = seq_len(length(points)), geometry = points)

  waterbodies <- select(waterbodies, wbid)

  points <- match_crs(points, waterbodies, "st_transform points to match waterbodies")

  points <- suppressMessages(st_join(points, waterbodies))

  wb_atts <- mutate(st_drop_geometry(waterbodies), index = seq_len(nrow(waterbodies)))

  waterbodies <- make_singlepart(waterbodies, stop_on_real_multi = TRUE)

  if(nrow(waterbodies) != nrow(wb_atts)) stop("Multipart waterbody polygons not supported.")

  waterbodies <- st_coordinates(waterbodies)

  if(ncol(waterbodies) == 4) waterbodies[ ,3] <- waterbodies[ ,4]

  near_wb <- matcher(waterbodies,
                     st_coordinates(points), search_radius)
  near_wb <- left_join(near_wb, wb_atts, by = c("L1" = "index"))
  near_wb <- left_join(data.frame(point_id = c(1:nrow(points))), near_wb, by = point_id)
  near_wb <- mutate(near_wb, nn.dists = ifelse(.data$nn.dists > search_radius,
                                               NA, .data$nn.dists))

  out <- st_drop_geometry(st_as_sf(bind_cols(select(near_wb, near_wbid = wbid,
                                                 near_wb_dist = "nn.dists"),
                                          select(points, in_wbid = wbid))))

  if(!is.null(flines)) {

    flines <- hy(flines)

    check_names(flines, c(id, wbid, topo_sort), "index_points_to_waterbodies flowlines")

    out <- mutate(out, joiner = ifelse(!is.na(.data$in_wbid),
                                       .data$in_wbid, .data$near_wbid),
                  id = seq_len(nrow(out)))

    flines <- st_drop_geometry(flines)

    out <- left_join(out, select(flines,
                                 wb_outlet_id = id,
                                 wbid, topo_sort),
                     by = c("joiner" = wbid), relationship = "many-to-many")

    out <- ungroup(filter(group_by(out, .data$id),
                          is.na(topo_sort) | topo_sort == min(topo_sort)))

    out <- select(out, -all_of(c(id, topo_sort, "joiner")))

  }

  if(rename_comid) {
    out <- rename(out, any_of(c(near_wb_COMID = "near_wbid",
                                in_wb_COMID = "in_wbid",
                                outlet_fline_COMID = "wb_outlet_id")))
  }

  out
}

rename_indexed <- function(x, matched) {
  orig_id <- names(attr(x, "orig_names")[attr(x, "orig_names") == id])
  orig_aggregate_id <- names(attr(x, "orig_names")[attr(x, "orig_names") == aggregate_id])
  new_aggregate_measure <- paste0(orig_aggregate_id, "_measure")

  rename(matched, any_of(setNames(c(id, aggregate_id, aggregate_id_measure),
                                         c(orig_id, orig_aggregate_id, new_aggregate_measure))))
}
