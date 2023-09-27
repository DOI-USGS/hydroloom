#' @title Disambiguate Flowline Indexes
#' @description Given a set of flowline indexes and numeric or ascii criteria,
#' return closest match. If numeric criteria are used, the minimum difference
#' in the numeric attribute is used for disambiguation. If ascii criteria are used,
#' the \link[utils]{adist} function is used with the following algorithm:
#' `1 - adist_score / max_string_length`. Comparisons ignore case.
#' @param indexes data.frame as output from \link{index_points_to_lines} with more than
#' one hydrologic location per indexed point.
#' @param flowpath data.frame with two columns. The first should join to the id
#' field of the indexes and the second should be the numeric or ascii metric such as drainage
#' area or Name. Names of this data.frame are not used.
#' @param hydro_location data.frame with two columns. The first should join to the
#' id field of the indexes and the second should be the numeric or ascii metric such as drainage
#' area or GNIS Name. Names of this data.frame are not used.
#' @returns data.frame indexes deduplicated according to the minimum difference
#' between the values in the metric columns. If two or more result in the same "minimum"
#' value, duplicates will be returned.
#' @export
#' @examples
#' source(system.file("extdata", "sample_flines.R", package = "nhdplusTools"))
#'
#' hydro_location <- sf::st_sf(id = c(1, 2, 3),
#'                             geom = sf::st_sfc(list(sf::st_point(c(-76.86934, 39.49328)),
#'                                                    sf::st_point(c(-76.91711, 39.40884)),
#'                                                    sf::st_point(c(-76.88081, 39.36354))),
#'                                               crs = 4326),
#'                             totda = c(23.6, 7.3, 427.9),
#'                             nameid = c("Patapsco", "", "Falls Run River"))
#'
#' indexes <- index_points_to_lines(sample_flines,
#'                                  hydro_location,
#'                                  search_radius = units::set_units(0.2, "degrees"),
#'                                  max_matches = 10)
#'
#' disambiguate_indexes(indexes,
#'                      dplyr::select(sample_flines, COMID, TotDASqKM),
#'                      dplyr::select(hydro_location, id, totda))
#'
#' result <- disambiguate_indexes(indexes,
#'                                dplyr::select(sample_flines, COMID, GNIS_NAME),
#'                                dplyr::select(hydro_location, id, nameid))
#'
#' result[result$point_id == 1, ]
#'
#' result[result$point_id == 2, ]
#'
#' result[result$point_id == 3, ]
#'
disambiguate_indexes <- function(indexes, flowpath, hydro_location) {
  orig_index_names <- names(indexes)

  indexes <- align_names(indexes)

  renamer <- setNames(names(indexes), orig_index_names)

  call_rename <- FALSE
  if(!is.hy(flowpath, silent = TRUE)) call_rename <- TRUE

  flowpath <- hy(flowpath)

  check_names(indexes, c(point_id, id, aggregate_id, aggregate_id_measure, offset),
              "disambiguage_indexes flowpath")

  flowpath <- st_drop_geometry(flowpath)
  hydro_location <- st_drop_geometry(hydro_location)

  if(ncol(flowpath) != 2 | ncol(hydro_location) != 2) {
    stop("flowpath and hydrolocation must be two-column data.frames")
  }

  names(flowpath) <- c(id, "metric_fp")

  names(hydro_location) <- c(point_id, "metric_hl")

  if(is.numeric(flowpath$metric_fp) & is.numeric(hydro_location$metric_hl)) {

    indexes <- indexes |>
      left_join(flowpath, by = id) |>
      left_join(hydro_location, by = point_id) |>
      mutate(metric_diff = abs(.data$metric_fp - .data$metric_hl)) |>
      group_by(.data$point_id) |>
      filter(.data$metric_diff == min(.data$metric_diff)) |>
      ungroup() |>
      select(-"metric_hl", -"metric_fp", -"metric_diff")

  } else if(is.character(flowpath$metric_fp) & is.character(hydro_location$metric_hl)) {

    indexes <- indexes |>
      left_join(flowpath, by = id) |>
      left_join(hydro_location, by = point_id) |>
      mutate(metric_diff = sapply(mapply(c, .data$metric_fp, .data$metric_hl,
                                         USE.NAMES = FALSE, SIMPLIFY = FALSE),
                                  string_score)) |>
      group_by(.data$point_id) |>
      filter(.data$metric_diff == max(.data$metric_diff)) |>
      ungroup() |>
      select(-"metric_hl", -"metric_fp", -"metric_diff")

  } else  stop("flowpath and hydrolocation metrics must both be numeric or character")

  if(call_rename) {
    rename(indexes, any_of(renamer))
  } else {
    indexes
  }

}

string_score <- function(x) {
  raw_score <- as.numeric(adist(x[[1]], x[[2]], ignore.case = TRUE))

  (1 - (raw_score) / max(c(nchar(x[[1]]), nchar(x[[2]]))))
}
