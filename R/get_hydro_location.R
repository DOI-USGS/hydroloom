#' Get Hydro Location
#' @description given a flowline index, returns the hydrologic location (point)
#' along the specific linear element referenced by the index.
#' @param indexes data.frame as output from \link{index_points_to_lines}.
#' @param flowpath data.frame with three columns: COMID, FromMeas, and ToMeas
#' as well as geometry.
#' @export
#' @examples
#' source(system.file("extdata", "sample_flines.R", package = "nhdplusTools"))
#'
#' indexes <- index_points_to_lines(sample_flines,
#'                    sf::st_sfc(sf::st_sfc(list(sf::st_point(c(-76.86934, 39.49328)),
#'                                               sf::st_point(c(-76.91711, 39.40884)),
#'                                               sf::st_point(c(-76.88081, 39.36354))),
#'                               crs = 4326)))
#'
#' get_hydro_location(indexes, sample_flines)
#'
get_hydro_location <- function(indexes, flowpath) {
  flowpath <- hy(flowpath)

  check_names(flowpath, c(id, aggregate_id_to_measure,
                          aggregate_id_from_measure),
              "get_hydro_location flowpath")

  if("id" %in% names(indexes)) indexes <- rename(indexes, point_id = "id")

  indexes <- align_names(indexes)

  flowpath <- rename(flowpath, all_of(c(frommeas = aggregate_id_from_measure,
                                        tomeas = aggregate_id_to_measure)))

  in_list <- Map(list,
                 indexes$aggregate_id_measure,
                 split(flowpath[match(indexes$id, flowpath$id), ],
                       seq(1, nrow(indexes))))

  do.call(c, lapply(in_list, get_hydro_location_single))

}

get_hydro_location_single <- function(x) {

  coords <- st_coordinates(x[[2]]) |>
    add_index() |>
    add_len()

  # First rescale 0-100 measures passed in.
  m <- rescale_measures(x[[1]], x[[2]]$frommeas, x[[2]]$tomeas)

  nus <- nrow(coords) - sum(coords$id_measure <= m)

  if(nus == 0) {
    nus <- 1
  }

  nds <- ifelse(nus < nrow(coords), nus + 1, nus)

  if(nds == nus) {
    return(
      st_sfc(st_point(c(coords$X[nds], coords$Y[nds])),
                 crs = st_crs(x[[2]]))
    )}

  new_m <- rescale_measures(m, coords$id_measure[nds], coords$id_measure[nus])

  new <- interp_meas(new_m, coords$X[nds], coords$Y[nds], coords$X[nus], coords$Y[nus])

  st_sfc(st_point(c(new[[1]], new[[2]])), crs = st_crs(x[[2]]))
}
