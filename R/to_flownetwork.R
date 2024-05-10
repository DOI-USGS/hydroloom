#' to flownetwork
#' @description
#' converts an `hy` object into a flownetwork with "id", "toid",
#' "upmain" and "downmain attributes.
#'
#' @inheritParams add_levelpaths
#' @details
#'
#' Required attributes:
#'
#' `id` and `toid` or `fromnode` and `tonode`
#'
#' `divergence`
#' an attribute containing 0, 1, or 2 where 0 indicates there is only one
#' downstream connection, 1 is the main connection downstream
#' of a diversion and 2 is secondary connection downstream of a diversion.
#'
#' `stream_level` or `levelpath`,
#' integer attributes which will have one and only one matching value upstream
#' at a confluence.
#'
#' @return data.frame "id", "toid", "upmain" and "downmain attributes. A check
#' is run to ensure upmain and downmain are valid with one and only one upmain
#' and one and only one downmain from any given network element.
#'
#' @export
#' @examples
#' f <- sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom"))
#' to_flownetwork(f)
#'
to_flownetwork <- function(x) {

  x <- hy(x, clean = TRUE)

  if("fromnode" %in% names(x)) x <- add_toids(x, return_dendritic = FALSE)

  if(!"divergence" %in% names(x)) stop("must provide a divergence attribute")

  if("stream_level" %in% names(x)) {
    level <- "stream_level"
  } else if("levelpath" %in% names(x)) {
    level <- "levelpath"
  } else {
    stop("must provide either stream_level of levelpath attribute")
  }

  x <- select(x, all_of(c(id, toid, divergence, level)))

  x <- x |>
    left_join(distinct(select(x, toid = id, toid_divergence = divergence)),
                     by = "toid") |>
    left_join(distinct(select(x, all_of(c(toid = id, toid_level = level)))),
                     by = "toid") |>
    mutate(downmain = .data$toid_divergence != 2,
           upmain = !!level == .data$toid_level) |>
    select(id, toid, upmain, downmain) |>
    distinct()

  dm <- x[x$downmain,]
  um <- x[x$upmain, ]

  if(any(duplicated(dm$id))) stop("duplicated down mains?")
  if(any(duplicated(um$toid))) stop("duplicated up mains?")

  x
}
