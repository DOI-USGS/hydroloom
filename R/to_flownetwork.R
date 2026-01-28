#' to flownetwork
#' @description
#' converts an `hy` object into a flownetwork with "id", "toid",
#' "upmain" and "downmain attributes.
#'
#' @inheritParams add_levelpaths
#' @param warn_dendritic logical if TRUE and a dendritic `toid` attribute is
#' provided, a warning will be emitted as toid is expected to be non-dendritic
#' for any `downmain` to be `FALSE`.
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
#' `levelpath`,
#' integer attribute which will have one and only one matching value upstream
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
to_flownetwork <- function(x, warn_dendritic = TRUE) {

  x <- hy(x, clean = TRUE)

  if ("toid" %in% names(x) && warn_dendritic) {
    if (!any(duplicated(x$id)))
      warning("toid was provided and appears to be dendritic.")
  }

  if (fromnode %in% names(x) && !toid %in% names(x))
    x <- add_toids(x, return_dendritic = FALSE)

  if (!divergence %in% names(x)) stop("must provide a divergence attribute")

  if (!levelpath %in% names(x)) stop("must provide a levelpath attribute")

  x <- select(x, all_of(c(id, toid, divergence, levelpath)))

  x <- x |>
    left_join(distinct(select(x, toid = id, toid_divergence = divergence)),
      by = "toid") |>
    left_join(distinct(select(x, all_of(c(toid = id, toid_level = levelpath)))),
      by = "toid") |>
    mutate(downmain = is.na(.data$toid_divergence) | .data$toid_divergence != 2) |>
    mutate(upmain = !is.na(.data$toid_level) & .data$toid_level == levelpath) |> # friggin dplyr syntax
    select(id, toid, upmain, downmain) |>
    distinct()

  dm <- x[x$downmain, ]
  um <- x[x$upmain, ]

  if (any(duplicated(dm$id))) stop("duplicated down mains?")
  if (any(duplicated(um$toid))) stop("duplicated up mains?")

  x
}
