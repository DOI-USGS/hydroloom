#' To Flownetwork
#' @description
#' converts an `hy` object into a flownetwork with "id", "toid",
#' "upmain" and "downmain attributes.
#'
#' @param x data.frame network compatible with \link{hydroloom_names}.
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
#' @seealso [hy_flownetwork], [hy_leveled], [hy_node], [add_levelpaths()]
#' @export
#' @name to_flownetwork
#' @examples
#' f <- sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom"))
#' to_flownetwork(f)
#'
to_flownetwork <- function(x, warn_dendritic = TRUE) {
  UseMethod("to_flownetwork")
}

#' @name to_flownetwork
#' @export
to_flownetwork.data.frame <- function(x, warn_dendritic = TRUE) {

  x <- hy(x, clean = TRUE)

  to_flownetwork(x, warn_dendritic)
}

#' @name to_flownetwork
#' @export
to_flownetwork.hy <- function(x, warn_dendritic = TRUE) {
  hy_classify_and_redispatch(x, "to_flownetwork", "hy_leveled",
    hy_guidance_leveled, warn_dendritic = warn_dendritic)
}

#' @name to_flownetwork
#' @export
to_flownetwork.hy_node <- function(x, warn_dendritic = TRUE) {

  if (all(c(divergence, levelpath) %in% names(x)))
    return(to_flownetwork.hy_leveled(x, warn_dendritic))

  warning("converting hy_node to non-dendritic edge list; ",
    "upmain/downmain will not be set. ",
    "Add divergence and levelpath attributes (see add_divergence(), ",
    "add_levelpaths()) to preserve main-path information.",
    call. = FALSE)

  # flownetwork carries topology only; the source hy_node remains
  # intact in the caller's environment with its full attribute set.
  nodes <- data.frame(
    id = x[[id]],
    fromnode = x[[fromnode]],
    tonode = x[[tonode]]
  )
  class(nodes) <- c("hy", class(nodes))
  nodes <- new_hy_node(nodes)

  edges <- add_toids(nodes, return_dendritic = FALSE)

  new_hy_flownetwork(data.frame(id = edges$id, toid = edges$toid))
}

#' @name to_flownetwork
#' @export
to_flownetwork.hy_topo <- function(x, warn_dendritic = TRUE) {

  if (all(c(divergence, levelpath) %in% names(x)))
    return(to_flownetwork.hy_leveled(x, warn_dendritic))

  hy_dispatch_error("to_flownetwork", "hy_leveled", x,
    "Use add_levelpaths() to add levelpath attributes.")
}

#' @name to_flownetwork
#' @export
to_flownetwork.hy_leveled <- function(x, warn_dendritic = TRUE) {

  if ("toid" %in% names(x) && warn_dendritic) {
    if (!any(duplicated(x$id)))
      warning("toid was provided and appears to be dendritic.")
  }

  if (fromnode %in% names(x) && !toid %in% names(x))
    x <- add_toids(as_hy_node(x), return_dendritic = FALSE)

  if (!divergence %in% names(x)) stop("must provide a divergence attribute")

  if (!levelpath %in% names(x)) stop("must provide a levelpath attribute")

  x <- select(st_drop_geometry(x), all_of(c(id, toid, divergence, levelpath)))

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

  # to_flownetwork() is the user-facing producer: drop hy round-trip
  # metadata so the result is a topology-only junction table.
  attr(x, "orig_names") <- NULL
  attr(x, "dendritic")  <- NULL

  new_hy_flownetwork(x)
}
