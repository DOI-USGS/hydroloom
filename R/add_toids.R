#' add toids
#' @description Given an hy object with fromnode and tonode attributes,
#' will return a toid attribute that is the result of joining
#' tonode and fromnode attributes.
#' @param x hy object without a toid attribute
#' @param return_dendritic logical remove non dendritic paths if TRUE. Requires
#' a "divergence" flag where 1 is main and 2 is secondary.
#' @return hy object with toid attribute
#' @importFrom dplyr left_join select filter bind_rows
#' @importFrom tidyr replace_na
#' @importFrom sf st_sf
#' @name add_toids
#' @export
#' @examples
#' g <- sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom"))
#'
#' x <- add_toids(hy(g))
#'
#' y <- add_toids(g)
#'
#' names(g)[1:4]
#'
#' names(x)[1:4]
#'
#' names(y)[1:4]
#'
add_toids <- function(x, return_dendritic = TRUE) {
  UseMethod("add_toids")
}

#' @name add_toids
#' @export
add_toids.data.frame <- function(x, return_dendritic = TRUE) {
  x <- hy(x)

  orig_names <- attr(x, "orig_names")

  x <- add_toids(x, return_dendritic)

  attr(x, "orig_names") <- orig_names
  class(x) <- c("hy", class(x))

  hy_reverse(x)
}

#' @name add_toids
#' @export
add_toids.hy <- function(x, return_dendritic = TRUE) {

  joiner_fun <- function(x) {
    select(
      left_join(select(drop_geometry(x), "id", "tonode"),
                select(drop_geometry(x), toid = "id", "fromnode"),
                by = c("tonode" = "fromnode")), -"tonode")
  }

  # slightly faster data.table
  # joiner_fun <- function(x) {
  #   as.data.frame(
  #     data.table(toid = x$id,
  #                node = x$fromnode)[data.table(id = x$id,
  #                                              node = x$tonode),
  #                                   on = 'node']
  #   )[, c("id", "toid")]
  # }

  if(return_dendritic) {
    if(!"divergence" %in% names(x)) {
      stop("To remove non dendritic paths, a divergence attribute is required.")
    }

    x$fromnode[which(x$divergence == 2)] <- NA

  }

  d <- is.na(x$tonode)

  # avoid cartesian join on disconnected lines!
  disconnected <- filter(x, d)

  disconnected$toid <- rep(0, nrow(disconnected))

  x <- filter(x, !d)

  x <- left_join(x, joiner_fun(x), by = c("id"))

  x$toid <- replace_na(x$toid, 0)

  x <- bind_rows(x, disconnected)

  sf_t <- inherits(x, "sf")

  as.data.frame(
    x <- x[ , c("id", "toid",
                names(x)[!names(x) %in% c("id", "toid")])]
  )

  if(sf_t)
    x <- st_sf(x)

  x

}
