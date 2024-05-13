#' @title Add Downstream IDs
#' @description Given input with fromnode and tonode attributes,
#' will return the input with a toid attribute that is the result of joining
#' tonode and fromnode attributes.
#' @inheritParams add_levelpaths
#' @param return_dendritic logical remove non dendritic paths if TRUE. Requires
#' a "divergence" flag where 1 is main and 2 is secondary.
#' @returns hy object with toid attribute
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

  if("toid" %in% names(x)) stop("network already contains a toid attribute")

  # joiner_fun <- function(x) {
  #   select(
  #     left_join(select(st_drop_geometry(x), "id", "tonode"),
  #               select(st_drop_geometry(x), toid = "id", "fromnode"),
  #               by = c("tonode" = "fromnode")), -"tonode")
  # }

  # slightly faster data.table
  joiner_fun <- function(x) {
    as.data.frame(
      data.table(toid = x$id,
                 node = x$fromnode)[data.table(id = x$id,
                                               node = x$tonode),
                                    on = 'node']
    )[, c("id", "toid")]
  }

  if(return_dendritic) {
    if(!"divergence" %in% names(x)) {
      stop("To remove non dendritic paths, a divergence attribute is required.")
    }

    x <- mutate(x,
                orig_fromnode = fromnode,
                fromnode = ifelse(.data$divergence == 2, NA, fromnode))

  }

  d <- is.na(x$tonode)

  # avoid cartesian join on disconnected lines!
  disconnected <- filter(x, d)

  out_val <- get_outlet_value(x)

  disconnected$toid <- rep(out_val, nrow(disconnected))

  x <- filter(x, !d) |>
    left_join(joiner_fun(filter(x, !d)), by = c("id")) |>
    mutate(toid = replace_na(toid, out_val)) |>
    bind_rows(disconnected)

  sf_t <- inherits(x, "sf")

  as.data.frame(
    x <- x[ , c("id", "toid",
                names(x)[!names(x) %in% c("id", "toid")])]
  )

  if(sf_t)
    x <- st_sf(x)

  if(return_dendritic) {
    x <- select(x, -fromnode)
    x <- rename(x, fromnode = "orig_fromnode")
  }

  x

}
