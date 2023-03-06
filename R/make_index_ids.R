#' make index ids
#' @description makes index ids for the provided hy object. These can be used
#' for graph traversal algorithms such that the row number and id are equal.
#' @inheritParams add_levelpaths
#' @param long_form logical if TRUE, return will be a long-form version of the
#' `to_list`. This form can be converted to the default list format with
#' \link{format_index_ids}.
#' @return list containing named elements: `to`: adjacency matrix `lengths`:
#' vector indicating the number of connections from each node, and: `to_list`:
#' a data.frame with an `id`, `indid` and a `toindid` list column. If long_form
#' = TRUE, return will be a long form data.frame with no list column as in `to_list`.
#' NOTE: the long_form output should be used with caution as indid may not
#' correspond to row number.
#' @name make_index_ids
#' @export
#' @examples
#'
#' x <- data.frame(id = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
#'               toid = c(2, 3, 4, 5, 0, 7, 8, 9, 4))
#'
#' make_index_ids(x)
#'
#' x <- hy(sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom")))
#'
#' x <- add_toids(x, return_dendritic = FALSE)
#'
#' x <- make_index_ids(x)
#'
#' names(x)
#' class(x$to)
#' class(x$lengths)
#' class(x$to_list)
#' is.list(x$to_list$toindid)
#'
make_index_ids <- function(x, long_form = FALSE) {
  UseMethod("make_index_ids")
}

#' @name make_index_ids
#' @export
make_index_ids.data.frame <- function(x, long_form = FALSE) {
  make_index_ids(hy(x), long_form)
}

#' @name make_index_ids
#' @export
make_index_ids.hy <- function(x, long_form = FALSE) {

  check_graph(x)

  x <- distinct(x)

  if(any(duplicated(x$id))) {
    out <- data.frame(id = unique(x$id),
                      indid = seq(1, length(unique(x$id))))

    out <- left_join(left_join(select(x, "id", "toid"),
                               out, by = "id"),
                     rename(out, toindid = "indid"),
                     by = c("toid" = "id"))

    out$toindid <- replace_na(out$toindid, 0)

    out <- select(out, -"toid")

  } else {
    out <- data.frame(id = x$id,
                      indid = seq(1, nrow(x)))

    out$toindid <- match(x$toid, x$id, nomatch = 0)
  }

  if(!long_form) {
    format_index_ids(out, TRUE)
  } else {
    out
  }
}

check_graph <- function(x) {
  x <- left_join(x, drop_geometry(x),
                 by = c("toid" = "id"),
                 relationship = "many-to-many")

  if(any(x$id == x$toid.y, na.rm = TRUE)) {
    stop("found one or more pairs of features that reference eachother.")
  }

  return(invisible())
}

#' format index ids
#' @param g data.frame with `id`, `inid` and `toindid` as returned by \link{make_index_ids}
#' with `long_form`=TRUE.
#' @param complete logical should the a data.frame with a list column be included
#' in the return?
#' @return list containing an adjacency matrix and a lengths vector indicating
#' the number of connections from each node. If `complete` is `TRUE` return
#' will also include a data.frame with an `indid` column and a `toindid` list
#' column.
#' @export
#' @examples
#' x <- sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom"))
#'
#' y <- add_toids(x) |>
#'   make_index_ids(long_form = TRUE) |>
#'   format_index_ids()
#'
format_index_ids <- function(g, return_list = FALSE) {

  g <- data.frame(id = unique(g$id),
                  indid = unique(g$indid),
                  toindid = I(split(g$toindid, g$indid)))

  to_l <- lengths(g$toindid)
  max_to <- max(to_l)

  # Convert list to matrix with NA fill
  to_m <- as.matrix(sapply(g$toindid, '[', seq(max_to)))

  if(max_to == 1) {
    to_m <- matrix(to_m, nrow = 1)
  }

  # NAs should be length 0
  to_l[is.na(to_m[1, ])] <- 0

  if(return_list) return(list(to = to_m, lengths = to_l,
                              to_list = g))

  return(list(to = to_m, lengths = to_l))

}
