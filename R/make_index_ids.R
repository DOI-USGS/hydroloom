#' make index ids
#' @description makes index ids for the provided hy object. These can be used
#' for graph traversal algorithms such that the row number and id are equal.
#' @inheritParams add_levelpaths
#' @param format logical if TRUE, return will be a list containing an adjacency
#' matrix and a lengths vector indicating the number of connections from each node.
#' @param complete logical if TRUE return will also include a data.frame with an
#' `indid` column and a `toindid` list column. Has no affect if `format` is FALSE.
#' @return data.frame containing `indid` and `toindid` columns suitable for use
#' in fast graph traversal. If x is non-dendritic (`indid`:`toindid` is 1:many),
#' an adjacency matrix transformation is necessary to get `indid` to correspond
#' to rows. Set `format=TRUE` to perform the transformation automatically.
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
#' x <- add_toids(x)
#'
#' make_index_ids(x)
#'
make_index_ids <- function(x, format = FALSE, complete = FALSE) {
  UseMethod("make_index_ids")
}

#' @name make_index_ids
#' @export
make_index_ids.data.frame <- function(x, format = FALSE, complete = FALSE) {
  make_index_ids(hy(x), format, complete)
}

#' @name make_index_ids
#' @export
make_index_ids.hy <- function(x, format = FALSE, complete = FALSE) {

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

    out <- select(out, -"id", -"toid")

  } else {
    out <- data.frame(indid = seq(1, nrow(x)))

    out$toindid <- match(x$toid, x$id, nomatch = 0)
  }

  if(format) {
    format_nonden_toid(out, return_list = complete)
  } else {
    out
  }

}

check_graph <- function(x) {
  x <- left_join(x, drop_geometry(x),
                 by = c("toid" = "id"))

  if(any(x$id == x$toid.y, na.rm = TRUE)) {
    stop("found one or more pairs of features that reference eachother.")
  }

  return(invisible())
}
