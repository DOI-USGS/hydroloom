#' make index ids
#' @description makes index ids for the provided hy object. These can be used
#' for graph traversal algorithms such that the row number and id are equal.
#' @param x hy data.frame
#' @param format logical if TRUE, return will be a list containing an adjacency
#' matrix and a lengths vector indicating the number of connections from each node.
#' @param complete logical if TRUE return will also include a data.frame with an
#' `indid` column and a `toindid` list column. Has no affect if `format` is FALSE.
#' @return data.frame containing `indid` and `toindid` columns suitable for use
#' in fast graph traversal. If x is non-dendritic (`indid`:`toindid` is 1:many),
#' an adjacency matrix transformation is necessary to get `indid` to correspond
#' to rows. Set `matrix=TRUE` to perform the transformation automatically.
#' @importFrom dplyr rename left_join
#' @export
#' @examples
#' x <- hy(sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom")))
#'
#' x <- add_toids(x)
#'
#' index_ids <- make_index_ids(x)
make_index_ids <- function(x, format = FALSE, complete = FALSE) {

  if(any(duplicated(x$id))) {
    out <- data.frame(id = unique(x$id),
                      indid = seq(1, length(unique(x$id))))

    out <- left_join(left_join(select(x, "id", "toid"),
                               out, by = "id"),
                     rename(out, toindid = "indid"),
                     by = c("toid" = "id"))

    out$toindid <- tidyr::replace_na(out$toindid, 0)

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

make_fromids <- function(index_ids, return_list = FALSE) {

  froms <- left_join(select(index_ids, "indid"),
                     select(index_ids, indid = "toindid", fromindid = "indid"),
                     by = "indid")

  froms <- data.frame(indid = unique(froms$indid),
                      fromindid = I(split(froms$fromindid, froms$indid)))

  # slightly faster but requires data.table
  # index_ids <- as.data.table(index_ids)
  #
  # froms <- merge(
  #   index_ids[,list(indid)],
  #   data.table::setnames(index_ids, c("toindid", "indid"), c("indid", "fromindid")),
  #   by = "indid", all.x = TRUE
  # )
  #
  # froms <- froms[,list(froms = list(c(fromindid))), by = indid]


  froms_l <- lengths(froms$fromindid)
  max_from <- max(froms_l)

  # Convert list to matrix with NA fill
  froms_m <- as.matrix(sapply(froms$fromindid, '[', seq(max_from)))

  # NAs should be length 0
  froms_l[is.na(froms_m[1, ])] <- 0

  if(return_list) return(list(froms = froms_m, lengths = froms_l,
                              froms_list = froms))

  return(list(froms = froms_m, lengths = froms_l))

}

format_nonden_toid <- function(g, return_list = FALSE) {

  g <- data.frame(indid = unique(g$indid),
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
#' @export
#' @examples
#' x <- hy(sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom")))
#'
#' x <- add_toids(x)
#'
add_toids <- function(x, return_dendritic = TRUE) {

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
