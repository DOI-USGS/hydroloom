#' @title Make Index ids
#' @description makes index ids for the provided hy object. These can be used
#' for graph traversal algorithms such that the row number and id are equal.
#' @inheritParams add_levelpaths
#' @param long_form logical if TRUE, return will be a long-form version of the
#' `to_list`. This form can be converted to the default list format with
#' \link{format_index_ids}. "Long" refers to the fact that ids that connect
#' to more than one `toid` will have multiple rows in the output.
#' @param mode character indicating the mode of the graph. Choose from "to",
#' "from", "both", or "none". Default is "to". Se Details for more information.
#' @details mode determines the direction of the graph. If "to", the graph will
#' be directed from the `id` to the `toid`. If "from", the graph will be
#' directed from the `toid` to the `id`. If "both", the graph will be
#' directed in both directions. If "none", the graph will be undirected.
#' @returns list containing named elements:
#' \describe{
#'   \item{to}{adjacency matrix with columns that correspond to `unqiue(x$id)`}
#'   \item{lengths}{vector indicating the number of connections from each node}
#'   \item{to_list}{a data.frame with an `id`, `indid` and a `toindid` list column.}
#' }
#' If long_form = TRUE, return will be a long form data.frame with no list column as in `to_list`.
#' NOTE: the long_form output is deprecated and will be removed in a future release.
#'
#' @name make_index_ids
#' @export
#' @examples
#'
#' x <- data.frame(id = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
#'   toid = c(2, 3, 4, 5, 0, 7, 8, 9, 4))
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

  x <- check_hy_outlets(x, fix = FALSE)

  if (!isTRUE(check_hy_graph(x))) {
    stop("found one or more pairs of features that reference eachother.
          Run check_hy_graph to identify issues.")
  }

  if (!missing(long_form)) {
    warning("long_form is deprecated and will be removed in a future release.")
  }

  vars <- c("id", "toid")
  if ("downmain" %in% names(x)) vars <- c(vars, "downmain")

  x <- distinct(select(x, all_of(vars)))

  out_val <- get_outlet_value(x)

  if (any(duplicated(x$id))) { # we have a nondedritic network

    # start table with id and row id
    out <- data.table(id = unique(x$id),
      indid = seq(1, length(unique(x$id))))

    out_rename <- copy(out)
    setnames(out_rename, old = "indid", new = "toindid")

    # merge the original table to the indid table by id then again by toid
    out <- 
      as.data.table(x)[, vars, with = FALSE] |> # only vars we need
      merge(out, by = "id", all.x = TRUE, sort = FALSE) |> # merge to get indid
      merge(out_rename, by.x = "toid", by.y = "id", all.x = TRUE, sort = FALSE) |> # merge to get toindid
        as.data.frame() |>
        as_tibble()

    out$toindid <- replace_na(out$toindid, 0)

    out <- select(out, -"toid")

  } else {
    # we have a dendritic network, so we can just use the id and toid
    out <- data.frame(id = x$id, indid = seq(1, nrow(x)))

    out$toindid <- match(x$toid, x$id, nomatch = out_val)
  }

  if (!long_form) {
    format_index_ids(out, TRUE)
  } else {
    out
  }
}

#' @title Format Index ids
#' @param g data.frame graph with `id`, `inid` and `toindid` as returned by \link{make_index_ids}
#' with `long_form`=TRUE.
#' @param return_list logical if TRUE, the returned list will include a
#' "froms_list" element containing all from ids in a list form.
#' @returns list containing an adjacency matrix and a lengths vector indicating
#' the number of connections from each node. If `return_list` is `TRUE`, return
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

  if ("downmain" %in% names(g)) {
    g <- data.frame(id = unique(g$id),
      indid = unique(g$indid),
      toindid = I(split(g$toindid, g$indid)),
      main = I(split(g$downmain, g$indid)))
  } else {
    g <- data.frame(id = unique(g$id),
      indid = unique(g$indid),
      toindid = I(split(g$toindid, g$indid)))
  }

  to_l <- lengths(g$toindid)
  max_to <- max(to_l)

  # Convert list to matrix with NA fill
  to_m <- as.matrix(sapply(g$toindid, "[", seq(max_to)))

  if ("main" %in% names(g))
    main <- as.matrix(sapply(g$main, "[", seq(max_to)))

  if (max_to == 1) {
    to_m <- matrix(to_m, nrow = 1)

    if ("main" %in% names(g))
      main <- matrix(main, nrow = 1)
  }

  # NAs should be length 0
  to_l[is.na(to_m[1, ])] <- 0

  out <- list(to = to_m, lengths = to_l)

  if ("main" %in% names(g))
    out <- c(out, list(main = main))

  if (return_list) return(c(out, list(to_list = g)))

  out

}

#' @title Convert "to" index ids to "from" index ids
#' @description given a set of index ids as retrieved from \link{make_index_ids}
#' return an adjacency matrix with pointers to identifiers that flow to the
#' row of the matrix in question.
#' @param index_ids data.frame as returned by \link{make_index_ids}
#' @param return_list logical if TRUE, the returned list will include a
#' "froms_list" element containing all from ids in a list form.
#' @param upmain data.frame containing `id` and `upmain` columns. `upmain` should
#' be a logical value indicating if the id is the upmain connection from its
#' downstream neighbors.
#' @returns list containing a "froms" matrix, "lengths" vector,
#' and optionally "froms_list" elements.
#' @export
#' @examples
#'
#' x <- data.frame(id = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
#'   toid = c(2, 3, 4, 5, 0, 7, 8, 9, 4))
#'
#' y <- make_index_ids(x)
#'
#' make_fromids(y)
#'
make_fromids <- function(index_ids, return_list = FALSE, upmain = NULL) {

  index_ids <- unnest(index_ids$to_list, "toindid")

  index_ids <- select(index_ids, -any_of("main"))

  #nolint start
  # froms <- left_join(select(index_ids, "indid"),
  #                    select(index_ids, indid = "toindid", fromindid = "indid"),
  #                    by = "indid")
  #
  # froms <- data.frame(indid = unique(froms$indid),
  #                     fromindid = I(split(froms$fromindid, froms$indid)))
  # nolint end

  # slightly faster but requires data.table
  index_ids <- as.data.table(index_ids)

  if (!is.null(upmain)) {
    index_ids <- merge(index_ids, as.data.table(upmain), by = "id", all.x = TRUE, sort = FALSE)
  }

  ids <- unique(index_ids[, c("indid", "id")])

  froms <- unique(merge(
    index_ids[, list(indid)],
    setnames(index_ids, c("toindid", "indid"), c("indid", "fromindid")),
    by = "indid", all.x = TRUE
  ))

  if (!is.null(upmain)) {
    froms <- froms[, list(fromindid = list(c(fromindid)),
      main = list(c(upmain))), by = indid]
  } else {
    froms <- froms[, list(fromindid = list(c(fromindid))), by = indid]
  }

  froms <- merge(ids, froms, by = "indid", all.x = TRUE)

  froms_l <- lengths(froms$fromindid, use.names = FALSE)
  max_from <- max(froms_l)

  # Convert list to matrix with NA fill
  froms_m <- matrix(sapply(froms$fromindid, "[", seq(max_from)),
    nrow = max_from, ncol = nrow(froms))

  main_m <- matrix(sapply(froms$main, "[", seq(max_from)),
    nrow = max_from, ncol = nrow(froms))

  # NAs should be length 0
  froms_l[is.na(froms_m[1, ])] <- 0

  out <- list(froms = froms_m, lengths = froms_l)

  if (!is.null(upmain)) {
    out <- c(out, list(main = main_m))
  }

  if (return_list) return(c(out, list(froms_list = froms)))

  out

}

fromindid <- NULL
