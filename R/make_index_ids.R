#' @title Make Index ids
#' @description makes index ids for the provided hy object. These can be used
#' for graph traversal algorithms such that the row number and id are equal.
#' @inheritParams add_levelpaths
#' @param long_form logical if TRUE, return will be a long-form version of the
#' `to_list`. This form can be converted to the default list format with
#' \link{format_index_ids}.
#' @returns list containing named elements: `to`: adjacency matrix `lengths`:
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

  x <- check_hy_outlets(x, fix = FALSE)

  if(!isTRUE(check_hy_graph(x))) {
    stop("found one or more pairs of features that reference eachother.
          Run check_hy_graph to identify issues.")
  }

  x <- distinct(x)

  out_val <- get_outlet_value(x)

  if(any(duplicated(x$id))) {
    out <- data.table(id = unique(x$id),
                      indid = seq(1, length(unique(x$id))))

    out <- unique(out)

    out_rename <- copy(out)
    setnames(out_rename, old = "indid", new = "toindid")

    vars <- c("id", "toid")
    if("downmain" %in% names(x)) vars <- c(vars, "downmain")

    out <- merge(merge(as.data.table(x)[, vars, with = FALSE],
                       out, by = "id", all.x = TRUE, sort = FALSE),
                 out_rename,
                 by.x = "toid", by.y = "id", all.x = TRUE, sort = FALSE) |>
      as.data.frame() |>
      as_tibble()

    # dplyr method of the above hanges on large datasets
    # out2 <- data.frame(id = unique(x$id),
    #                   indid = seq(1, length(unique(x$id))))
    #
    # out2 <- left_join(left_join(select(x, "id", "toid"),
    #                            out2, by = "id"),
    #                  rename(out2, toindid = "indid"),
    #                  by = c("toid" = "id"))

    out$toindid <- replace_na(out$toindid, 0)

    out <- select(out, -"toid")

  } else {
    out <- data.frame(id = x$id,
                      indid = seq(1, nrow(x)))

    out$toindid <- match(x$toid, x$id, nomatch = out_val)
  }

  if(!long_form) {
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

  if("downmain" %in% names(g)) {
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
  to_m <- as.matrix(sapply(g$toindid, '[', seq(max_to)))

  if("main" %in% names(g))
    main <- as.matrix(sapply(g$main, '[', seq(max_to)))

  if(max_to == 1) {
    to_m <- matrix(to_m, nrow = 1)

    if("main" %in% names(g))
      main <- matrix(main, nrow = 1)
  }

  # NAs should be length 0
  to_l[is.na(to_m[1, ])] <- 0

  out <- list(to = to_m, lengths = to_l)

  if("main" %in% names(g))
    out <- c(out, list(main = main))

  if(return_list) return(c(out, list(to_list = g)))

  out

}
