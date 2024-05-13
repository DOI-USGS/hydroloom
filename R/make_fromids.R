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
#'               toid = c(2, 3, 4, 5, 0, 7, 8, 9, 4))
#'
#' y <- make_index_ids(x)
#'
#' make_fromids(y)
#'
make_fromids <- function(index_ids, return_list = FALSE, upmain = NULL) {

  index_ids <- unnest(index_ids$to_list, "toindid")

  index_ids <- select(index_ids, -any_of("main"))

  # froms <- left_join(select(index_ids, "indid"),
  #                    select(index_ids, indid = "toindid", fromindid = "indid"),
  #                    by = "indid")
  #
  # froms <- data.frame(indid = unique(froms$indid),
  #                     fromindid = I(split(froms$fromindid, froms$indid)))

  # slightly faster but requires data.table
  index_ids <- as.data.table(index_ids)

  if(!is.null(upmain)) {
    index_ids <- merge(index_ids, as.data.table(upmain), by = "id", all.x = TRUE, sort = FALSE)
  }

  ids <- unique(index_ids[,c("indid", "id")])

  froms <- unique(merge(
    index_ids[,list(indid)],
    setnames(index_ids, c("toindid", "indid"), c("indid", "fromindid")),
    by = "indid", all.x = TRUE
  ))

  if(!is.null(upmain)) {
    froms <- froms[,list(fromindid = list(c(fromindid)),
                         main = list(c(upmain))), by = indid]
  } else {
    froms <- froms[,list(fromindid = list(c(fromindid))), by = indid]
  }

  froms <- merge(ids, froms, by = "indid", all.x = TRUE)

  froms_l <- lengths(froms$fromindid, use.names = FALSE)
  max_from <- max(froms_l)

  # Convert list to matrix with NA fill
  froms_m <- matrix(sapply(froms$fromindid, '[', seq(max_from)),
                    nrow = max_from, ncol = nrow(froms))

  main_m <- matrix(sapply(froms$main, '[', seq(max_from)),
                   nrow = max_from, ncol = nrow(froms))

  # NAs should be length 0
  froms_l[is.na(froms_m[1, ])] <- 0

  out <- list(froms = froms_m, lengths = froms_l)

  if(!is.null(upmain)) {
    out <- c(out, list(main = main_m))
  }

  if(return_list) return(c(out, list(froms_list = froms)))

  out

}

fromindid <- NULL
