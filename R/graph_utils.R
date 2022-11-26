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
