#' all paths depth first
#' @description given a starting node, return all reachable paths
#' @param x hy data.frame containing network topology
#' @param starts vector with ids from x to start at. If not supplied will start
#' from headwaters.
#' @param direction character only "down" supported so far.
#' @examples
#'
#' x <- hy(sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom")))
#'
#' x <- add_toids(x, return_dendritic = FALSE)


all_paths_dfs <- function(x, starts = NULL, direction = "down") {

  if(direction != "down") {
    stop("up not supported yet.")
    # will need get_fromids fomr nhdplusTools
  }

  g <- make_index_ids(x)

  g <- hydroloom:::format_nonden_toid(g)

  # TODO: test format nondend and friends!
  # note NA vs 0 and such

  # nrow to reuse
  n <- nrow(x)

  if(is.null(starts)) {
    starts <- which(!g$indid %in% g$toindid)
  }

  # Some vectors to track results
  to_visit <- out <- rep(0, n)

  set <- out
  out_list <- rep(list(list()), length(starts))


  # output order tracker
  o <- 1
  set_id <- 1

  for(s in starts) {

    # Set up the starting node
    node <- s

    # within set node tracker
    n <- 1
    # v is a pointer into the to_visit vector
    v <- 1

    while(v > 0) {

      # track the order that nodes were visited
      out[node] <- o
      # increment to the next node
      o <- o + 1

      set[n] <- node
      n <- n + 1

      # does nothing if froms_l[node] == 0

      for(from in seq_len(froms$lengths[node])) {
        if(node <= ncol(froms$froms) &&
           !is.na(next_node <- froms$froms[from, node])) {
          # Add the next node to visit to the tracking vector
          to_visit[v] <- next_node
          v <- v + 1
        }}

      # go to the last element added in to_visit
      v <- v - 1
      node <- to_visit[v]

    }

    if(split) {
      out_list[[set_id]] <- x[set[1:(n - 1)], 1]
      set_id <- set_id + 1
    }
  }


}
