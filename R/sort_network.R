#' Sort Network
#' @description given a network with an id and and toid, returns a sorted
#' and potentially split set of output.
#'
#' Can also be used as a very fast implementation of upstream
#' with tributaries navigation. The full network from each
#' outlet is returned in sorted order.
#'
#' @export
#' @inheritParams add_levelpaths
#' @param split logical if TRUE, the result will be split into
#' independent networks identified by the id of their outlet. The
#' outlet id of each independent network is added as a "terminalID"
#' attribute.
#' @param outlets same as id in x. if specified, only the network
#' emanating from these outlets will be considered and returned.
#' @return data.frame containing a topologically sorted version
#' of the requested network and optionally a terminal id.
#' @name sort_network
#' @examples
#' x <- sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom"))
#'
#' g <- add_toids(x)
#'
#' head(g <- sort_network(g))
#'
#' g$topo_sort <- nrow(g):1
#'
#' plot(g['topo_sort'])
#'
#' g <- add_toids(x, return_dendritic = FALSE)
#'
#' g <- sort_network(g)
#'
#' g$topo_sort <- nrow(g):1
#'
#' plot(g['topo_sort'])
#'
sort_network <- function(x, split = FALSE, outlets = NULL) {
  UseMethod("sort_network")
}

#' @name sort_network
#' @export
#'
sort_network.data.frame <- function(x, split = FALSE, outlets = NULL) {
  class_x <- class(x)

  x <- hy(x)

  x <- sort_network(x, split, outlets)

  hy_reverse(x)

}

sort_network.hy <- function(x, split = FALSE, outlets = NULL) {
  hy_g <- get_hyg(x, add = TRUE, id = id)

  x <- select(drop_geometry(x), id, toid, everything())

  # index for fast traversal
  index_ids <- make_index_ids(x)

  froms <- make_fromids(index_ids)

  if(!is.null(outlets)) {
    starts <- which(index_ids$to_list$id %in% outlets)
  } else {
    # All the start nodes
    starts <- which(index_ids$to_list$id %in% x$id[x$toid == 0])
  }
  # Some vectors to track results
  to_visit <- out <- rep(0, length(index_ids$to_list$id))

  visited <- rep(FALSE, length(index_ids$to_list$id))

  # Use to track if a node is ready to be visited.
  # will subtract from this and not visit the upstream until ready element = 1
  ready <- index_ids$lengths

  if(split) {
    set <- out
    out_list <- rep(list(list()), length(starts))
  }

  # output order tracker
  o <- 1
  set_id <- 1

  for(s in starts) {

    # Set up the starting node
    node <- s

    # within set node tracker for split = TRUE
    n <- 1
    # v is a pointer into the to_visit vector
    v <- 1

    trk <- 1

    while(v > 0) {

      # track the order that nodes were visited
      out[node] <- o
      # increment to the next node
      o <- o + 1

      if(split) {
        set[n] <- node
        n <- n + 1
      }

      # loop over upstream catchments
      # does nothing if froms_l[node] == 0

      for(from in seq_len(froms$lengths[node])) {

        # grab the next upstream node
        next_node <- froms$froms[from, node]

        # check if we have a node to visit
        # not needed? was in the if below node <= ncol(froms$froms) &&
        if(!is.na(next_node)) {

          if(ready[next_node] == 1) {
            # Add the next node to visit to the tracking vector
            to_visit[v] <- next_node
            # mark as visited
            visited[next_node] <- TRUE

            v <- v + 1
          } else {
            # we don't want to visit an upstream neighbor until all its
            # downstream neighbors have been visited. Ready is initialized
            # to the length of downstream neighbors and provides a check.
            ready[next_node] <- ready[next_node] - 1
            o <- o - 1
          }

          # mark it as visited so we don't come back
          # not needed?
          # froms$froms[from, node] <- NA
        }}

      # go to the last element added in to_visit
      v <- v - 1
      node <- to_visit[v]

      trk <- trk + 1

      if(trk > length(index_ids$to_list$id) * 2) {
        stop("runaway while loop, something wrong with the network?")
      }

    }

    if(split) {
      out_list[[set_id]] <- pull(x[set[1:(n - 1)], 1])
      set_id <- set_id + 1
    }
  }

  if(split) names(out_list) <- pull(x[starts, 1])

  ### rewrites x into the correct order. ###
  id_order <- unique(x$id)[which(out != 0)]
  out <- out[out != 0]

  if(split & o - 1 != length(id_order)) stop("Are two or more outlets within the same network?")

  x <- filter(x, .data$id %in% id_order) |>
    left_join(tibble(id = id_order, sorter = out), by = "id") |>
    arrange(desc(.data$sorter)) |>
    select(-"sorter")

  if(split) {

    # this is only two columns
    ids <- methods::as(names(out_list), class(pull(x[1, 1])))

    out_list <- data.frame(ids = ids) |>
      mutate(set = out_list) |>
      unnest("set")

    names(out_list) <- c(terminal_id, id)

    ### adds grouping terminalID to x ###
    x <- left_join(x, out_list, by = names(x)[1])

  }

  x <- put_hyg(x, hy_g)

  return(x)
}
