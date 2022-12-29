#' Get Sorted Network
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
#' @param outlets same as id in x; if specified only the network
#' emanating from these outlets will be considered and returned.
#' @return data.frame containing a topologically sorted version
#' of the requested network and optionally a terminal id.
#' @name sort_network
#' @examples
#' g <- sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom"))
#' g <- add_toids(g)
#' head(g <- sort_network(g))
#' g$topo_sort <- 1:nrow(g)
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

  x <- drop_geometry(x)

  # nrow to reuse
  n_row <- nrow(x)

  x <- select(x, id, toid, everything())

  # index for fast traversal
  index_ids <- make_index_ids(x)

  if(!is.null(outlets)) {
    starts <- which(unique(x$id) %in% outlets)
  } else {
    # All the start nodes
    starts <- which(unique(x$id) %in% x$id[x$toid == 0])
  }

  froms <- make_fromids(index_ids)

  # Some vectors to track results
  to_visit <- out <- rep(0, n_row)

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

      trk <- trk + 1

      if(trk > n_row * 2) {
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
  if(o - 1 != nrow(x)) {
    x <- x[which(out != 0), ]
    out <- out[out != 0]
  }

  if(split & o - 1 != nrow(x)) stop("Are two or more outlets within the same network?")

  x <- x[rev(order(out)), ]

  if(split) {

    # this is only two columns
    ids <- methods::as(names(out_list), class(pull(x[1, 1])))

    out_list <- data.frame(ids = ids) %>%
      mutate(set = out_list) %>%
      tidyr::unnest_longer("set")

    names(out_list) <- c(terminal_id, id)

    ### adds grouping terminalID to x ###
    x <- left_join(x, out_list, by = names(x)[1])

  }

  x <- put_hyg(x, hy_g)

  return(x)
}
