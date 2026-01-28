#' Sort Network
#' @description given a network with an id and and toid, returns a sorted
#' and potentially split set of output.
#'
#' Can also be used as a very fast implementation of upstream
#' with tributaries navigation. The full network from each
#' outlet is returned in sorted order.
#'
#' If a network includes diversions, all flowlines downstream of
#' the diversion are visited prior to continuing upstream. See
#' note on the `outlets` parameter for implications of this
#' implementation detail.
#'
#' @export
#' @inheritParams add_levelpaths
#' @param split logical if TRUE, the result will be split into
#' independent networks identified by the id of their outlet. The
#' outlet id of each independent network is added as a "terminalid"
#' attribute.
#' @param outlets same as id in x. if specified, only the network
#' emanating from these outlets will be considered and returned.
#' NOTE: If outlets does not include all outlets from a given
#' network containing diversions, a partial network may be returned.
#' @returns data.frame containing a topologically sorted version
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

  x <- hy(x)

  x <- sort_network(x, split, outlets)

  hy_reverse(x)

}

#' @name sort_network
#' @export
#'
sort_network.hy <- function(x, split = FALSE, outlets = NULL) {

  hy_g <- get_hyg(x, add = TRUE, id = id)

  x <- select(st_drop_geometry(x), id, toid, everything())

  # index for fast traversal
  index_ids <- make_index_ids(x)

  froms <- make_fromids(index_ids)

  if(!is.null(outlets)) {
    starts <- which(index_ids$to_list$id %in% outlets)
  } else {
    # All the start nodes
    if(any(x$toid != get_outlet_value(x) & !x$toid %in% x$id)) {
      warning("no outlet found -- will start from outlets that go no where.")
      starts <- which(index_ids$to_list$id %in% x$id[!x$toid %in% x$id])
    } else {
      starts <- which(index_ids$to_list$id %in% x$id[x$toid == get_outlet_value(x)])
    }
  }
  # Some vectors to track results
  to_visit <- out <- rep(0, length(index_ids$to_list$id))

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

            v <- v + 1
          } else {
            # we don't want to visit an upstream neighbor until all its
            # downstream neighbors have been visited. Ready is initialized
            # to the length of downstream neighbors and provides a check.
            ready[next_node] <- ready[next_node] - 1
          }

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
      out_list[[set_id]] <- index_ids$to_list$id[set[1:(n - 1)]]
      set_id <- set_id + 1
    }
  }

  if(split) names(out_list) <- index_ids$to_list$id[starts]

  ### rewrites x into the correct order. ###
  id_order <- unique(x$id)[which(out != 0)]
  out <- out[out != 0]

  if(split && o - 1 != length(id_order)) stop("Are two or more outlets within the same network?")

  if(is.null(outlets) && length(unique(x$id)) != length(out)) warning("some features missed in sort. Are there loops in the network?")

  x <- filter(x, .data$id %in% id_order) |>
    left_join(tibble(id = id_order, sorter = out), by = "id") |>
    arrange(desc(.data$sorter)) |>
    select(-"sorter")

  if(split) {

    # this is only two columns
    ids <- as(names(out_list), class(pull(x[1, 1])))

    out_list <- data.frame(ids = ids) |>
      mutate(set = out_list) |>
      unnest("set")

    names(out_list) <- c(terminal_id, id)

    ### adds grouping terminalID to x ###
    x <- left_join(x, out_list, by = names(x)[1])

  }

  x <- put_hyg(x, hy_g)

  x
}

#' Add topo_sort
#' @description calls \link{sort_network} without support for splitting the network
#' and adds a `nrow:1` topo_sort attribute.
#' @inheritParams sort_network
#' @returns data.frame containing a topo_sort attribute.
#' @name add_topo_sort
#' @export
add_topo_sort <- function(x, outlets = NULL) {
  UseMethod("add_topo_sort")
}

#' @name add_topo_sort
#' @export
#'
add_topo_sort.data.frame <- function(x, outlets = NULL) {

  x <- hy(x)

  x <- add_topo_sort(x, outlets)

  hy_reverse(x)

}

#' @name add_topo_sort
#' @export
#'
add_topo_sort.hy <- function(x, outlets = NULL) {

  out <- sort_network(x, outlets = outlets)

  ids <- unique(out$id)

  dplyr::left_join(out,
                   data.frame(id = ids,
                              topo_sort = seq(from = length(ids), to = 1, by = -1)),
                   by = "id")

}
