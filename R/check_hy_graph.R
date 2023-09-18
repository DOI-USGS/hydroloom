#' Check hy Graph
#' @description check that a id toid graph doesn't contain localized loops.
#' @inheritParams add_levelpaths
#' @param loop_check logical if TRUE, the entire network is walked from
#' top to bottom searching for loops. This loop detection algorithm visits
#' a node in the network only once all its upstream neighbors have been
#' visited. A complete depth first search is performed at each node, searching
#' for paths that lead to an already visited (upstream) node. This algorithm
#' is often referred to as "recursive depth first search".
#' @return if no localized loops are found, returns TRUE. If localized
#' loops are found, problem rows with a row number added.
#' @export
#' @examples
#' # notice that row 4 (id = 4, toid = 9) and row 8 (id = 9, toid = 4) is a loop.
#' test_data <- data.frame(id = c(1, 2, 3, 4, 6, 7, 8, 9),
#'                       toid = c(2, 3, 4, 9, 7, 8, 9, 4))
#' check_hy_graph(test_data)
#'
check_hy_graph <- function(x, loop_check = FALSE) {

  if(!inherits(x, "hy")) {
    x <- hy(x)
  }

  if(loop_check) {
    index_ids <- make_index_ids(x)

    starts <- index_ids$to_list$indid[index_ids$to_list$id %in% x$id[!x$id %in% x$toid]]

    check <- check_hy_graph_internal(index_ids, starts)

    check <- unlist(check)

    if(any(!is.na(check))) {
      return(filter(x, id %in% check))
    }

  }

  x <- merge(data.table(mutate(x, row = 1:n())),
             data.table(rename(st_drop_geometry(x), toid_check = toid)),
             by.x = "toid", by.y = "id", all.x = TRUE)

  x <- as_tibble(x)

  check <- x$id == x$toid_check

  if(any(check, na.rm = TRUE)) {

    filter(x, check)

  } else {

    TRUE

  }

}

check_hy_outlets <- function(x, fix = FALSE) {

  if(!inherits(x, "hy")) {
    x <- hy(x)
  }

  check <- !x$toid %in% x$id

  if(any(x$toid[check] != get_outlet_value(x))) {

    if(fix) {

      warning("Outlets don't follow hydroloom convention of 0 or '', fixing.")

      x$toid[check] <- rep(get_outlet_value(x), sum(check))

    } else {

      warning("Outlets don't follow hydroloom convention of 0 or '', not fixing.")

    }

  }

  x

}


check_hy_graph_internal <- function(g, all_starts) {

  f <- make_fromids(g)

  # used to track which path tops we need to go back to
  to_visit_queue <- fastqueue(missing_default = 0)

  lapply(all_starts, function(x) to_visit_queue$add(x))

  out_stack <- faststack()

  # to track where we've been
  visited_tracker <- rep(FALSE, ncol(g$to))

  # Set up the starting node we change node below so this just tracks for clarity
  node <- to_visit_queue$remove()

  # trigger for making a new path
  new_path <- FALSE

  pb = txtProgressBar(0, ncol(g$to), style = 3)
  on.exit(close(pb))
  n <- 0

  while(node > 0) {

    if(!visited_tracker[node])
      n <- n + 1

    # mark it as visited
    visited_tracker[node] <- TRUE

    if(!n %% 100)
      setTxtProgressBar(pb, n)


    # now look at what's downtream and add to a queue
    for(to in seq_len(g$lengths[node])) {

      # Add the next node to visit to the tracking vector
      if(g$to[to, node]!= 0 && !visited_tracker[g$to[to, node]])
        to_visit_queue$add(g$to[to, node])

      # stops us from visiting a node again when we revisit
      # from another upstream path.
      g$to[to, node] <- 0

    }

    # go to the last element added in to_visit_queue
    node <- to_visit_queue$remove()

    # if nothing there, just increment to the next visit position
    # this indicates we hit a new path
    while((node == 0 && to_visit_queue$size() > 0) |
          # or if we are at a node that's already been visited, skip it.
          (node != 0 && visited_tracker[node])) {

      node <- to_visit_queue$remove()

    }

    node_temp <- node

    track <- 0
    while(node != 0 &&
          f$lengths[node] != 0 &&
          any(!visited_tracker[
      f$froms[seq(1, f$lengths[node]), node]])) {

      to_visit_queue$add(node)
      node <- to_visit_queue$remove()

      track <- track + 1
      if(track > to_visit_queue$size()) {
        warning("stuck in a loop at ", g$to_list$id[node_temp])
        out_stack$push(node_temp)

        visited_tracker[node] <- TRUE

        node <- to_visit_queue$remove()

        break
      }
    }

    check <- NULL

    if(node != 0 && !visited_tracker[node])
      check <- loop_search_dfs(g, node, visited_tracker)

    if(!is.null(check)) {
      message("found loop at ", g$to_list$id[check])
      warning("found a loop at ", g$to_list$id[check])
      out_stack$push(check)
    }

  }

  setTxtProgressBar(pb, n)

  # if we got this far, Cool!
  unique(g$to_list$id[as.integer(out_stack$as_list())])

}

loop_search_dfs <- function(g, node, visited_tracker) {

  # stack to track stuff we need to visit
  to_visit_stack <- faststack(missing_default = 0)

  # while we still have nodes to check
  while(node != 0) {

    # means we hit a node that we already visited
    if(visited_tracker[node]) {
      return(node)
    }

    for(to in seq_len(g$lengths[node])) {
      to_visit_stack$push(g$to[to, node])
      g$to[to, node] <- 0
    }

    # grab the next node
    node <- to_visit_stack$pop()

    # if it's 0 grab the next node unless it's empty
    while(!node && to_visit_stack$size() > 0) {
      node <- to_visit_stack$pop()
    }

  }

}
