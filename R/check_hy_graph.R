#' Check hy graph
#' @description check that a id toid graph doesn't contain localized loops.
#' @inheritParams add_levelpaths
#' @param loop_check logical if TRUE, a special mode of
#' \link{navigate_network_dfs} is used to walk the entire network from
#' top to bottom searching for loops.
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

    fun <- function(s) {
      lapply(s, function(s) {
        hydroloom:::check_hy_graph_internal(index_ids, s)}
      )
    }

    starts <- split(starts, ceiling(seq_along(starts) / 100))

    check <- pbapply::pblapply(starts, FUN = fun, cl = "future")

    check <- unlist(check)

    if(any(!is.na(check))) {
      return(filter(x, id %in% check))
    }

  }

  x <- left_join(mutate(x, row = 1:n()),
                 drop_geometry(x),
                 by = c("toid" = "id"),
                 relationship = "many-to-many")

  check <- x$id == x$toid.y

  if(any(check, na.rm = TRUE)) {

    filter(x, check)

  } else {

    TRUE

  }

}

check_hy_outlets <- function(x) {

  if(!inherits(x, "hy")) {
    x <- hy(x)
  }

  check <- !x$toid %in% x$id

  if(any(x$toid[check] != get_outlet_value(x))) {

    warning("Outlets don't follow hydroloom convention, fixing.")

    x$toid[check] <- rep(get_outlet_value(x), sum(check))

  }

  x

}


check_hy_graph_internal <- function(g, all_starts) {

  # Some vectors to track results
  # indexes into the full set from all starts
  # used to pull out members of the DFS
  set_index <-
    # used to track which path tops we need to go back to
    # this is pre-allocated much much longer than needed
    # a special pointer is used to interact with this tracking vector
    to_visit_pointer <-
    # to track where we've been
    visited_tracker <- rep(0, ncol(g$to))

  for(start in all_starts) {

    # Set up the starting node we change node below so this just tracks for clarity
    node <- start

    # within set node tracker
    node_index <- 1
    # v is a pointer into the to_visit_pointer vector
    visit_index <- 1

    # trigger for making a new path
    new_path <- FALSE

    while(visit_index > 0) {

      if(visited_tracker[node] > 10) {
        reps <- table(set_index)
        reps <- reps[reps > 5]

        loop_maybe <- as.integer(names(reps))

        loop_maybe <- set_index[set_index %in% loop_maybe]

        warning(paste0("loop below ", g$to_list$id[start], "?"))
        return(g$to_list$id[loop_maybe])
      }

      # this is the first time we've seen this,
      # add it to the set in the current path.
      set_index[node_index] <- node
      node_index <- node_index + 1

      # mark it as visited
      visited_tracker[node] <- visited_tracker[node] + 1

      # now look at what's downtream
      for(to in seq_len(g$lengths[node])) {
        # Add the next node to visit to the tracking vector
        to_visit_pointer[visit_index] <- g$to[to, node]

        # add to the visit index
        visit_index <- visit_index + 1
      }

      # go to the last element added in to_visit_pointer
      visit_index <- visit_index - 1
      node <- to_visit_pointer[visit_index]

      # if nothing there, just increment to the next visit position
      # this indicates we hit a new path
      while(!node && visit_index > 0) {
        visit_index <- visit_index - 1
        node <- to_visit_pointer[visit_index]
        new_path <- TRUE
      }

    }

  }

  # if we got this far, Cool!
  return(NA_integer_)

}

clean_order <- function(x) {
  # the above block needs some clean up to get the value to be nice
  if(is.na(x) | is.infinite(x)) {
    x <- 0
  } else if(x != 0) {
    # the path starts one down from the minimum upstream.
    x <- x + 1
  }
  x
}
