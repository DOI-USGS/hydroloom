#' all paths depth first
#' @description given a starting node, return all reachable paths. Once visited,
#' a node is marked as visited and will not take part in a future path.
#' @inheritParams add_levelpaths
#' @param starts vector with ids from x to start at.
#' @param direction character only "down" supported so far.
#' @param reset logical if TRUE, reset graph for each start such that later paths
#' will have overlapping results.
#' @return list containing dfs result for each start.
#' @export
#' @examples
#'
#' x <- hy(sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom")))
#'
#' x <- add_toids(x, return_dendritic = FALSE)
#'
#' navigate_network_dfs(x, 8893402)
#'
#' navigate_network_dfs(x, 8897784, direction = "up")
#'
navigate_network_dfs <- function(x, starts, direction = "down", reset = FALSE) {

  x <- hy(x, clean = TRUE)

  if(!all(starts %in% x$id)) stop("all starts must be in x")

  g <- make_index_ids(x)

  if(direction == "up") {
    g <- make_fromids(g, return_list = TRUE)

    names(g) <- c("to", "lengths", "to_list")
    names(g$to_list) <- c("indid", "id", "toindid")
  }

  navigate_network_dfs_internal(g, starts, reset)

}

navigate_network_dfs_internal <- function(g, all_starts, reset) {

  # these are in indid space
  all_starts <- unique(g$to_list$indid[match(all_starts, g$to_list$id)])

  # if reset is TRUE, we keep all connections in subsequent starts runs
  if(reset) save_to <- g$to

  # Some vectors to track results
  # indexes into the full set from all starts
  # used to pull out members of the DFS
  set_index <-
    # indexes into the current path
    # use to split apart the members of the dfs into paths
    path_index <-
    # used to track which path tops we need to go back to
    # this is pre-allocated much much longer than needed
    # a special pointer is used to interact with this tracking vector
    to_visit_pointer <- rep(0, ncol(g$to))

  # to track where we've been
  visited_tracker <- rep(NA_integer_, ncol(g$to))

  # will store output sets (per start) and paths (per sub path)
  out_list <- rep(list(list()), length(all_starts))

  # output set andd path ids used to populate out_list
  set_id <- 1
  path_id <- 1

  # path start sort position used to find loops
  path_start_position <- 1

  for(start in all_starts) {

    # Set up the starting node we change node below so this just tracks for clarity
    node <- start

    # within set node tracker
    node_index <- 1
    # v is a pointer into the to_visit_pointer vector
    visit_index <- 1

    # special handler for empty paths
    none_in_path <- TRUE

    # trigger for making a new path
    new_path <- FALSE

    # only reset if we are explicitely asking for overlapping paths
    if(reset) visited_tracker <- rep(NA_integer_, ncol(g$to))

    while(visit_index > 0) {

      message(g$to_list$id[node])

      if(is.na(visited_tracker[node])) {
        # this is the first time we've seen this,
        # add it to the set in the current path.
        set_index[node_index] <- node
        path_index[node_index] <- path_id
        node_index <- node_index + 1

        visited_tracker[node] <- node_index

        # this path has stuff
        none_in_path = FALSE
      }

      # now look at what's downtream
      for(to in seq_len(g$lengths[node])) {
          # Add the next node to visit to the tracking vector
          to_visit_pointer[visit_index] <- g$to[to, node]

          # break the graph where we've been so we stop next time
          g$to[to, node] <- 0

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

      # if we just hit a new path we need to do a little setup
      if(new_path) {
        # increment the path id and set new_path back to FALSE
        path_id <- path_id + 1
        new_path <- FALSE

      }

    }

    if(none_in_path) {
      out_list[[set_id]] <- list()
    } else {
      out_list[[set_id]] <- split(pull(g$to_list, "id")[set_index[1:(node_index - 1)]], path_index[1:(node_index-1)])
    }

    set_id <- set_id + 1

    # for every start, we need to reset
    if(reset) g$to <- save_to

  }

out_list

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
