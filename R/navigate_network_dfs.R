#' @title Navigate all Paths Depth First
#' @description given a starting node, return all reachable paths. Once visited,
#' a node is marked as visited and will not take part in a future path.
#' @param x data.frame containing hydroloom compatible network or list
#' as returned by \link{make_index_ids} (for down) or \link{make_fromids}
#' (for up). The list formats avoids recreating the index ids for every call
#' to navigate network dfs in the case that it needs to be called many times.
#' @param starts vector with ids from x to start at.
#' @param direction character "up", "upmain", "down", or "downmain". If "upmain" or
#' "downmain", x must contain sufficient information to construct an upmain and
#' downmain network (see details).
#' @param reset logical if TRUE, reset graph for each start such that later paths
#' will have overlapping results.
#'
#' @details
#' `navigate_network_dfs` offers two usage patterns. In the simple case,
#' you can provide an `hy` in which case preprocessing is performed automatically,
#' or you can do the preprocessing ahead of time and provide index ids. The latter
#' is more complicated but can be much faster in certain circumstances.
#'
#' `hy` object:
#'
#' If the function will only be called one or a few times, it can be called
#' with x containing (at a minimum) `id` and `toid`. For "upmain" and "downmain"
#' support, x also requires attributes for determination of the primary upstream
#' and downstream connection across every junction.
#'
#' In this pattern, the `hy` object will be passed to  \link{make_index_ids}
#' called for every call to `navigate_network_dfs` and the resulting index ids
#' will be used for network navigation.
#'
#' Index ids:
#'
#' If the function will be called repeatedly or index_ids are available for
#' other reasons, the index_id list as created by \link{make_index_ids} (for
#' downstream) or \link{make_fromids} (for upstream) can be used. For "upmain"
#' and "downmain" support, the `main` element must be included.
#'
#'
#' @returns list containing dfs result for each start.
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


  # check if we to index ids or from index ids
  # should this be done with a class attribute?

  if(all(c("to", "lengths", "to_list") %in% names(x))) {

    if(!grepl("down", direction))
      stop("Direction must be 'down' if x contains to index ids")

  } else if(all(c("froms", "lengths", "froms_list") %in% names(x))) {

    if(!grepl("up", direction))
      stop("Direction must be 'up' if x contains from index ids")

  } else {

    # prepare the flownetwork for navigation
    # all of this has to be done seperately for the above two cases

    if(!inherits(x, "data.frame"))
      stop("if x is not a length three list as
            returned by make_index_ids or make_fromids
            it must be a data.frame that can be coerced
            to an hy object.")

    x <- hy(x, clean = TRUE)

    if(!toid %in% names(x) & # if we can create a flow network
       fromnode %in% names(x) &
       grepl("main", direction)) { # and main is the goal
      x <- to_flownetwork(x)
    } else if(!toid %in% names(x) & fromnode %in% names(x)) {
      # otherwise make sure we have toids
      x <- add_toids(x, return_dendritic = FALSE)
    }

    g <- make_index_ids(x)

    if(grepl("up", direction)) {

      upmain_atts <- NULL
      if(direction == "upmain") {
        upmain_atts <- distinct(select(x, all_of(c(id, upmain))))
      }

      g <- make_fromids(g, return_list = TRUE, upmain = upmain_atts)
    }

    rm(x) # this can be huge

  }

  if(!exists(("g"))) {
    g <- x
    rm(x)
  }

  # if we are going upstream, we need to update names
  # the code below assumes the graph is directed where we are going
  if(grepl("up", direction)) {
    g_names <- c("to", "lengths")
    to_list_names <- c("indid", "id", "toindid")

    if(grepl("main", direction)) {
      g_names <- c(g_names, "main")
      to_list_names <- c(to_list_names, "main")
    }

    names(g) <- c(g_names, "to_list")
    names(g$to_list) <- to_list_names
  }

  # if the above resulted in dat that won't work
  if(grepl("main", direction) & !"main" %in% names(g))
    stop("main path must be provided for downmain navigation")

  # make sure this will actually work!
  if(!all(starts %in% g$to_list$id)) stop("all starts must be in x")

  main <- grepl("main", direction)

  navigate_network_dfs_internal(g, starts, reset, main)

}

navigate_network_dfs_internal <- function(g, all_starts, reset, main) {

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

  # output set and path ids used to populate out_list
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
      if(main) {

        for(to in seq_len(g$lengths[node])) {
          if(g$to[to, node] != 0 && g$main[to, node]) {
            # Add the next node to visit to the tracking vector
            to_visit_pointer[visit_index] <- g$to[to, node]

            # break the graph where we've been so we stop next time
            g$to[to, node] <- 0

            # add to the visit index
            visit_index <- visit_index + 1
          }
        }

      } else {
        for(to in seq_len(g$lengths[node])) {
          # Add the next node to visit to the tracking vector
          to_visit_pointer[visit_index] <- g$to[to, node]

          # break the graph where we've been so we stop next time
          g$to[to, node] <- 0

          # add to the visit index
          visit_index <- visit_index + 1
        }
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
