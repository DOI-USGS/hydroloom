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

navigate_network_dfs_internal <- function(g, starts, reset) {

  starts <- unique(g$to_list$indid[match(starts, g$to_list$id)])

  if(reset) save_to <- g$to

  # Some vectors to track results
  set <- to_visit <- path <- rep(0, ncol(g$to))

  # to track where we've been
  visited <- rep(0, ncol(g$to))


  out_list <- rep(list(list()), length(starts))

  # output order tracker
  o <- 1
  set_id <- 1
  path_id <- 1

  for(s in starts) {

    # Set up the starting node
    node <- s

    # within set node tracker
    n <- 1
    # v is a pointer into the to_visit vector
    v <- 1

    none <- TRUE

    # trigger for making a new path
    new_path <- FALSE

    if(reset) visited <- rep(0, ncol(g$to))

    while(v > 0) {

      if(visited[node] == 0) {

        set[n] <- node
        path[n] <- path_id
        n <- n + 1

        visited[node] <- visited[node] + 1

        none = FALSE
      }

      for(to in seq_len(g$lengths[node])) {
          # Add the next node to visit to the tracking vector
          to_visit[v] <- g$to[to, node]
          g$to[to, node] <- 0
          v <- v + 1
        }

      # go to the last element added in to_visit
      v <- v - 1
      node <- to_visit[v]

      while(!node && v > 0) {
        v <- v - 1
        node <- to_visit[v]
        new_path <- TRUE
      }

      if(new_path) {
        path_id <- path_id + 1
        new_path <- FALSE
      }

    }

    if(none) {
      out_list[[set_id]] <- list()
    } else {
      out_list[[set_id]] <- split(pull(g$to_list, "id")[set[1:(n - 1)]], path[1:(n-1)])
    }

    set_id <- set_id + 1

    if(reset) g$to <- save_to

  }

out_list

}
