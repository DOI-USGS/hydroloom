#' all paths depth first
#' @description given a starting node, return all reachable paths. Once visited,
#' a node is marked as visited and will not take part in a future path.
#' @param x hy data.frame containing network topology
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
#' all_paths_dfs(x, 8893402)
#'
all_paths_dfs <- function(x, starts, direction = "down", reset = FALSE) {

  if(direction != "down") {
    stop("up not supported yet.")
    # will need get_fromids fomr nhdplusTools
  }

  x <- hy(x, clean = TRUE)

  if(!all(starts %in% x$id)) stop("all starts must be in x")

  g <- make_index_ids(x, format = TRUE, complete = TRUE)

  starts <- unique(g$to_list$indid[which(unique(x$id) %in% starts)])

  if(reset) save_to <- g$to

  # Some vectors to track results
  set <- to_visit <- path <- rep(0, nrow(x))

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

    # trigger for making a new path
    new_path <- FALSE

    while(v > 0) {

      # increment to the next node
      o <- o + 1

      set[n] <- node
      path[n] <- path_id
      n <- n + 1

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

    out_list[[set_id]] <- split(unique(x[,1])[set[1:(n - 1)]], path[1:(n-1)])
    set_id <- set_id + 1

    if(reset) g$to <- save_to
  }

out_list

}
