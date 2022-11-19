#' all paths depth first
#' @description given a starting node, return all reachable paths
#' @param x hy data.frame containing network topology
#' @param starts vector with ids from x to start at.
#' @param direction character only "down" supported so far.
#' @export
#' @examples
#'
#' x <- hy(sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom")))
#'
#' x <- add_toids(x, return_dendritic = FALSE)
#'
#' all_paths_dfs(x, 8893402)
#'
all_paths_dfs <- function(x, starts, direction = "down") {

  if(direction != "down") {
    stop("up not supported yet.")
    # will need get_fromids fomr nhdplusTools
  }

  g <- make_index_ids(x)

  starts <- unique(g$indid[which(x$id %in% starts)])

  g <- hydroloom:::format_nonden_toid(g)

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
  }

out_list

}
