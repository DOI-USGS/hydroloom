#' Subset Network
#' @description Subsets a network to features upstream of a given outlet.
#' For non-dendritic networks, the function identifies flowpaths connected
#' to the upstream basin via fromnode/tonode that are not reachable by
#' upstream navigation alone. This is useful for networks where an upstream
#' navigation returns a basin that contains a nested basin (closed or intersecting)
#' connected through diversions. If \code{only_up} is \code{FALSE}, those
#' nested basins are captured by navigating downstream from missed diversions
#' to find their outlets, then navigating upstream from those outlets.
#' 
#' Note: If a diversion leaves a basin entirely, the subset will include the 
#' entire basin upstream of where the diversion terminates. this function 
#' will return both closed basins and intersecting basins.
#'
#' @param x data.frame network compatible with \link{hydroloom_names}.
#' @param outlet identifier of the outlet flowpath to subset upstream from.
#' @param only_up logical if \code{TRUE}, only upstream navigation is used and
#' any missed diversion connections are disconnected. If \code{FALSE} (default),
#' nested endorheic basins reachable through diversions are also included.
#' @details
#'
#' Required attributes: `id`, `fromnode`, `tonode`
#'
#' Conditionally: `divergence` (if non-dendritic)
#'
#' @returns data.frame subset of \code{x} containing flowpaths upstream of
#' the outlet.
#' 
#' Note: if "toid" is included in the input, it will be returned without modification.
#' This may result in one or more "toid" entries that contain ids that are not part
#' of the subset.
#' 
#' @name subset_network
#' @export
#' @examples
#' x <- sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom"))
#'
#' sub <- subset_network(x, 8893420)
#'
#' nrow(sub)
#'
subset_network <- function(x, outlet, only_up = FALSE) {
  UseMethod("subset_network")
}

#' @name subset_network
#' @export
subset_network.data.frame <- function(x, outlet, only_up = FALSE) {
  x <- hy(x)

  x <- subset_network(x, outlet, only_up)

  hy_reverse(x)
}

#' @name subset_network
#' @export
subset_network.hy <- function(x, outlet, only_up = FALSE) {

  check_names(x, c(id, fromnode, tonode), "subset_network")

  hy_g <- get_hyg(x, add = TRUE, id = id)

  x <- st_drop_geometry(x)

  leave_toid <- toid %in% names(x)

  x <- add_toids_internal(x)

  index_ids <- make_index_ids(x, mode = "both")

  outlet_row <- x[x$id %in% outlet, ]

  up_ids <- unlist(navigate_network_dfs(index_ids$from, outlet, "up"))

  up <- x[x$id %in% up_ids, ]

  extra_down <- x[!x$id %in% up$id &
                    x$fromnode %in% up$tonode &
                    !x$fromnode %in% outlet_row$tonode, ]

  if (only_up) {

    message("disconnecting ", nrow(extra_down), " missed diversion(s)")

  } else {

    missed <- extra_down$id

    while (length(missed) > 0) {

      down <- navigate_network_dfs(index_ids$to, missed, "down")

      outlets <- x$id[x$toid == get_outlet_value(x) & x$id %in% unlist(down)]

      more_up <- unlist(navigate_network_dfs(index_ids$from, outlets, "up"))

      more_up <- more_up[!more_up %in% up$id]

      up <- bind_rows(up, x[x$id %in% more_up, ])

      missed <- x$id[!x$id %in% up$id &
                        x$fromnode %in% up$tonode &
                        !x$fromnode == outlet_row$tonode]
    }
  }

  result <- x[x$id %in% up$id, ]

  if(!leave_toid)
    result <- select(result, -all_of(toid))
  
  result <- distinct(result)

  result <- put_hyg(result, hy_g)

  result
}
