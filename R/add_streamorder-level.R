#' @title add Streamorder
#' @description Adds a strahler stream order.
#'
#' Algorithm: If more than one upstream flowpath has an order equal to the
#' maximum upstream order then the downstream flowpath is assigned the maximum
#' upstream order plus one. Otherwise it is assigned the maximum upstream order.
#'
#' To match the NHDPlus algorithm, non-dendritic network connectivity must
#' be included. All secondary paths will have the `stream_order` of upstream
#' primary paths and a `stream_calculator` value of 0. Secondary paths have
#' no affect on the order of downstream paths.
#'
#' @param x data.frame network compatible with \link{hydroloom_names}.
#' @param status boolean if status updates should be printed.
#' @details
#'
#' Required attributes: `id` and `toid` or `fromnode`, `tonode`, and `divergence`
#'
#' @returns data.frame containing added `stream_order` and `stream_calculator` attribute.
#' @export
#' @name add_streamorder
#' @examples
#' x <- sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom"))
#'
#' x <- dplyr::select(x, COMID, FromNode, ToNode, Divergence)
#'
#' x <- add_streamorder(x)
#'
#' plot(sf::st_geometry(x), lwd = x$stream_order, col = "blue")
#' plot(sf::st_geometry(x), lwd = x$stream_calculator, col = "blue")
#'
add_streamorder <- function(x, status = TRUE) {
  UseMethod("add_streamorder")
}

#' @name add_streamorder
#' @export
add_streamorder.data.frame <- function(x, status = TRUE) {
  x <- hy(x)

  x <- add_streamorder(x, status)

  hy_reverse(x)
}

#' @name add_streamorder
#' @export
add_streamorder.hy <- function(x, status = TRUE) {

  if ("stream_order" %in% names(x)) stop("network already contains a stream_order attribute")

  net <- add_toids_internal(x)

  # if there's any non-dendritic network we need a divergence marker
  if (length(unique(net$id)) < nrow(net)) {

    required_atts <- c(id, toid, divergence)
    error_context <- "If id, fromnode, tonode, and divergence are not supplied, add_streamorder with non-dendritic"

  } else {

    required_atts <- c(id, toid)
    error_context <- "add_streamorder"
  }

  if (!exists("net", inherits = FALSE)) {

    check_names(net, required_atts, error_context)

    net <- select(net, all_of(required_atts))

  }

  out_val <- get_outlet_value(net)

  net$toid <- replace_na(net$toid, out_val)

  # First sort so we have upstream first and outlets last.
  net <- sort_network(net)

  # Now generate a working index against the sorted data.
  index_ids <- make_index_ids(net, mode = "both")

  # will fill in order as we go in this
  order <- rep(1, length(index_ids$from$lengths))
  calc <- rep(1, length(order))

  if (divergence %in% names(x)) {
    # get a divergence marker as logical
    div <- left_join(tibble(id = index_ids$to$to_list$id),
      distinct(select(st_drop_geometry(x),
        all_of(c(id, divergence)))),
      by = id)

    # set divergences to stream calc 0 to be propagated through minor paths
    calc[div$divergence == 2] <- 0

  }

  for (i in seq_along(index_ids$from$lengths)) {

    l <- index_ids$from$lengths[i]

    # nothing to do if nothing upstream
    if (l > 0) {

      # these are the upstream orders
      orders <- order[index_ids$from$froms[1:l, i]]
      calcs <- calc[index_ids$from$froms[1:l, i]]

      # need to know if all upstream catchments are on a minor path
      # all used to reset calc to order downstream of a confluence with a minor path
      all_calc_zero <- all(calcs == 0)
      # any used to control whether to increment or not
      any_calc_zero <- any(calcs == 0)
      # calc was set to zero already so can just move on if it's set
      cur_calc <- calc[i]

      # only consider non calc-0 upstream flowlines!
      # this means a large order flowline that is downstream of a
      # diversion will be ignored
      if (any_calc_zero && !all_calc_zero) {
        orders <- orders[!calcs == 0]
      }

      # Need the max upstream order for this work
      # note this is max upstream order that is not
      # downstream of a diversion
      max_order <- max(orders)

      # the core stream order algorithm:
      # if more than one upstream order is the same
      # as the max upstream order, increment by one.
      # otherwise use the max upstream order.
      # do not increment if one or more calcs are 0
      #
      # if current catchment is set as calc 0, we won't mess with incoming order
      if (cur_calc == 0) {
        order[i] <- max_order
        # If combining two of the same max order AND we are not below a minor path
      } else if (length(orders[orders == max_order]) > 1 && !any_calc_zero) {
        order[i] <- max_order + 1
        calc[i] <- order[i]
        # If we are not on a minor path
      } else if (!all_calc_zero) {
        order[i] <- max_order
        calc[i] <- order[i]
        # if we are on a minor path just pass downstream
      } else {
        order[i] <- max_order
        calc[i] <- 0
      }

    }
  }

  left_join(x,
    bind_cols(id = unique(net$id), tibble(stream_order = order,
      stream_calculator = calc)),
    by = "id")

}

#' @title Add Streamlevel
#' @description Applies a topological sort and calculates stream level.
#' Algorithm: Terminal level paths are assigned level 1 (see note 1).
#' Paths that terminate at a level 1 are assigned level 2. This pattern is
#' repeated until no paths remain.
#'
#' If a TRUE/FALSE coastal attribute is included, coastal terminal paths
#' begin at 1 and internal terminal paths begin at 4 as is implemented by
#' the NHD stream leveling rules.
#' @param x data.frame network compatible with \link{hydroloom_names}.
#' @details
#'
#' Required attributes: `levelpath`, `dn_levelpath`
#'
#' @param coastal character attribute name containing a logical flag
#' indicating if a given terminal catchment flows to the coast of is an
#' inland sink. If no coastal flag is included, all terminal paths are
#' assumed to be coastal.
#'
#' @returns data,frame containing added `stream_level` attribute
#' @export
#' @name add_streamlevel
#' @examples
#' x <- sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom"))
#'
#' x <- add_toids(x)
#' 
#' x <- dplyr::rename(x, orig_stream_level = StreamLeve)
#' 
#' y <- add_streamlevel(x)
#'
#' plot(sf::st_geometry(y), lwd = y$stream_level, col = "blue")
#'
#' x$coastal <- rep(FALSE, nrow(x))
#'
#' y <- add_streamlevel(x, coastal = "coastal")
#'
#' unique(y$stream_level)
#'
#' x$coastal[!x$Hydroseq == min(x$Hydroseq)] <- TRUE
#'
#' y <- add_streamlevel(x)
#'
#' unique(y$stream_level)
#'
add_streamlevel <- function(x, coastal = NULL) {
  UseMethod("add_streamlevel")
}

#' @name add_streamlevel
#' @export
add_streamlevel.data.frame <- function(x, coastal = NULL) {
  x <- hy(x)

  x <- add_streamlevel(x, coastal)

  hy_reverse(x)
}

#' @name add_streamlevel
#' @export
add_streamlevel.hy <- function(x, coastal = NULL) {

  check_names(x, c(levelpath, dn_levelpath), "add_streamlevel")

  x$dn_levelpath <- replace_na(x$dn_levelpath, 0)

  net <- select(st_drop_geometry(x), any_of(c(levelpath, dn_levelpath, coastal)))

  l <- net |>
    filter(.data$levelpath != .data$dn_levelpath) |>
    rename(id = "levelpath",
      toid = "dn_levelpath") |>
    sort_network() |>
    distinct()

  l <- l[rev(seq_len(nrow(l))), ]

  l$level <- rep(0, nrow(l))

  l$level[!l$toid %in% l$id] <- 1

  if (!is.null(coastal) && coastal %in% names(l)) {
    l$level[l$level == 1 & !l[[coastal]]] <- 4
  }

  id <- l$id
  toid <- l$toid
  level <- l$level

  toids <- match(toid, id)

  # walk the network from bottom path up
  for (i in seq_along(id)) {
    if (!is.na(toids[i])) {

      level[i] <- # level at current
        level[toids[i]] + 1 # level of downstream + 1

    }
  }

  left_join(x, tibble(levelpath = id, stream_level = level), by = "levelpath")

}
