#' @title Navigate Connected Paths
#' @description Given a dendritic network and set of ids, finds paths or
#' lengths between all identified flowpath outlets. This algorithm finds
#' paths between outlets regardless of flow direction.
#' @param x data.frame network compatible with \link{hydroloom_names}.
#' @param outlets vector or list of length 2 of ids from data.frame
#' @param status logical print status and progress bars?
#' @details
#'
#' Required attributes: `id`, `toid`, `length_km`
#' 
#' If `outlets` is a vector, all combinations (\link{combn}) of the given `outlets` will be produced.
#' If `outlets` is a list, it must be of length 2 (froms, tos). 
#' Recycling is performed via \link{expand.grid} if child vector lengths do not match.
#' `outlets` may also be a 2-column dataframe.
#'
#' @returns data.frame containing the distance between pairs of network outlets
#' and a list column containing flowpath identifiers along path that connect outlets.
#' For a network with one terminal outlet, the data.frame will have `nrow(x)^2` rows.
#' @export
#' @examples
#' x <- sf::read_sf(system.file("extdata", "walker.gpkg", package = "hydroloom"))
#'
#' outlets <- c(5329303, 5329357, 5329317, 5329365, 5329435, 5329817)
#'
#' x <- add_toids(hy(x))
#'
#' navigate_connected_paths(x, outlets)
#'
navigate_connected_paths <- function(x, outlets, status = FALSE) {
  x <- hy(x)

  if (!status) {
    pbopts <- pboptions(type = "none")
    on.exit(pboptions(pbopts), add = TRUE)
  }

  outlet_ids <- if (is.list(outlets)) {
    stopifnot(length(outlets) == 2)
    unique(unlist(outlets))
  } else {
    stopifnot(is.vector(outlets))
    unique(outlets)
  }

  if (!all(outlet_ids %in% x$id))
    stop("All outlets must be in x.")

  x <- st_drop_geometry(x)

  if (any(duplicated(x$id)))
    stop("x contains duplicate ids. Please remove duplicates before proceeding.")

  index <- make_index_ids(distinct(select(x, id, toid)))

  if (dim(index$to)[1] != 1) stop("network must be dendritic")

  get_dwn <- function(indid, toindid) {
    next_dn <- toindid[indid]
    out <- list(indid)
    i <- 2
    while (next_dn != 0) {
      indid <- next_dn
      next_dn <- toindid[indid]
      out[[i]] <- indid
      i <- i + 1
    }
    unlist(out)
  }

  id_match <- match(outlet_ids, index$to_list$id)
  
  if (status)
    message("Finding all downstream paths.")

  # we could in theory be storing these as we go and looking up
  # subsequences but that gets pretty complicated
  all_dn <- pblapply(id_match, function(indid, toindid) {
    out <- get_dwn(indid, toindid)
    if ((lo <- length(out)) > 1) {
      out[2:lo] # don't want to include the starting flowpath
    } else {
      out[1]
    }
  }, toindid = index$to)

  if (status)
    message("Finding all connected pairs.")    

  get_path <- function(p, all_dn) {
    x <- all_dn[[p[1]]]
    y <- all_dn[[p[2]]]
    x_len <- length(x)
    y_len <- length(y)

    if (x_len == 1) # if one end is a terminal
      return(y)
    
    if (y_len == 1)
      return(x)
    
    if (x[x_len] == y[y_len]) {
      return(c(x[match(x, y, nomatch = 0) == 0], y[match(y, x, nomatch = 0) == 0]))
    }
    
    numeric(0)
  }

  # outlet_ids and id_match are interchangeable in combn
  # but for outlet list we need to get to an index
  pairs <- if(is.list(outlets)) {
    if (is.data.frame(outlets) || diff(lengths(outlets)) == 0) {
      # if the lengths are equal no recycling should be applied
      sapply(outlets, \(x) match(x, outlet_ids))
    } else {
      expand.grid(lapply(outlets, \(x) match(x, outlet_ids)))
    }
  } else {
    t(combn(length(id_match), 2))
  }

  length_km <- x$length_km

  paths <- pbapply(pairs, 1, \(p) {
    path <- get_path(p, all_dn)
    if (length(path) != 0) {
      list(
        id_1 = p[1],
        id_2 = p[2],
        network_distance_km = sum(length_km[path]),
        path = list(path)
      )  
    }
  }, simplify = FALSE)

  df <- rbindlist(paths)

  df$id_1 <- outlet_ids[match(df$id_1, seq_along(id_match))]
  df$id_2 <- outlet_ids[match(df$id_2, seq_along(id_match))]

  as.data.frame(df)
}

#' @title Get Partial Flowpath Length
#' @param hydro_location list containing a hydrologic locations with names
#' aggregate_id (reachcode) and aggregate_id_measure (reachcode measure).
#' @param network data.frame network compatible with \link{hydroloom_names}.
#' @param flowpath data.frame containing one flowpath that corresponds to the
#' `hydro_location`. Not required if `x` is provided. `x` is not required if
#' `flowpath` is provided.
#' @description Finds the upstream and downstream lengths along a given
#' flowline. Internally, the function
#' rescales the aggregate_id_measure to a id_measure and applies that
#' rescaled measure to the length of the flowline.
#'
#' @returns list containing `up` and `dn` elements with numeric length in
#' km.
#' @export
#' @examples
#'
#' x <- sf::read_sf(system.file("extdata", "walker.gpkg", package = "hydroloom"))
#'
#' hydro_location <- list(comid = 5329339,
#'   reachcode = "18050005000078",
#'   reach_meas = 30)
#'
#' (pl <- get_partial_length(hydro_location, x))
#'
get_partial_length <- function(hydro_location, network = NULL, flowpath = NULL) {

  hydro_location <- align_names(as.data.frame(hydro_location))

  check_names(hydro_location, c(aggregate_id_measure, aggregate_id),
    "get_partial_length hydro_location")

  if (is.null(flowpath)) {

    if (is.null(network)) {
      stop("network must be supplied if flowline is null")
    }

    network <- hy(network)

    check_names(network, c(aggregate_id, aggregate_id_from_measure, aggregate_id_to_measure),
      "get_partial_length x")
    flowpath <- get_fl(hydro_location, network)
  } else {
    flowpath <- align_names(flowpath)
    check_names(flowpath, c(aggregate_id, aggregate_id_from_measure, aggregate_id_to_measure))
  }

  if (nrow(flowpath) == 0) {
    stop("hydrolocation not found in network provided")
  }

  meas <- rescale_measures(measure = hydro_location$aggregate_id_measure,
    from = flowpath$aggregate_id_from_measure,
    to = flowpath$aggregate_id_to_measure) / 100

  list(dn = flowpath$length_km * meas,
    up = flowpath$length_km * (1 - meas))
}
