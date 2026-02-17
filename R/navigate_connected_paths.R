#' @title Navigate Connected Paths
#' @description Given a dendritic network and set of ids, finds paths or
#' lengths between all identified flowpath outlets. This algorithm finds
#' paths between outlets regardless of flow direction.
#' @param x data.frame network compatible with \link{hydroloom_names}.
#' @param outlets vector of ids from data.frame
#' @param status logical print status and progress bars?
#' @details
#'
#' Required attributes: `id`, `toid`, `length_km`
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

  stopifnot(is.vector(outlets))

  if (!all(outlets %in% x$id))
    stop("All outlets must be in x.")

  x <- st_drop_geometry(x)

  if (any(duplicated(x$id)))
    stop("x contains duplicate ids. Please remove duplicates before proceeding.")

  index <- make_index_ids(distinct(select(x, id, toid)))

  if (dim(index$to)[1] != 1) stop("network must be dendritic")

  get_dwn <- function(indid, toindid) {
    next_dn <- toindid[1, indid]
    if (next_dn == 0) {
      indid
    } else {
      c(indid, get_dwn(next_dn, toindid))
    }
  }

  id_match <- match(outlets, index$to_list$id)

  if (status)
    message("Finding all downstream paths.")

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

    if (length(x) == 1) # if one end is a terminal
      return(list(x = integer(0), y = y))

    if (length(y) == 1)
      return(list(x = x, y = integer(0)))

    if (tail(x, 1) == tail(y, 1))
      return(list(x = x[!x %in% y], y = y[!y %in% x]))

    list()
  }

  pairs <- t(combn(length(id_match), 2))

  paths <- pbapply(pairs, 1, get_path, all_dn = all_dn, cl = future_available())

  connected_paths <- paths[lengths(paths) > 0]

  length_km <- select(left_join(index$to_list,
    select(x, id, "length_km"),
    by = id),
  id, "length_km")

  if (status)
    message("Summing length of all connected pairs.")

  get_length <- function(p, length_km) {
    sum(length_km$length_km[p[[1]]], length_km$length_km[p[[2]]])
  }

  path_lengths <- pblapply(connected_paths, get_length, length_km = length_km)

  path_lengths <- cbind(as.data.frame(matrix(id_match[pairs[lengths(paths) > 0, ]],
    ncol = 2)),
  data.frame(length = as.numeric(path_lengths)))

  names(path_lengths) <- c("indid_1", "indid_2", "network_distance_km")

  paths <- cbind(as.data.frame(matrix(id_match[pairs[lengths(paths) > 0, ]],
    ncol = 2)))

  names(paths) <- c("indid_1", "indid_2")

  paths[["path"]] <- lapply(connected_paths, function(x) {
    c(x$x, x$y)
  })

  path_lengths <- left_join(path_lengths, paths, by = c("indid_1", "indid_2"))

  path_lengths <- left_join(path_lengths,
    select(index$to_list, id_1 = id, indid),
    by = c("indid_1" = "indid")) |>
    left_join(select(index$to_list, id_2 = id, indid),
      by = c("indid_2" = "indid")) |>
    select(-"indid_1", -"indid_2")

  select(path_lengths, all_of(c("id_1", "id_2", "network_distance_km", "path")))

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
