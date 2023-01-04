#' Get Paths
#' @description Given a network and set of ids, finds paths or lengths between all
#' identified flowpath outlets. This algorithm finds paths between outlets
#' regardless of flow direction.
#' @inheritParams add_levelpaths
#' @param outlets vector of ids from data.frame
#' @param cores integer number of cores to use for parallel computation.
#' @param status logical print status and progress bars?
#' @return data.frame containing the distance between pairs of network outlets
#' and a list column containing flowpath identifiers along path that connect outlets.
#' For a network with one terminal outlet, the data.frame will have `nrow(x)^2` rows.
#' @importFrom utils combn
#' @export
#' @examples
#' x <- sf::read_sf(system.file("extdata", "walker.gpkg", package = "hydroloom"))
#'
#' outlets <- c(5329303, 5329357, 5329317, 5329365, 5329435, 5329817)
#'
#' x <- add_toids(hy(x))
#'
#' navigate_connected_paths(fl, outlets)
#'
navigate_connected_paths <- function(x, outlets, status = FALSE) {
  x <- hy(x)

  if(!status) {
    pbopts <- pboptions(type = "none")
    on.exit(pboptions(pbopts), add = TRUE)
  }

  stopifnot(is.vector(outlets))

  if(!all(outlets %in% x$id))
    stop("All outlets must be in x.")

  x <- drop_geometry(x)

  index <- make_index_ids(select(x, id, toid), long_form = TRUE)

  get_dwn <- function(indid, toindid) {
    next_dn <- toindid[indid]
    if(next_dn == 0) {
      return(indid)
    } else {
      return(c(indid, get_dwn(next_dn, toindid)))
    }
  }

  id_match <- match(outlets, index$id)

  if(status)
    message("Finding all downstream paths.")

  all_dn <- pbapply::pblapply(index$indid[id_match], function(indid, toindid) {
    out <- get_dwn(indid, toindid)
    if((lo <- length(out)) > 1) {
      out[2:lo] # don't want to include the starting flowpath
    } else {
      out[1]
    }
  }, toindid = index$toindid)

  if(status)
    message("Finding all connected pairs.")

  get_path <- function(p, all_dn) {
    x <- all_dn[[p[1]]]
    y <- all_dn[[p[2]]]

    if(length(x) == 1) # if one end is a terminal
      return(list(x = integer(0), y = y))

    if(length(y) == 1)
      return(list(x = x, y = integer(0)))

    if(tail(x, 1) == tail(y, 1))
      return(list(x = x[!x %in% y], y = y[!y %in% x]))

    list()
  }

  pairs <- t(combn(length(id_match), 2))
  paths <- pbapply(pairs, 1, get_path, all_dn = all_dn, cl = "future")

  connected_paths <- paths[lengths(paths) > 0]

  length_km <- select(left_join(index,
                               select(x, id, "length_km"),
                               by = id),
                     id, "length_km")

  if(status)
    message("Summing length of all connected pairs.")

  get_length <- function(p, length_km)
    sum(length_km$length_km[p[[1]]], length_km$length_km[p[[2]]])

  path_lengths <- pbapply::pblapply(connected_paths, get_length, length_km = length_km)

  path_lengths <- cbind(as.data.frame(matrix(id_match[pairs[lengths(paths) > 0,]],
                                             ncol = 2)),
                        data.frame(length = as.numeric(path_lengths)))

  names(path_lengths) <- c("indid_1", "indid_2", "network_distance_km")

  paths <- cbind(as.data.frame(matrix(id_match[pairs[lengths(paths) > 0,]],
                                      ncol = 2)))

  names(paths) <- c("indid_1", "indid_2")

  paths[["path"]] <- lapply(connected_paths, function(x) {
    c(x$x, x$y)
  })

  path_lengths <- left_join(path_lengths, paths, by = c("indid_1", "indid_2"))

  path_lengths <- left_join(path_lengths,
                            select(index, id_1 = id, indid),
                            by = c("indid_1" = "indid")) |>
    left_join(select(index, id_2 = id, indid),
              by = c("indid_2" = "indid")) |>
    select(-"indid_1", -"indid_2")

  select(path_lengths, all_of(c("id_1", "id_2", "network_distance_km", "path")))

}
