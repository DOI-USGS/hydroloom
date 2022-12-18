required_atts_navigate <- function(mode, distance) {
  required_atts <- list(UM = c(id, levelpath, topo_sort),
                        DM = c(id, levelpath, dn_levelpath,
                               topo_sort, dn_topo_sort),
                        UT = c(id, levelpath,
                               topo_sort, dn_topo_sort),
                        DD = c(id, levelpath, dn_levelpath,
                               topo_sort, dn_topo_sort, dn_minor_topo_sort))

  if(!is.null(distance)) required_atts <-
      sapply(required_atts, function(x) c(x, c(pathlength_km, length_km)))

  required_atts[[mode]]
}

get_start_row <- function(network, id) {
  start_row <- network[network$id == id, ]

  if(nrow(start_row) > 1) {
    stop("Found duplicate id for starting catchment. Duplicate rows in network?")
  }

  start_row
}

#' @title Navigate Hydro Network
#' @description Navigates a network of connected catchments using NHDPlus style
#' network attributes.
#' @param network data.frame compatible with \link{hydroloom_names}.
#' @param start character or numeric to match identifier attribute. The
#' starting catchment is included.
#' @param mode character chosen from c(UM, DM, UT, or DD).
#'
#' \enumerate{
#'   \item UM: upstream mainstem
#'   \item DM: downstream main
#'   \item UT: upstream with tributaries
#'   \item DD: downstream with diversions
#' }
#'
#' @param distance numeric distance in km to limit navigation. The first
#' catchment that exceeds the provided distance is included.
#' @details if only `mode` is supplied, require network attributes are displayed.
#' @return vector of identifiers found along navigation
#' @name navigate_hydro_network
#' @export
#' @examples
#'
#' plot_fun <- function(x, s, n) {
#'    plot(sf::st_geometry(x), col = "grey")
#'    plot(sf::st_geometry(x[x$id %in% n, ]), add = TRUE)
#'    plot(sf::st_geometry(x[x$id %in% s, ]), col = "red", lwd = 3, add = TRUE)
#' }
#'
#' x <- hy(sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom")))
#'
#' start <- 8891126
#' dm <- navigate_hydro_network(x, start, "DM")
#'
#' plot_fun(x, start, dm)
#'
#' dd <- navigate_hydro_network(x, start, "DD")
#'
#' plot_fun(x, start, dd)
#'
#' start <- 8894356
#'
#' um <- navigate_hydro_network(x, start, "UM")
#'
#' plot_fun(x, start, um)
#'
#' ut <- navigate_hydro_network(x, start, "UT")
#'
#' plot_fun(x, start, ut)
#'
navigate_hydro_network <- function(network, start, mode, distance = NULL) {
  if(missing(mode) | !mode %in% c('UM', 'DM', 'UT', 'DD')) {
    stop("must choose mode input from: 'UM', 'DM', 'UT', 'DD'")
  }

  required_atts <- required_atts_navigate(mode, distance)

  if(missing(network)) {
    required_atts_error(context = mode, required_atts = required_atts)
  }

  UseMethod("navigate_hydro_network")
}


#' @name navigate_hydro_network
#' @export
navigate_hydro_network.data.frame <- function(network, start, mode, distance = NULL) {
  navigate_hydro_network(hy(network), start, mode, distance)
}

#' @name navigate_hydro_network
#' @export
navigate_hydro_network.hy <- function(network, start, mode, distance = NULL) {

  required_atts <- required_atts_navigate(mode, distance)

  if(!all(required_atts %in% names(network))) required_atts_error(mode, required_atts)

  fun <- switch (mode,
    "UT" = get_UT,
    "UM" = get_UM,
    "DM" = get_DM,
    "DD" = get_DD
  )

  fun(select(drop_geometry(network), all_of(required_atts)),
      start, distance)

}

get_UT <- function(network, id, distance) {

  start_row <- get_start_row(network, id)

  if (!is.null(distance)) {
    if (distance < start_row$length_km) return(id)
  }

  all <- private_get_UT(network, id)

  if (!is.null(distance)) {
    stop_pathlength_km <- start_row$pathlength_km -
      start_row$length_km +
      distance

    network <- filter(network, .data$id %in% all)

    return(filter(network, .data$pathlength_km <= stop_pathlength_km)$id)
  } else {
    return(all)
  }
}

private_get_UT <- function(network, id) {

  # expect multiples
  main <- network[network$id %in% id, ]

  if (length(main$topo_sort) == 1) {
    full_main <- filter(network,
                        levelpath %in% main$levelpath &
                          topo_sort >= main$topo_sort)

    trib_lpid <- filter(network, dn_topo_sort %in% full_main$topo_sort &
                          !levelpath %in% main$levelpath  &
                          topo_sort >= main$topo_sort)$levelpath
  } else {
    full_main <- filter(network, levelpath %in% main$levelpath)

    trib_lpid <- filter(network, dn_topo_sort %in% full_main$topo_sort &
                          !levelpath %in% main$levelpath)$levelpath
  }

  trib_id <- filter(network, levelpath %in% trib_lpid)$id

  if (length(trib_id) > 0) {
    return(c(full_main$id, private_get_UT(network, trib_id)))
  } else {
    return(full_main$id)
  }
}

get_UM <- function(network, id, distance = NULL) {

  main <- get_start_row(network, id)

  main_us <- filter(network, .data$levelpath %in% main$levelpath &
                             .data$topo_sort >= main$topo_sort)

  if (!is.null(distance)) {

    if (length(main$length_km) == 1) {
      if (main$length_km > distance) {
        return(main$id)
      }
    }

    stop_pathlength_km <- main$pathlength_km - main$length_km + distance

    main_us <- filter(main_us, .data$pathlength_km <= stop_pathlength_km)

  }

  return(main_us$id)
}

get_DM <- function(network, id, distance = NULL) {

  start_row <- get_start_row(network, id)

  if (!is.null(distance)) {
    if (distance < start_row$length_km){
      return(id)
    }
  }

  main_ds <- private_get_DM(network, id)

  if (!is.null(distance)) {

    stop_pathlength_km <- start_row$pathlength_km + start_row$length_km - distance

    main_ds <- network |>
      filter(id %in% main_ds$id, (pathlength_km + length_km) >= stop_pathlength_km)
  }

  return(main_ds$id)
}


private_get_DM <- function(network, id) {

  main <- ds_main <- network[network$id %in% id, ]

  if (length(main$topo_sort) == 1) {
    ds_main <- network |>
      filter(levelpath %in% main$levelpath &
               topo_sort <= main$topo_sort)
  }

  ds_hs <- ds_main |>
    filter(!dn_levelpath %in% main$levelpath) |>
    select(dn_topo_sort)

  if (nrow(ds_hs) > 0) {

    ds_lpid <- network |>
      filter(topo_sort == ds_hs$dn_topo_sort) |>
      select(levelpath)

    if (nrow(ds_lpid) > 0) {
      ds_id <- network |>
        filter(levelpath == ds_lpid$levelpath & topo_sort <= ds_hs$dn_topo_sort) |>
        select(id)

      return(rbind(
        select(ds_main, id, topo_sort),
        private_get_DM(network, id = ds_id$id)
      ))
    }
  }

  return(select(ds_main, id, topo_sort))
}

get_DD <- function(network, id, distance = NULL) {

  start_row <- get_start_row(network, id)

  stop_pathlength_km <- 0

  if (!is.null(distance)) {
    if (distance < start_row$length_km) return(id)

    stop_pathlength_km <- start_row$pathlength_km +
      start_row$length_km -
      distance
  }
  all <- private_get_DD(network, id, stop_pathlength_km)

  if (!is.null(distance)) {
    network <- filter(network, id %in% unique(all))

    return(filter(network, (pathlength_km + length_km) >= stop_pathlength_km)$id)
  } else {
    return(unique(all))
  }
}

private_get_DD <- function(network, id, stop_pathlength_km = 0) {

  main <- ds_main <- network[network$id %in% id, ]

  if (length(main$topo_sort) == 1) {
    ds_main <- filter(network,
                      .data$levelpath %in% main$levelpath &
                        .data$topo_sort <= main$topo_sort)
  }

  ds_hs <- c(filter(ds_main, !.data$dn_levelpath %in% main$levelpath)$dn_topo_sort,
             filter(ds_main, !.data$dn_minor_topo_sort == 0)$dn_minor_topo_sort)

  ds_lpid <- filter(network, .data$topo_sort %in% ds_hs)$levelpath

  if (length(ds_lpid) > 0) {
    if (length(ds_hs) == 1) {
      # Same as DM
      ds_id <- filter(network,
                      .data$levelpath %in% ds_lpid &
                        .data$topo_sort <= ds_hs)$id
    } else {
      # Works for divergent paths.
      ds_hs <- filter(network, .data$topo_sort %in% ds_hs)
      ds_id <- filter(network, .data$levelpath %in% ds_lpid) |>
        left_join(select(ds_hs, levelpath, max_topo_sort = topo_sort),
                         by = "levelpath") |>
        filter(.data$topo_sort <= .data$max_topo_sort)
      ds_id <- ds_id$id
    }

    # This allows this algorithm to work for short distances
    # in a reasonable time in large systems.
    if ("pathlength_km" %in% names(ds_main) &
        all(ds_main$pathlength_km <= stop_pathlength_km)) return(ds_main$id)

    c(ds_main$id, private_get_DD(network, ds_id, stop_pathlength_km))
  } else {
    return(ds_main$id)
  }
}


