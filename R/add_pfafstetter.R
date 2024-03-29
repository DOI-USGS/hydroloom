required_atts_pfafsetter <- c(id, toid, total_da_sqkm, topo_sort, levelpath)

#' @title Add Pfafstetter Codes
#' @description Determines Pfafstetter codes for a dendritic network with
#' total drainage area, levelpath, and topo_sort attributes. Topo_sort and
#' levelpath attributes must be self consistent (levelpath values are the
#' same as the outlet topo_sort value) as generated by \link{add_levelpaths}.
#' @inheritParams add_levelpaths
#' @param max_level integer number of levels to attempt to calculate.
#' If the network doesn't have resolution to support the desired level,
#' unexpected behavior may occur.
#' @returns data.frame with added pfafstetter column
#' @name add_pfafstetter
#' @export
#' @examples
#'
#' x <- sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom"))
#'
#' x <- add_toids(x)
#'
#' pfaf <- add_pfafstetter(x, max_level = 2)
#'
#' plot(pfaf["pf_level_2"], lwd = 2)
#'
#' \donttest{
#' if(require(nhdplusTools)) {
#'
#' # uses tempdir for example
#' work_dir <- nhdplusTools::nhdplusTools_data_dir(tempdir())
#'
#' try(
#' source(system.file("extdata/nhdplushr_data.R", package = "nhdplusTools"))
#' )
#' if(exists("hr_data")) {
#' x <- hy(hr_data$NHDFlowline)
#'
#' x <- add_toids(x)
#'
#' x <- dplyr::select(x, id, toid, da_sqkm)
#'
#' #' add terminal_id -- add in function?
#' x <- sort_network(x, split = TRUE)
#'
#' x$total_da_sqkm <- accumulate_downstream(x, "da_sqkm")
#' x$name <- ""
#'
#' x <- add_levelpaths(x, name_attribute = "name", weight_attribute = "total_da_sqkm")
#'
#' x <- add_pfafstetter(x, max_level = 3)
#'
#' plot(x["pf_level_3"], lwd = 2)
#'
#' pfaf <- add_pfafstetter(x, max_level = 4)
#'
#' hr_catchment <- dplyr::left_join(hr_data$NHDPlusCatchment,
#'                                  sf::st_drop_geometry(pfaf), by = c("FEATUREID" = "id"))
#'
#' colors <- data.frame(pf_level_4 = unique(hr_catchment$pf_level_4),
#'                      color = sample(terrain.colors(length(unique(hr_catchment$pf_level_4)))))
#'
#' hr_catchment <- dplyr::left_join(hr_catchment, colors, by = "pf_level_4")
#'
#' plot(hr_catchment["color"], border = NA, reset = FALSE)
#' plot(sf::st_geometry(x), col = "blue", add = TRUE)
#' } else {
#'   message("nhdplusTools > 1.0 required for this example")
#' }
#' }
#' }

add_pfafstetter <- function(x, max_level = 2, status = FALSE) {
  UseMethod("add_pfafstetter")
}

#' @name add_pfafstetter
#' @export
add_pfafstetter.data.frame <- function(x, max_level = 2, status = FALSE) {

  x <- hy(x)

  x <- add_pfafstetter(x, max_level, status)

  hy_reverse(x)

}

#' @name add_pfafstetter
#' @export
add_pfafstetter.hy <- function(x, max_level = 2, status = FALSE) {

  check_names(x, required_atts_pfafsetter, "add_pfafstetter")

  mainstem_levelpath <- unique(x$levelpath[x$topo_sort == min(x$topo_sort)])

  mainstem <- st_drop_geometry(x)[x$levelpath == mainstem_levelpath, ]

  left_join(x,
            bind_rows(get_pfaf_9(select(st_drop_geometry(x),
                                        all_of(required_atts_pfafsetter)),
                                 mainstem, max_level, status = status)) |>

              cleanup_pfaf(), by = id)
}

#' @noRd
get_pfaf_9 <- function(x, mainstem, max_level, pre_pfaf = 0, assigned = NA, status = FALSE) {

  if(!levelpath_outlet_id %in% names(x)) {
    x <- add_levelpath_outlet_ids(x)
  }

  if((pre_pfaf / 10^(max_level-1)) > 1) return()

  if(status && ((pre_pfaf - 1111) %% 1000) == 0) {
    message(paste("On level:", pre_pfaf - 1111))
  }
  # Get all tributary outlets that go to the passed mainstem.
  trib_outlets <- x[x$toid %in% mainstem$id &
                      x$levelpath != mainstem$levelpath[1], ]

  # Exclude those that have already been defined as drainage basin outlets
  if(inherits(assigned, "data.frame")) {
    trib_outlets <- trib_outlets[!trib_outlets$id %in%
                                   assigned$members[(assigned$pfaf %% 2) == 0], ]
  }

  if(length(mainstem$id) == 1 && nrow(trib_outlets) == 0) {
    return()
  }

  # Get the top 4 tributaries (or less) by total drainage area and arrange along the mainstem
  area_filter <- (if(nrow(trib_outlets) >= 4) 4 else nrow(trib_outlets))
  area_filter <- sort(trib_outlets$total_da_sqkm, decreasing = TRUE)[area_filter]
  t4_tribs <- trib_outlets[trib_outlets$total_da_sqkm >= area_filter, ]
  t4_tribs <- left_join(t4_tribs, select(x, "id", ms_ts = "topo_sort"),
                        by = c("toid" = "id")) |> arrange(.data$ms_ts)

  # t4_tribs <- t4_tribs[t4_tribs$ms_ts < max(mainstem$topo_sort),]

  ms_inter <- lapply(seq_len(5), function(x, ms, ts) {
    if(x > (length(ts) + 1)) return(data.frame(id = NA_real_))
    if(x == 1) {
      ms <- ms[ms$topo_sort <= ts[x], ]
    } else if(x == 5 | x == (length(ts) + 1)) {
      ms <- ms[ms$topo_sort > ts[x - 1], ]
    } else {
      ms <- ms[ms$topo_sort > ts[x - 1] & ms$topo_sort <= ts[x], ]
    }
    if(nrow(ms) > 0) ms$p_id <- c(1, 3, 5, 7, 9)[x]
    ms
  }, ms = mainstem, ts = t4_tribs$ms_ts)

  out <- data.frame(p_id = c(1:9))
  out[["members"]] <- list(ms_inter[[1]]$id, x$id[x$levelpath_outlet_id == t4_tribs$levelpath_outlet_id[1]],
                           ms_inter[[2]]$id, x$id[x$levelpath_outlet_id == t4_tribs$levelpath_outlet_id[2]],
                           ms_inter[[3]]$id, x$id[x$levelpath_outlet_id == t4_tribs$levelpath_outlet_id[3]],
                           ms_inter[[4]]$id, x$id[x$levelpath_outlet_id == t4_tribs$levelpath_outlet_id[4]],
                           ms_inter[[5]]$id)
  out[["pfaf"]] <- out$p_id + pre_pfaf * 10

  if(all(sapply(out$members, function(x) all(is.na(x))))) out$members[[1]] <- mainstem$id
  out <- unnest(out, "members")
  out <- list(out[!is.na(out$members), ])

  if(nrow(out[[1]]) == 0 | all(out[[1]]$members %in% mainstem$id)) {
    return(out)
  }

  c(out, unlist(lapply(c(1:9), apply_fun,
                       p9 = out[[1]], x = x, max_level = max_level, status = status),
                recursive = FALSE))
}

apply_fun <- function(p, p9, x, max_level, status) {
  p_sub <- p9[p9$p_id == p, ]
  ms_ids <- p_sub$members
  pre_pfaf <- unique(p_sub$pfaf)
  mainstem <- x[x$id %in% ms_ids, ]

  if(length(pre_pfaf) > 0) {
    get_pfaf_9(x, mainstem, max_level, pre_pfaf = pre_pfaf, assigned = p9, status = status)
  } else {
    NULL
  }
}

#' @noRd
cleanup_pfaf <- function(pfaf) {
  # Add level number
  pfaf$level <- ceiling(log10(pfaf$pfaf + 0.01))
  pfaf <- select(pfaf, -"p_id", id = "members")

  pfaf$uid <- 1:nrow(pfaf)

  # Deduplicate problem tributaries
  remove <- do.call(c, lapply(1:length(unique(pfaf$level)), function(l, pfaf) {
    check <- pfaf[pfaf$level == l, ]
    check <- group_by(check, .data$id)
    check <- filter(check, n() > 1 & .data$pfaf < max(.data$pfaf))$uid
  }, pfaf = pfaf))

  pfaf <- pivot_wider(select(pfaf[!pfaf$uid %in% remove, ], -"uid"),
                      id_cols = "id", names_from = "level",
                      names_prefix = "pf_level_", values_from = "pfaf")

  # replace NAs with known values.
  for(i in 3:ncol(pfaf)) {
    pfaf[, i][is.na(pfaf[, i, drop = TRUE]) & !is.na(pfaf[, (i - 1), drop = TRUE]), ] <-
      1 + (pfaf[, (i - 1)][is.na(pfaf[, i, drop = TRUE]) & !is.na(pfaf[, (i - 1), drop = TRUE]), ] * 10)
  }

  for(i in (ncol(pfaf) - 1):2) {
    pfaf[, i][is.na(pfaf[, i, drop = TRUE]), ] <-
      floor(pfaf[, (i + 1)][is.na(pfaf[, i, drop = TRUE]), ] / 10)
  }

  pfaf
}
