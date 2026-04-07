utils::globalVariables(c("priority", "lp_name_attribute", "ds_nameid", "lp_weight_attribute"))

required_atts_add_levelpaths <- c("id", "toid")

#' Add Level Paths
#' @description Assigns level paths using the stream-leveling approach of
#' NHD and NHDPlus. If arbolate sum is provided in the weight column, this
#' will match the behavior of NHDPlus. Any numeric value can be
#' included in this column and the largest value will be followed when
#' no nameid is available.
#'
#' x must include id, toid, and conditionally divergence attributes.
#' If a "topo_sort" (hydrosequence in nhdplus terms) attribute is included,
#' it will be used instead of recreation.
#'
#' @param x data.frame network compatible with \link{hydroloom_names}.
#' @param name_attribute character attribute to be used as name identifiers.
#' @param weight_attribute character attribute to be used as weight.
#' @param override_factor numeric multiplier to use to override `name_attribute`.
#' See details.
#' @param status boolean if status updates should be printed.
#' @returns data.frame with id, levelpath_outlet_id, topo_sort, and levelpath columns.
#' See details for more info.
#' @details
#' The levelpath algorithm defines upstream mainstem paths through a network.
#' At a given junction with two or more upstream flowlines, the main path is
#' either 1) the path with the same name, 2) the path with any name, 3) or the
#' path with the larger weight. If the `weight_attribute` is `override_factor`
#' times larger on a path, it will be followed regardless of the name_attribute
#' indication.
#'
#' If id and toid are non-dendritic so id:toid is many to one and id is
#' non-unique, a divergence attribute must be included such that the dendritic
#' network can be extracted after the network is sorted.
#'
#' @name add_levelpaths
#' @export
#' @examples
#' g <- sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom"))
#'
#' test_flowline <- add_toids(g)
#'
#' # use NHDPlus attributes directly
#' add_levelpaths(test_flowline,
#'   name_attribute = "GNIS_ID",
#'   weight_attribute = "ArbolateSu")
#'
#' # use hy attributes where they can be mapped
#' add_levelpaths(hy(test_flowline),
#'   name_attribute = "GNIS_ID",
#'   weight_attribute = "arbolate_sum")
#'
add_levelpaths <- function(x, name_attribute, weight_attribute,
                           override_factor = NULL, status = FALSE) {

  if (any(!c(name_attribute, weight_attribute) %in% names(x))) {
    stop("name and weight attribute must be in x")
  }

  UseMethod("add_levelpaths")
}

#' @name add_levelpaths
#' @export
#'
add_levelpaths.data.frame <- function(x, name_attribute, weight_attribute,
                                      override_factor = NULL, status = FALSE) {
  x <- hy(x)

  orig_names <- attr(x, "orig_names")

  name_attribute <- align_name_char(name_attribute)
  weight_attribute <- align_name_char(weight_attribute)

  x <- add_levelpaths(x, name_attribute, weight_attribute, override_factor,
    status)

  attr(x, "orig_names") <- orig_names
  if (!inherits(x, "hy")) class(x) <- c("hy", class(x))

  hy_reverse(x)

}

#' @name add_levelpaths
#' @export
add_levelpaths.hy <- function(x, name_attribute, weight_attribute,
                              override_factor = NULL, status = FALSE) {
  hy_classify_and_redispatch(x, "add_levelpaths", "hy_topo", hy_guidance_topo,
    name_attribute = name_attribute, weight_attribute = weight_attribute,
    override_factor = override_factor, status = status)
}

#' @name add_levelpaths
#' @export
add_levelpaths.hy_node <- function(x, name_attribute, weight_attribute,
                                   override_factor = NULL, status = FALSE) {
  hy_node_to_topo(x, "add_levelpaths",
    name_attribute = name_attribute, weight_attribute = weight_attribute,
    override_factor = override_factor, status = status)
}

#' @name add_levelpaths
#' @export
add_levelpaths.hy_flownetwork <- function(x, name_attribute, weight_attribute,
                                          override_factor = NULL, status = FALSE) {
  add_levelpaths.hy_topo(x, name_attribute, weight_attribute,
    override_factor, status)
}

#' @name add_levelpaths
#' @export
add_levelpaths.hy_topo <- function(x, name_attribute, weight_attribute,
                                   override_factor = NULL, status = FALSE) {

  if (nrow(x) == 0) {
    x[[levelpath_outlet_id]] <- NA_character_
    x[[topo_sort]] <- NA_integer_
    x[[levelpath]] <- NA_integer_
    return(x)
  }

  req_atts <- required_atts_add_levelpaths
  check_names(x, req_atts, "add_levelpaths")

  if (length(unique(x$id)) != nrow(x)) {
    if (!divergence %in% names(x))
      stop(paste("Non unique ids found. A divergence attribute must be included",
        "if id is non-unique"))
    req_atts <- c(required_atts_add_levelpaths, divergence)
  }

  hy_g <- get_hyg(x, add = TRUE, id = id)

  orig_names <- attr(x, "orig_names")

  x <- st_drop_geometry(x)

  if (topo_sort %in% names(x)) {
    req_atts <- c(req_atts, topo_sort)
  }

  extra <- distinct(select(x, all_of(c(id, names(x)[!names(x) %in% req_atts]))))

  x <- select(x, all_of(c(req_atts,
    "lp_name_attribute" = name_attribute,
    "lp_weight_attribute" = weight_attribute))) |>
    distinct()

  out_val <- get_outlet_value(x)

  x$toid <- replace_na(x$toid, out_val)

  x[["lp_name_attribute"]] <- replace_na(x[["lp_name_attribute"]], " ") # NHDPlusHR uses NA for empty names.
  x[["lp_name_attribute"]][x[["lp_name_attribute"]] == "-1"] <- " "

  if (!topo_sort %in% names(x)) x <- add_topo_sort(x)

  if (divergence %in% names(x)) {
    divs <- x[[id]][x[[divergence]] > 1]

    x <- filter(x, !.data$toid %in% divs)
  }

  x$levelpath <- rep(0, nrow(x))

  # get downstream name id to identify name continuity at junctions
  x <- x |>
    left_join(st_drop_geometry(select(x, all_of(c("id", ds_nameid = "lp_name_attribute")))),
      by = c("toid" = "id")) |>
    mutate(ds_nameid = ifelse(is.na(.data$ds_nameid), " ", .data$ds_nameid))

  # Vectorized reweight: replaces per-junction split/apply/combine with a
  # single data.table grouped operation. For each toid group, rows are ranked
  # by name-match priority then weight, and assigned sequential integer weights.
  dt <- as.data.table(x)

  # priority determines junction routing order:
  #   1 = name matches downstream (highest priority, continues named path)
  #   2 = has a name but doesn't match downstream
  #   3 = unnamed (lowest priority)
  dt[, priority := fcase(
    lp_name_attribute == ds_nameid & ds_nameid != " ", 1L,
    lp_name_attribute != " ",                          2L,
    default = 3L
  )]

  if (!is.null(override_factor)) {
    # override_factor allows a much heavier unnamed path to outrank a named one:
    # inflate weight for name-matched rows, then sort purely by weight
    dt[priority == 1L,
      lp_weight_attribute := lp_weight_attribute * override_factor]
    setorderv(dt, c("toid", "lp_weight_attribute"), c(1L, -1L))
  } else {
    # standard: sort by name-match priority first, then by weight within bucket
    setorderv(dt, c("toid", "priority", "lp_weight_attribute"), c(1L, 1L, -1L))
  }

  # assign sequential weights within each junction group:
  # highest-priority row gets group size N, lowest gets 1
  dt[, lp_weight_attribute := as.numeric(rev(seq_len(.N))), by = toid]
  dt[, priority := NULL]

  x <- as_tibble(dt)
  x <- x[, c("id", "toid", "topo_sort", "levelpath",
    "lp_weight_attribute", "lp_name_attribute")]

  attr(x, "orig_names") <- orig_names
  class(x) <- c("hy", class(x))

  x <- arrange(x, .data$topo_sort)

  to_ind <- make_index_ids(x, mode = "both")
  from_ind <- to_ind$from
  to_ind <- to_ind$to

  x$ind <- seq_len(nrow(x))
  x$toind <- to_ind$to[1, ]

  # extract vectors once to avoid repeated data.frame column access in the loop
  weight_vec <- x[["lp_weight_attribute"]]
  topo_sort_vec <- x[["topo_sort"]]
  froms_mat <- from_ind$froms
  froms_len <- from_ind$lengths

  levelpath_result <- integer(nrow(x))
  done_vec <- logical(nrow(x))

  # start from network outlets and work upstream in waves
  outlet_inds <- which(x$toid == get_outlet_value(x))
  total_done <- 0L
  checker <- 0L

  if (status) {
    pb <- txtProgressBar(0, nrow(x), style = 3)
    on.exit(close(pb), add = TRUE)
  }

  while (length(outlet_inds) > 0L && checker < 10000000L) {

    all_path_inds <- integer(0L)

    for (oid in outlet_inds) {
      path_inds <- get_path_vec(weight_vec, oid, froms_mat, froms_len)

      # levelpath value = topo_sort of the outlet (most downstream node)
      levelpath_result[path_inds] <- topo_sort_vec[oid]
      done_vec[path_inds] <- TRUE
      total_done <- total_done + length(path_inds)
      all_path_inds <- c(all_path_inds, path_inds)
    }

    # find upstream tributaries branching off the paths just traced
    next_outlets <- na.omit(as.integer(froms_mat[, all_path_inds]))
    next_outlets <- next_outlets[!next_outlets %in% all_path_inds]
    outlet_inds <- unique(next_outlets[!done_vec[next_outlets]])

    checker <- checker + 1L

    if (status) setTxtProgressBar(pb, total_done)
  }

  x$levelpath <- levelpath_result
  x$done <- done_vec

  x <- add_levelpath_outlet_ids(x)

  x <- put_hyg(x, hy_g)

  x <- select(x, all_of(c(id, toid, levelpath_outlet_id, topo_sort, levelpath)),
    !all_of(c("done", "lp_name_attribute", "lp_weight_attribute", "ind", "toind")))

  x <- left_join(x, select(extra, -any_of(c("levelpath_outlet_id", "topo_sort", "levelpath"))),
    by = id)

  classify_hy(x)

}

add_levelpath_outlet_ids <- function(x) {
  left_join(x, st_drop_geometry(x) |>
    group_by(.data$levelpath) |>
    filter(.data$topo_sort == min(.data$topo_sort)) |>
    ungroup() |>
    select(levelpath_outlet_id = "id", "levelpath"),
  by = "levelpath")
}

#' Trace upstream path following maximum weight
#' @description Walks upstream from a starting node through the adjacency
#' matrix, always following the upstream neighbor with the highest weight.
#' Uses direct vector/matrix indexing instead of data.frame subsetting
#' for performance on large networks.
#' @param weight_vec numeric vector of weights indexed by node position
#' @param tailid integer starting node index
#' @param froms_mat integer matrix from make_index_ids (columns = nodes,
#'   rows = upstream connections, NA for empty slots)
#' @param froms_len integer vector of upstream connection counts per node
#' @returns integer vector of node indices along the traced path
#' @importFrom data.table fcase setorderv
#' @noRd
get_path_vec <- function(weight_vec, tailid, froms_mat, froms_len) {

  tracker <- integer(256L)
  counter <- 1L

  repeat {
    if (counter > length(tracker)) {
      # double capacity (amortized O(1) growth)
      tracker <- c(tracker, integer(length(tracker)))
    }
    tracker[counter] <- tailid
    counter <- counter + 1L

    n_from <- froms_len[tailid]
    if (n_from == 0L) break

    if (n_from == 1L) {
      next_id <- froms_mat[1L, tailid]
      if (is.na(next_id)) break
      tailid <- next_id
    } else {
      upstream <- froms_mat[seq_len(n_from), tailid]
      upstream <- upstream[!is.na(upstream)]
      if (length(upstream) == 0L) break
      tailid <- upstream[which.max(weight_vec[upstream])]
    }
  }

  tracker[seq_len(counter - 1L)]
}
