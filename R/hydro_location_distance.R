utils::globalVariables(c(".ord", "enters", "from_km", "n_cand", "n_enters",
  "prev_id", "ref_km", "to_km"))

##### hydro_location_distance.R -- distances from confluences along mainstems #####
# The aggregate flowpath axis and the three exported functions it underlies.
# Every position and hydro location on an aggregate_id is a kilometer value on
# the axis, so the work is arithmetic on the axis rather than coordinate
# geometry. The mainstem referencing article walks the design.

#' Link to the next flowline down the aggregate flowpath for hy_topo input
#' @description For each row, the id of the next flowline down within the
#' same mainstem: toid where the row's toid points to a flowline with the
#' same aggregate_id, NA otherwise.
#' @param x data.frame with id, toid, aggregate_id
#' @returns vector of links parallel to x, NA at the downstream end of each
#' aggregate flowpath
#' @noRd
aggregate_flowpath_link_topo <- function(x) {

  agg <- x$aggregate_id

  m <- match(x$toid, x$id)

  same <- !is.na(m) & !is.na(agg) & !is.na(agg[m]) & agg == agg[m]

  link <- x$toid
  link[!same] <- NA

  link
}

#' Link to the next flowline down the aggregate flowpath for hy_node input
#' @description For each row, the id of the row on the same aggregate
#' flowpath whose
#' fromnode equals this row's tonode: a join on (aggregate_id, tonode) to
#' (aggregate_id, fromnode).
#' @param x data.frame with id, fromnode, tonode, aggregate_id
#' @returns vector of links parallel to x, NA at the downstream end of each
#' aggregate flowpath
#' @noRd
aggregate_flowpath_link_node <- function(x) {

  key_down <- paste(x$aggregate_id, x$tonode)
  key_up   <- paste(x$aggregate_id, x$fromnode)

  m <- match(key_down, key_up)

  link <- x$id[m]
  link[is.na(x$aggregate_id)] <- NA

  link
}

#' Build the aggregate flowpath axis
#' @description The ordered flowlines of each identified mainstem, with
#' cumulative kilometers from the bottom flowline present. See the mainstem
#' referencing article.
#' @param x hy_topo (id, toid, aggregate_id, length_km) or hy_node
#' (fromnode, tonode in place of toid). Rows with NA aggregate_id are
#' dropped. If length_km is absent and x is sf, st_length() supplies it.
#' @returns tibble with id, aggregate_id, topo_sort, length_km, link,
#' from_km, to_km, one row per identified flowline, ordered bottom first
#' within each mainstem.
#' @noRd
aggregate_flowpath_axis <- function(x) {

  if (inherits(x, "hy_flownetwork"))
    stop("aggregate_flowpath_axis() needs a unique link per flowline; a hy_flownetwork ",
      "has a duplicated id where one flowline has two downstream rows. ",
      "Drop divergence == 2 rows first.", call. = FALSE)

  check_names(x, aggregate_id, "aggregate_flowpath_axis")

  if (!length_km %in% names(x)) {
    if (inherits(x, "sf")) {
      x[[length_km]] <- as.numeric(units::set_units(sf::st_length(x),
        "km", mode = "standard"))
    } else {
      stop("aggregate_flowpath_axis() requires length_km, or an sf x to measure it from.",
        call. = FALSE)
    }
  }

  if (inherits(x, "hy_node")) {
    link <- aggregate_flowpath_link_node(x)
  } else {
    link <- aggregate_flowpath_link_topo(x)
  }

  keep <- c(id, aggregate_id, length_km, intersect(topo_sort, names(x)))
  axis <- as_tibble(sf::st_drop_geometry(x[, keep]))
  axis$link <- link

  axis <- axis[!is.na(axis$aggregate_id), ]

  if (!topo_sort %in% names(axis)) {

    sort_in <- data.frame(id = axis$id, toid = axis$link)
    sort_in$toid[is.na(sort_in$toid)] <- get_outlet_value(sort_in)

    sorted <- sort_network_impl(sort_in)

    axis$topo_sort <- match(axis$id, rev(sorted$id))
  }

  axis <- arrange(axis, .data$aggregate_id, .data$topo_sort)

  axis <- mutate(group_by(axis, .data$aggregate_id),
    from_km = cumsum(lag(.data$length_km, default = 0)),
    to_km = .data$from_km + .data$length_km)

  axis <- ungroup(axis)

  broken <- check_axis_continuity(axis)

  if (length(broken) > 0)
    stop("the aggregate flowpath axis is not continuous on aggregate_id(s): ",
      paste(broken, collapse = ", "),
      ".\n  Each flowline must link to the one below it on its aggregate ",
      "flowpath. A missing flowline, a toid down a minor path, one ",
      "mainstem id on both branches of a divergence, or two runs given ",
      "one id breaks this. Fix how x was assembled.", call. = FALSE)

  axis
}

#' Continuity check over an ordered aggregate flowpath axis
#' @description After ordering bottom first, each flowline's link to the
#' next flowline down the aggregate flowpath must be the id of the flowline
#' before it, the bottom row excepted (link NA). One shifted comparison over
#' the whole axis, the shift taken within each aggregate_id, vectorized over
#' all rows.
#' @param axis ordered aggregate flowpath axis (grouped output of
#' aggregate_flowpath_axis, ungrouped)
#' @returns character vector of aggregate_ids that fail the check
#' @noRd
check_axis_continuity <- function(axis) {

  dt <- data.table::data.table(aggregate_id = axis$aggregate_id,
    id = axis$id, link = axis$link)

  # downstream end of each aggregate flowpath: no row before it, so prev_id
  # and link are NA; every other row's link must equal the id of the row
  # before it. The axis
  # is already ordered bottom first within each mainstem, so the shift down
  # one within aggregate_id is the row before.
  dt[, prev_id := data.table::shift(id, 1L), by = "aggregate_id"]

  ok <- (is.na(dt$prev_id) & is.na(dt$link)) |
    (!is.na(dt$prev_id) & !is.na(dt$link) & dt$link == dt$prev_id)

  unique(axis$aggregate_id[!ok])
}


#' Get confluences of aggregate flowpaths
#'
#' @description Returns a hydro location for each confluence where one
#' aggregate flowpath flows into another, such as a tributary mainstem joining
#' the mainstem it drains to. An aggregate flowpath is the run of flowlines
#' that share an `aggregate_id` and link end to end. Each confluence is
#' identified by the tributary's `aggregate_id` and placed on the first
#' flowline of the receiving aggregate flowpath below it.
#'
#' Confluences work as reference points for distances along a river because
#' any network that carries both mainstems has the confluence too. Pass the
#' result to [add_hydro_location_distance()] to record positions as distances
#' from these confluences, and to [index_hydro_location_distance()] to place
#' those distances on the same or another network.
#'
#' @details
#' A confluence is found where the bottom flowline of one aggregate flowpath
#' connects downstream to a flowline with a different `aggregate_id`. The
#' hydro location goes at the upstream end (`id_measure` 100) of that
#' downstream flowline rather than at the end of the tributary. That keeps it
#' on the receiving aggregate flowpath, and it still exists when the receiving
#' flowpath is truncated just above the confluence. Only topology is used; no
#' coordinates are read.
#'
#' Each aggregate flowpath has one bottom flowline, so it yields at most one
#' confluence. A tributary whose downstream connection is a diversion
#' (`divergence == 2`) is dropped with a warning.
#'
#' The output follows the `hydrolocation` feature type of OGC 25-045, with
#' `aggregate_id` in the role of `mainstem_id` and `hydro_location_id` in the
#' role of `link`.
#'
#' @param x `hy_topo` or `hy_node`, or a data.frame coercible with [hy()].
#' `aggregate_id` is set on the flowlines of each aggregate flowpath and `NA`
#' elsewhere. A `topo_sort` column keeps building the aggregate flowpath axis
#' cheap; without it a full topological sort is run.
#' @returns data.frame with one row per confluence: `aggregate_id` (the
#' receiving aggregate flowpath), `hydro_location_id` (the tributary's
#' `aggregate_id`), `id` (the receiving flowline just below the confluence)
#' and `id_measure` (always 100, the upstream end of that flowline).
#' @inheritSection index_points_to_lines Measures
#' @name get_confluences
#' @export
#' @examples
#' # New Hope flowlines, with ref/mainstems URIs joined on as aggregate_id
#' g <- sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom"))
#' csv <- utils::read.csv(
#'   system.file("extdata/new_hope_mainstems.csv", package = "hydroloom"))
#'
#' g$mainstemid <- csv$mainstem_uri[match(g$COMID, csv$comid)]
#' g$REACHCODE <- NULL
#'
#' net <- add_toids(hy(g))
#'
#' confluences <- get_confluences(net)
#'
#' head(confluences)
get_confluences <- function(x) {
  UseMethod("get_confluences")
}

#' @name get_confluences
#' @export
get_confluences.data.frame <- function(x) {
  hy_as_dataframe(x, "get_confluences")
}

#' @name get_confluences
#' @export
get_confluences.hy <- function(x) {
  hy_classify_and_redispatch(x, "get_confluences", "hy_topo or hy_node",
    hy_guidance_topo)
}

#' @name get_confluences
#' @export
get_confluences.hy_topo <- function(x) {

  # the axis's continuity check makes the bottom flowline unique per
  # mainstem, so each tributary yields at most one confluence
  aggregate_flowpath_axis(x)

  agg <- x$aggregate_id
  m <- match(x$toid, x$id)

  rows <- !is.na(m) & !is.na(agg) & !is.na(agg[m]) & agg != agg[m]

  if (divergence %in% names(x)) {
    minor <- !is.na(x$divergence[m]) & x$divergence[m] == 2
    if (any(rows & minor))
      warning("tributary flowline(s) ", paste(x$id[rows & minor], collapse = ", "),
        " connect down a divergence (divergence == 2); dropped.",
        call. = FALSE)
    rows <- rows & !minor
  }

  get_confluences_rows(agg[m[rows]], agg[rows], x$id[m[rows]])
}

#' @name get_confluences
#' @export
get_confluences.hy_node <- function(x) {

  # the axis's continuity check makes the bottom flowline unique per
  # mainstem; the axis already carries the link to the next flowline down
  # the aggregate flowpath, so map it
  # back to x by id rather than recomputing it
  axis <- aggregate_flowpath_axis(x)

  # downstream end of each aggregate flowpath (link NA). The axis
  # dropped NA-aggregate_id rows and reordered, so index into it by id.
  link <- axis$link[match(x$id, axis$id)]
  bottoms <- !is.na(x$aggregate_id) & is.na(link)

  # tributary bottoms, kept in order so the output and any warning follow it;
  # r_agg is a character join/group key, r_idx indexes back into x so output
  # columns keep the uncoerced aggregate_id type
  trib <- data.table::data.table(
    .ord = seq_len(sum(bottoms)),
    r_agg = as.character(x$aggregate_id[bottoms]),
    r_node = x$tonode[bottoms],
    r_idx = which(bottoms))

  # candidates: identified flowlines starting at a node; c_agg is a character
  # join key, c_idx indexes back into x for the output
  ident <- !is.na(x$aggregate_id)
  cand <- data.table::data.table(
    c_agg = as.character(x$aggregate_id[ident]),
    c_node = x$fromnode[ident],
    c_idx = which(ident))

  # a candidate's mainstem enters the node when its (aggregate_id, node)
  # appears as a (aggregate_id, tonode) among the identified rows
  enters_key <- paste(x$aggregate_id[ident], x$tonode[ident])

  # one join of bottoms to candidates on node = fromnode, dropping a
  # candidate on the tributary's own mainstem
  j <- cand[trib, on = c(c_node = "r_node"), allow.cartesian = TRUE,
    nomatch = NULL]
  j <- j[j$c_agg != j$r_agg, ]

  conf <- get_confluences_rows(character(0), character(0), x$id[0])

  if (nrow(j) > 0) {

    j$enters <- paste(j$c_agg, j$c_node) %in% enters_key

    # per tributary: the single entering candidate when exactly one enters;
    # otherwise the sole candidate when there is exactly one; more than one
    # candidate with no single entering receiver is ambiguous and dropped
    j[, n_cand := .N, by = ".ord"]
    j[, n_enters := sum(enters), by = ".ord"]

    pick <- (j$n_enters == 1 & j$enters) |
      (j$n_enters != 1 & j$n_cand == 1)

    drop <- j$n_enters != 1 & j$n_cand > 1

    ambiguous <- unique(x$aggregate_id[j$r_idx[drop]][order(j$.ord[drop])])
    if (length(ambiguous) > 0)
      warning(paste0("tributary ", ambiguous, " enters a node with more ",
        "than one candidate receiver; dropped.", collapse = "\n"),
        call. = FALSE)

    sel <- j[pick, ]
    data.table::setorder(sel, .ord)

    conf <- get_confluences_rows(x$aggregate_id[sel$c_idx],
      x$aggregate_id[sel$r_idx], x$id[sel$c_idx])
  }

  conf
}

#' Assemble the confluence output table
#' @noRd
get_confluences_rows <- function(aggregate_id, hydro_location_id, id) {

  tibble(aggregate_id = aggregate_id,
    hydro_location_id = hydro_location_id,
    id = id,
    id_measure = rep(100, length(id)))
}

#' Kilometer value of positions on an aggregate flowpath axis
#' @description A position (id, id_measure) is at from_km + length_km *
#' id_measure / 100. Joins by id; positions whose id is not on the axis
#' get NA.
#' @param id vector of flowline ids
#' @param id_measure vector of interpolative measures (0-100)
#' @param axis aggregate flowpath axis
#' @returns list with km (numeric) and aggregate_id (the mainstem each id
#' is on), parallel to id
#' @noRd
position_to_km <- function(id, id_measure, axis) {

  m <- match(id, axis$id)

  km <- axis$from_km[m] + axis$length_km[m] * id_measure / 100

  list(km = km, aggregate_id = axis$aggregate_id[m])
}

#' Map kilometer values on an aggregate flowpath axis back to flowline and
#' measure
#' @description For each (aggregate_id, km), the flowline whose interval
#' holds km and the measure within it. A km below the bottom or above the
#' top of the flowlines present is placed at that end (measure 0 or 100)
#' and the overshoot returned as past_end_km.
#' @param aggregate_id vector of mainstem ids
#' @param km vector of kilometer values
#' @param axis aggregate flowpath axis
#' @returns tibble with id, id_measure, past_end_km, status parallel to km
#' @noRd
km_to_position <- function(aggregate_id, km, axis) {

  n <- length(km)

  axis_dt <- as.data.table(axis)

  # per-aggregate-flowpath ends, for the past-end branches
  ends <- axis_dt[, list(
    bottom_id = id[which.min(from_km)],
    bottom_from = min(from_km),
    top_id = id[which.max(to_km)],
    top_to = max(to_km)),
    by = "aggregate_id"]

  ei <- match(as.character(aggregate_id), as.character(ends$aggregate_id))
  e_bottom_id <- ends$bottom_id[ei]
  e_bottom_from <- ends$bottom_from[ei]
  e_top_id <- ends$top_id[ei]
  e_top_to <- ends$top_to[ei]

  # rolling join to the flowline whose interval holds km: within each aggregate_id
  # the last row with from_km <= km (roll = +Inf). An exact interval
  # boundary resolves to the lower interval (a confluence at measure 100
  # stays on the flowline below it), so nudge the join key down by 1e-9 to
  # exclude the next interval's from_km; a km at the downstream end then misses
  # and is filled from the bottom row below. See the mainstem referencing
  # article.
  look <- axis_dt[, list(aggregate_id, from_km,
    match_id = id, match_from = from_km, match_len = length_km)]
  data.table::setkey(look, aggregate_id, from_km)

  q <- data.table::data.table(.row = seq_len(n),
    aggregate_id = as.character(aggregate_id),
    from_km = km - 1e-9)

  hit <- look[q, on = c("aggregate_id", "from_km"), roll = Inf,
    mult = "first"]
  data.table::setorder(hit, .row)

  match_id <- hit$match_id
  match_from <- hit$match_from
  match_len <- hit$match_len

  out_id <- vector(mode = class(axis$id), length = n)
  out_id[] <- NA
  out_meas <- rep(NA_real_, n)
  past <- rep(0, n)
  status <- rep("placed", n)

  in_aggregate_flowpath <- !is.na(e_bottom_id) & !is.na(km)
  status[!in_aggregate_flowpath] <- "hydro_location_not_in_x"

  below <- in_aggregate_flowpath & km < e_bottom_from
  above <- in_aggregate_flowpath & km > e_top_to
  inside <- in_aggregate_flowpath & !below & !above

  out_id[below] <- e_bottom_id[below]
  out_meas[below] <- 0
  past[below] <- e_bottom_from[below] - km[below]
  status[below] <- "past_end"

  out_id[above] <- e_top_id[above]
  out_meas[above] <- 100
  past[above] <- km[above] - e_top_to[above]
  status[above] <- "past_end"

  # km exactly at the downstream end's from_km falls just below the
  # down-nudged join key and misses; it belongs to the bottom interval
  bottom_hit <- inside & is.na(match_id)
  match_id[bottom_hit] <- e_bottom_id[bottom_hit]
  match_from[bottom_hit] <- e_bottom_from[bottom_hit]
  match_len[bottom_hit] <- axis$length_km[match(e_bottom_id[bottom_hit], axis$id)]

  out_id[inside] <- match_id[inside]
  out_meas[inside] <- (km[inside] - match_from[inside]) /
    match_len[inside] * 100

  tibble(id = out_id, id_measure = out_meas, past_end_km = past,
    status = status)
}

#' Add distance from the nearest hydro location
#'
#' @description Records each position as a signed distance along its
#' aggregate flowpath from the nearest of a set of hydro locations, such as
#' the confluences returned by [get_confluences()]. An aggregate flowpath is
#' the run of flowlines that share an `aggregate_id` and link end to end, such
#' as an identified mainstem. Each position is a flowline `id` and
#' `id_measure`. The result adds the `aggregate_id` the position is on, the
#' `hydro_location_id` it is measured from, and `distance_km`.
#'
#' A distance from a confluence does not depend on the flowline ids of the
#' network it was recorded on. [index_hydro_location_distance()] takes the
#' same record and places it on any network that carries the same aggregate
#' flowpaths, for example to move a gage location from NHDPlusV2 to 3DHP.
#'
#' @details
#' Positions and hydro locations are converted to kilometers along the
#' aggregate flowpath axis (the aggregate flowpath measured from its
#' downstream end in `x`). Only hydro locations on the same `aggregate_id` as
#' a position are candidates, and the one with the smallest absolute
#' difference in kilometers is recorded. A position exactly halfway between
#' two hydro locations takes the one with the smaller `hydro_location_id`,
#' compared as strings.
#'
#' `distance_km` is negative when the position is downstream of the hydro
#' location (toward the outlet) and positive when it is upstream (toward the
#' headwater). The sign is a hydroloom convention; HY_DistanceDescription in
#' HY_Features describes the same two directions as downstream and upstream.
#'
#' @param indexes data.frame. Positions with `id` and `id_measure`, on `x`.
#' @param hydro_locations data.frame. Hydro locations to measure from, with
#' `hydro_location_id`, `id` and `id_measure`, on `x` (for example the output
#' of [get_confluences()]).
#' @param x `hy_topo` or `hy_node`, or a data.frame coercible with [hy()].
#' Carries `aggregate_id` and `length_km`. A `topo_sort` column keeps building
#' the aggregate flowpath axis cheap; without it a full topological sort is
#' run.
#' @returns `indexes` with `aggregate_id`, `hydro_location_id` and
#' `distance_km` added. `aggregate_id` is `NA` for a position on a flowline
#' without one. A position on an aggregate flowpath with no hydro location
#' gets `NA` `hydro_location_id` and `distance_km`, with a message.
#' @inheritSection index_points_to_lines Measures
#' @name add_hydro_location_distance
#' @export
#' @examples
#' g <- sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom"))
#' csv <- utils::read.csv(
#'   system.file("extdata/new_hope_mainstems.csv", package = "hydroloom"))
#'
#' g$mainstemid <- csv$mainstem_uri[match(g$COMID, csv$comid)]
#' g$REACHCODE <- NULL
#'
#' net <- add_toids(hy(g))
#'
#' confluences <- get_confluences(net)
#'
#' # the midpoint of every identified flowline, as a distance from its
#' # nearest confluence
#' positions <- data.frame(id = net$id[!is.na(net$aggregate_id)],
#'   id_measure = 50)
#'
#' recorded <- add_hydro_location_distance(positions, confluences, net)
#'
#' head(recorded)
add_hydro_location_distance <- function(indexes, hydro_locations, x) {
  UseMethod("add_hydro_location_distance", x)
}

#' @name add_hydro_location_distance
#' @export
add_hydro_location_distance.data.frame <- function(indexes, hydro_locations, x) {
  add_hydro_location_distance(indexes, hydro_locations, hy(x))
}

#' @name add_hydro_location_distance
#' @export
add_hydro_location_distance.hy <- function(indexes, hydro_locations, x) {
  hy_classify_and_redispatch(x, "add_hydro_location_distance",
    "hy_topo or hy_node", hy_guidance_topo,
    indexes = indexes, hydro_locations = hydro_locations)
}

#' @name add_hydro_location_distance
#' @export
add_hydro_location_distance.hy_topo <- function(indexes, hydro_locations, x) {
  add_hydro_location_dist_impl(indexes, hydro_locations, aggregate_flowpath_axis(x))
}

#' @name add_hydro_location_distance
#' @export
add_hydro_location_distance.hy_node <- function(indexes, hydro_locations, x) {
  add_hydro_location_dist_impl(indexes, hydro_locations, aggregate_flowpath_axis(x))
}

#' Shared body for add_hydro_location_distance
#' @noRd
add_hydro_location_dist_impl <- function(indexes, hydro_locations, axis) {

  pos <- position_to_km(indexes$id, indexes$id_measure, axis)
  ref <- position_to_km(hydro_locations$id, hydro_locations$id_measure, axis)

  ref_tbl <- tibble(aggregate_id = ref$aggregate_id,
    hydro_location_id = hydro_locations$hydro_location_id,
    ref_km = ref$km)

  ref_tbl <- ref_tbl[!is.na(ref_tbl$aggregate_id) & !is.na(ref_tbl$ref_km), ]

  n <- length(pos$km)

  pos_dt <- data.table::data.table(.row = seq_len(n),
    aggregate_id = as.character(pos$aggregate_id),
    pos_km = pos$km)
  pos_dt <- pos_dt[!is.na(pos_dt$aggregate_id) & !is.na(pos_dt$pos_km), ]

  ref_dt <- data.table::as.data.table(ref_tbl)
  ref_dt$aggregate_id <- as.character(ref_dt$aggregate_id)
  ref_dt$hl_str <- as.character(ref_dt$hydro_location_id)
  # the rolling join overwrites its roll column (ref_km) with the position's
  # km, so keep the hydro location km under a second column to recover the
  # distance
  ref_dt$ref_km_val <- ref_dt$ref_km

  hl <- rep(NA_character_, n)
  dist <- rep(NA_real_, n)

  if (nrow(ref_dt) > 0 && nrow(pos_dt) > 0) {

    # hydro locations sorted so coincident ones put the smaller
    # hydro_location_id string first; the rolling joins below keep the first
    # match, so a tie resolves to the smaller string. See the mainstem
    # referencing article.
    data.table::setorderv(ref_dt, c("aggregate_id", "ref_km", "hl_str"))
    data.table::setkey(ref_dt, aggregate_id, ref_km)

    q <- data.table::data.table(.row = pos_dt$.row,
      aggregate_id = pos_dt$aggregate_id, ref_km = pos_dt$pos_km)

    # nearest hydro location at or below each position, and at or above; pick
    # the smaller absolute difference, ties already broken by the sort. A
    # single roll = "nearest" would break an equidistant tie to the lower key,
    # not to the smaller hydro_location_id string.
    below <- ref_dt[q, on = c("aggregate_id", "ref_km"), roll = Inf,
      mult = "first"]
    above <- ref_dt[q, on = c("aggregate_id", "ref_km"), roll = -Inf,
      mult = "first"]

    data.table::setorder(below, .row)
    data.table::setorder(above, .row)

    pos_km <- pos_dt$pos_km
    d_below <- pos_km - below$ref_km_val
    d_above <- pos_km - above$ref_km_val

    # smaller absolute difference wins; an exact tie between a hydro location
    # below and one above takes the smaller hydro_location_id string
    tie <- !is.na(d_below) & !is.na(d_above) & abs(d_below) == abs(d_above)
    tie_below <- tie & below$hl_str <= above$hl_str

    take_below <- !is.na(d_below) &
      (is.na(d_above) | (abs(d_below) < abs(d_above)) | tie_below)

    best_hl <- ifelse(take_below, below$hydro_location_id,
      above$hydro_location_id)
    best_d <- ifelse(take_below, d_below, d_above)

    hl[pos_dt$.row] <- best_hl
    dist[pos_dt$.row] <- best_d
  }

  if (any(is.na(hl) & !is.na(pos$aggregate_id)))
    message("some positions are on a mainstem with no hydro location; ",
      "distance_km is NA there.")

  indexes[[aggregate_id]] <- pos$aggregate_id
  indexes[[hydro_location_id]] <- hl
  indexes[[distance_km]] <- dist

  indexes
}

#' Index a distance from a hydro location to a flowline
#'
#' @description Places distances from hydro locations on the flowlines of
#' `x`, the inverse of [add_hydro_location_distance()]. Each record gives an
#' `aggregate_id`, a `hydro_location_id` and a `distance_km`, and the result
#' adds the flowline `id` and `id_measure` at that distance. An aggregate
#' flowpath is the run of flowlines that share an `aggregate_id` and link end
#' to end, such as an identified mainstem.
#'
#' Because a record points to a hydro location rather than to a flowline, `x`
#' does not have to be the network the distance was recorded on. A distance
#' recorded on NHDPlusV2 can be placed on 3DHP, as long as both carry the same
#' `aggregate_id` values and `hydro_locations` is on `x`, for example
#' `get_confluences(x)`.
#'
#' @details
#' Each record's hydro location is converted to kilometers along the
#' aggregate flowpath axis of `x` (the aggregate flowpath measured from its
#' downstream end), `distance_km` is added, and the flowline holding the
#' result is found. A result that lands on a boundary between two flowlines
#' goes to the lower one, at `id_measure` 100. `distance_km` uses the sign
#' convention of [add_hydro_location_distance()]: negative downstream,
#' positive upstream.
#'
#' `status` reports what happened to each record. `placed` is a normal
#' result. `hydro_location_not_in_x` means the `aggregate_id` and
#' `hydro_location_id` pair is not in `hydro_locations`, or that hydro
#' location is not on `x`, and `id` and `id_measure` are `NA`. `past_end`
#' means the distance runs past the downstream or upstream end of the
#' aggregate flowpath's flowlines in `x` (a truncated network, for example).
#' The record is placed at that end, at `id_measure` 0 or 100, and
#' `past_end_km` gives how far past the end it reached.
#'
#' @param positions data.frame. Records with `aggregate_id`,
#' `hydro_location_id` and `distance_km`.
#' @param hydro_locations data.frame. Hydro locations to measure from, with
#' `aggregate_id`, `hydro_location_id`, `id` and `id_measure`, on `x` (for
#' example the output of `get_confluences(x)`).
#' @param x `hy_topo` or `hy_node`, or a data.frame coercible with [hy()].
#' Carries `aggregate_id` and `length_km`. A `topo_sort` column keeps building
#' the aggregate flowpath axis cheap; without it a full topological sort is
#' run.
#' @returns `positions` with `id`, `id_measure`, `past_end_km` and `status`
#' added. `status` is `placed`, `hydro_location_not_in_x` or `past_end`, as
#' described in Details.
#' @inheritSection index_points_to_lines Measures
#' @name index_hydro_location_distance
#' @export
#' @examples
#' g <- sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom"))
#' csv <- utils::read.csv(
#'   system.file("extdata/new_hope_mainstems.csv", package = "hydroloom"))
#'
#' g$mainstemid <- csv$mainstem_uri[match(g$COMID, csv$comid)]
#' g$REACHCODE <- NULL
#'
#' net <- add_toids(hy(g))
#'
#' confluences <- get_confluences(net)
#'
#' # 0.5 km upstream of each confluence, back to a flowline id and measure
#' records <- data.frame(
#'   aggregate_id = confluences$aggregate_id,
#'   hydro_location_id = confluences$hydro_location_id,
#'   distance_km = 0.5)
#'
#' index_hydro_location_distance(records, confluences, net)
index_hydro_location_distance <- function(positions, hydro_locations, x) {
  UseMethod("index_hydro_location_distance", x)
}

#' @name index_hydro_location_distance
#' @export
index_hydro_location_distance.data.frame <- function(positions, hydro_locations, x) {
  index_hydro_location_distance(positions, hydro_locations, hy(x))
}

#' @name index_hydro_location_distance
#' @export
index_hydro_location_distance.hy <- function(positions, hydro_locations, x) {
  hy_classify_and_redispatch(x, "index_hydro_location_distance",
    "hy_topo or hy_node", hy_guidance_topo,
    positions = positions, hydro_locations = hydro_locations)
}

#' @name index_hydro_location_distance
#' @export
index_hydro_location_distance.hy_topo <- function(positions, hydro_locations, x) {
  index_hydro_location_dist_impl(positions, hydro_locations, aggregate_flowpath_axis(x))
}

#' @name index_hydro_location_distance
#' @export
index_hydro_location_distance.hy_node <- function(positions, hydro_locations, x) {
  index_hydro_location_dist_impl(positions, hydro_locations, aggregate_flowpath_axis(x))
}

#' Shared body for index_hydro_location_distance
#' @noRd
index_hydro_location_dist_impl <- function(positions, hydro_locations, axis) {

  ref <- position_to_km(hydro_locations$id, hydro_locations$id_measure, axis)

  ref_tbl <- tibble(aggregate_id = hydro_locations$aggregate_id,
    hydro_location_id = hydro_locations$hydro_location_id,
    ref_km = ref$km)

  key_in  <- paste(positions$aggregate_id, positions$hydro_location_id)
  key_ref <- paste(ref_tbl$aggregate_id, ref_tbl$hydro_location_id)

  m <- match(key_in, key_ref)
  ref_km <- ref_tbl$ref_km[m]

  target_km <- ref_km + positions$distance_km

  placed <- km_to_position(positions$aggregate_id, target_km, axis)

  # a hydro location that is not in x gives hydro_location_not_in_x,
  # overriding the km mapping's own status
  missing_ref <- is.na(ref_km)
  placed$status[missing_ref] <- "hydro_location_not_in_x"
  placed$id[missing_ref] <- NA
  placed$id_measure[missing_ref] <- NA
  placed$past_end_km[missing_ref] <- 0

  positions[[id]] <- placed$id
  positions[[id_measure]] <- placed$id_measure
  positions[[past_end_km]] <- placed$past_end_km
  positions[["status"]] <- placed$status

  positions
}
