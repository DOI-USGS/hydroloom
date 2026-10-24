##### hydro_location_distance.R -- referent distances along mainstems #####
# The chain frame and the three exported functions it underlies. Nothing
# here reads coordinates. See the "Internal: the chain frame" and
# "Step 3. Referent distances" sections of the mainstem translation
# workplan for the design.

#' Within-chain downstream link for a hy_topo frame
#' @description For each row, the id of the next flowline down within the
#' same mainstem: toid where the row toid names has the same aggregate_id,
#' NA otherwise.
#' @param x data.frame with id, toid, aggregate_id
#' @returns vector of links parallel to x, NA at chain bottoms
#' @noRd
chain_link_topo <- function(x) {

  agg <- x$aggregate_id

  m <- match(x$toid, x$id)

  same <- !is.na(m) & !is.na(agg) & !is.na(agg[m]) & agg == agg[m]

  link <- x$toid
  link[!same] <- NA

  link
}

#' Within-chain downstream link for a hy_node frame
#' @description For each row, the id of the row in the same chain whose
#' fromnode equals this row's tonode: a join on (aggregate_id, tonode) to
#' (aggregate_id, fromnode).
#' @param x data.frame with id, fromnode, tonode, aggregate_id
#' @returns vector of links parallel to x, NA at chain bottoms
#' @noRd
chain_link_node <- function(x) {

  key_down <- paste(x$aggregate_id, x$tonode)
  key_up   <- paste(x$aggregate_id, x$fromnode)

  m <- match(key_down, key_up)

  link <- x$id[m]
  link[is.na(x$aggregate_id)] <- NA

  link
}

#' Build the chain frame
#' @description The ordered flowlines of each identified mainstem, with
#' cumulative kilometers from the bottom flowline present. Reads no
#' geometry. See the "Internal: the chain frame" section of the workplan.
#' @param x hy_topo (id, toid, aggregate_id, length_km) or hy_node
#' (fromnode, tonode in place of toid). Rows with NA aggregate_id are
#' dropped. If length_km is absent and x is sf, st_length() supplies it.
#' @returns tibble with id, aggregate_id, topo_sort, length_km, link,
#' from_km, to_km, one row per identified flowline, ordered bottom first
#' within each mainstem.
#' @noRd
chain_frame <- function(x) {

  if (inherits(x, "hy_flownetwork"))
    stop("chain_frame() needs a unique link per flowline; a hy_flownetwork ",
      "has a duplicated id where one flowline has two downstream rows. ",
      "Drop divergence == 2 rows first.", call. = FALSE)

  check_names(x, aggregate_id, "chain_frame")

  if (!length_km %in% names(x)) {
    if (inherits(x, "sf")) {
      x[[length_km]] <- as.numeric(units::set_units(sf::st_length(x),
        "km", mode = "standard"))
    } else {
      stop("chain_frame() requires length_km, or an sf x to measure it from.",
        call. = FALSE)
    }
  }

  if (inherits(x, "hy_node")) {
    link <- chain_link_node(x)
  } else {
    link <- chain_link_topo(x)
  }

  fr <- as_tibble(sf::st_drop_geometry(x))
  fr$link <- link

  fr <- fr[!is.na(fr$aggregate_id), c(id, aggregate_id, length_km, "link",
    intersect(topo_sort, names(fr)))]

  if (!topo_sort %in% names(fr)) {

    sort_in <- data.frame(id = fr$id, toid = fr$link)
    sort_in$toid[is.na(sort_in$toid)] <- get_outlet_value(sort_in)

    sorted <- sort_network_impl(sort_in)

    fr$topo_sort <- match(fr$id, rev(sorted$id))
  }

  fr <- arrange(group_by(fr, .data$aggregate_id), .data$topo_sort)

  fr <- mutate(fr,
    from_km = cumsum(lag(.data$length_km, default = 0)),
    to_km = .data$from_km + .data$length_km)

  fr <- ungroup(fr)

  broken <- check_chain_continuity(fr)

  if (length(broken) > 0)
    stop("the chain frame is not continuous on mainstem(s): ",
      paste(broken, collapse = ", "),
      ".\n  Each flowline's within-chain link must be the id of the one ",
      "below it. A missing flowline, a toid down a minor path, one ",
      "mainstem id on both branches of a divergence, or two runs given ",
      "one id breaks this. Fix how x was assembled.", call. = FALSE)

  fr
}

#' Continuity check over an ordered chain frame
#' @description After ordering bottom first, each flowline's within-chain
#' link must be the id of the flowline before it, the bottom row excepted
#' (link NA). One shifted comparison over the whole frame, the shift taken
#' within each mainstem, vectorized over all rows.
#' @param fr ordered chain frame (grouped output of chain_frame, ungrouped)
#' @returns character vector of aggregate_ids that fail the check
#' @noRd
check_chain_continuity <- function(fr) {

  dt <- data.table::data.table(aggregate_id = fr$aggregate_id,
    id = fr$id, link = fr$link)

  # bottom row of each chain: no row before it, so prev_id and link are NA;
  # every other row's link must equal the id of the row before it. The frame
  # is already ordered bottom first within each mainstem, so the shift down
  # one within aggregate_id names the row before.
  dt[, prev_id := data.table::shift(id, 1L), by = "aggregate_id"]

  ok <- (is.na(dt$prev_id) & is.na(dt$link)) |
    (!is.na(dt$prev_id) & !is.na(dt$link) & dt$link == dt$prev_id)

  unique(fr$aggregate_id[!ok])
}


#' Confluences of identified mainstems
#'
#' @description Identifies each confluence of two identified mainstems as a
#' hydrolocation on the receiving mainstem at the outlet of the tributary.
#' A confluence exists where the bottom flowline of a tributary mainstem
#' connects downstream to a flowline on a different identified mainstem; the
#' point is anchored to that downstream flowline, which the link always
#' names and which is present even when the receiving mainstem is truncated
#' just above the node. Reads no coordinates.
#'
#' This is the `hydrolocation` feature type of OGC 25-045 with `mainstem_id`
#' as `aggregate_id` and `link` as `hydro_location_id`. Each identified
#' mainstem yields at most one confluence. A tributary whose downstream
#' connection is a diversion (`divergence == 2`) is dropped with a warning.
#'
#' @param x `hy_topo` or `hy_node` carrying `aggregate_id` on identified
#' flowlines, or a data.frame coercible with [hy()].
#' @returns data.frame with one row per confluence: `aggregate_id` (the
#' receiving mainstem), `hydro_location_id` (the tributary), `id` (the
#' flowline below the node on the receiving mainstem) and `id_measure`
#' (100).
#' @name get_confluences
#' @export
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

  # the frame's continuity check makes the bottom flowline unique per
  # mainstem, so each tributary yields at most one confluence
  chain_frame(x)

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

  # continuity check: makes the bottom flowline unique per mainstem
  chain_frame(x)

  # bottom rows of each identified chain (within-chain link NA)
  link <- chain_link_node(x)
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

#' Kilometer value of positions in a chain frame
#' @description A position (id, id_measure) is at from_km + length_km *
#' id_measure / 100. Joins by id; positions whose id is not in the frame
#' get NA.
#' @param id vector of flowline ids
#' @param id_measure vector of interpolative measures (0-100)
#' @param fr chain frame
#' @returns list with km (numeric) and aggregate_id (the mainstem each id
#' is on), parallel to id
#' @noRd
position_to_km <- function(id, id_measure, fr) {

  m <- match(id, fr$id)

  km <- fr$from_km[m] + fr$length_km[m] * id_measure / 100

  list(km = km, aggregate_id = fr$aggregate_id[m])
}

#' Map kilometer values back to flowline and measure in a chain frame
#' @description For each (aggregate_id, km), the flowline whose interval
#' holds km and the measure within it. A km below the bottom or above the
#' top of the flowlines present is placed at that end (measure 0 or 100)
#' and the overshoot returned as past_end_km.
#' @param aggregate_id vector of mainstem ids
#' @param km vector of kilometer values
#' @param fr chain frame
#' @returns tibble with id, id_measure, past_end_km, status parallel to km
#' @noRd
km_to_position <- function(aggregate_id, km, fr) {

  n <- length(km)

  frames <- as.data.table(fr)

  # per-chain ends, for the past-end branches
  ends <- frames[, list(
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

  # rolling join to the flowline whose interval holds km: within each chain
  # the last row with from_km <= km (roll = +Inf). At an exact interval
  # boundary the original picked the lower interval, so nudge the join key
  # down by 1e-9 to exclude the next interval's from_km; a km at the chain
  # bottom then misses and is filled from the bottom row below.
  look <- frames[, list(aggregate_id, from_km,
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

  out_id <- vector(mode = class(fr$id), length = n)
  out_id[] <- NA
  out_meas <- rep(NA_real_, n)
  past <- rep(0, n)
  status <- rep("placed", n)

  in_chain <- !is.na(e_bottom_id) & !is.na(km)
  status[!in_chain] <- "hydro_location_not_in_x"

  below <- in_chain & km < e_bottom_from
  above <- in_chain & km > e_top_to
  inside <- in_chain & !below & !above

  out_id[below] <- e_bottom_id[below]
  out_meas[below] <- 0
  past[below] <- e_bottom_from[below] - km[below]
  status[below] <- "past_end"

  out_id[above] <- e_top_id[above]
  out_meas[above] <- 100
  past[above] <- km[above] - e_top_to[above]
  status[above] <- "past_end"

  # km exactly at the chain's bottom from_km falls just below the
  # down-nudged join key and misses; it belongs to the bottom interval
  bottom_hit <- inside & is.na(match_id)
  match_id[bottom_hit] <- e_bottom_id[bottom_hit]
  match_from[bottom_hit] <- e_bottom_from[bottom_hit]
  match_len[bottom_hit] <- fr$length_km[match(e_bottom_id[bottom_hit], fr$id)]

  out_id[inside] <- match_id[inside]
  out_meas[inside] <- (km[inside] - match_from[inside]) /
    match_len[inside] * 100

  tibble(id = out_id, id_measure = out_meas, past_end_km = past,
    status = status)
}

#' Distance from the nearest referent along a mainstem
#'
#' @description Records, for each position, the signed distance along its
#' mainstem to the nearest referent among those supplied. Through the chain
#' frame every position and referent on the same mainstem is a kilometer
#' value; the nearest referent by absolute difference is the one recorded.
#' Reads no coordinates.
#'
#' @param indexes data.frame of positions with `id` and `id_measure`, on `x`.
#' @param hydro_locations data.frame of referents with `hydro_location_id`,
#' `id` and `id_measure`, on `x` (for example the output of
#' [get_confluences()]).
#' @param x `hy_topo` or `hy_node` carrying `aggregate_id` and `length_km`.
#' @returns `indexes` with `aggregate_id`, `hydro_location_id` and
#' `distance_km` added. `distance_km` is negative toward the outlet and
#' positive toward the head. A position on a mainstem with no referent gets
#' NA and a message.
#' @name add_hydro_location_distance
#' @export
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
add_hydro_location_distance.hy_topo <- function(indexes, hydro_locations, x) {
  add_hydro_location_distance_impl(indexes, hydro_locations, chain_frame(x))
}

#' @name add_hydro_location_distance
#' @export
add_hydro_location_distance.hy_node <- function(indexes, hydro_locations, x) {
  add_hydro_location_distance_impl(indexes, hydro_locations, chain_frame(x))
}

#' Shared body for add_hydro_location_distance
#' @noRd
add_hydro_location_distance_impl <- function(indexes, hydro_locations, fr) {

  pos <- position_to_km(indexes$id, indexes$id_measure, fr)
  ref <- position_to_km(hydro_locations$id, hydro_locations$id_measure, fr)

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

  # every position joined to every referent on its mainstem; nearest by
  # absolute difference, a tie taking the smaller hydro_location_id as a
  # string -- the same order() the per-row loop used, over all rows at once
  joined <- ref_dt[pos_dt, on = "aggregate_id", allow.cartesian = TRUE,
    nomatch = NULL]
  joined$d <- joined$pos_km - joined$ref_km

  hl <- rep(NA_character_, n)
  dist <- rep(NA_real_, n)

  if (nrow(joined) > 0) {
    joined$d_abs <- abs(joined$d)
    data.table::setorderv(joined, c(".row", "d_abs", "hl_str"))
    best <- joined[!duplicated(joined$.row), ]
    hl[best$.row] <- best$hydro_location_id
    dist[best$.row] <- best$d
  }

  if (any(is.na(hl) & !is.na(pos$aggregate_id)))
    message("some positions are on a mainstem with no referent; ",
      "distance_km is NA there.")

  indexes[[aggregate_id]] <- pos$aggregate_id
  indexes[[hydro_location_id]] <- hl
  indexes[[distance_km]] <- dist

  indexes
}

#' Position from a referent distance along a mainstem
#'
#' @description The inverse of [add_hydro_location_distance()]. For each
#' record (`aggregate_id`, `hydro_location_id`, `distance_km`) it finds the
#' referent's kilometer value in `x`'s frame, adds the distance, and maps
#' the result to `id` and `id_measure`. Reads no coordinates.
#'
#' @param positions data.frame of records with `aggregate_id`,
#' `hydro_location_id` and `distance_km`.
#' @param hydro_locations data.frame of referents with `aggregate_id`,
#' `hydro_location_id`, `id` and `id_measure` (for example
#' [get_confluences()] output).
#' @param x `hy_topo` or `hy_node` carrying `aggregate_id` and `length_km`.
#' @returns `positions` with `id`, `id_measure`, `past_end_km` and `status`
#' added. `status` is `placed`, `hydro_location_not_in_x`, or `past_end`; a
#' `past_end` row is placed at the end flowline present and `past_end_km`
#' carries how far past the end the arithmetic went.
#' @name index_hydro_location_distance
#' @export
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
index_hydro_location_distance.hy_topo <- function(positions, hydro_locations, x) {
  index_hydro_location_distance_impl(positions, hydro_locations, chain_frame(x))
}

#' @name index_hydro_location_distance
#' @export
index_hydro_location_distance.hy_node <- function(positions, hydro_locations, x) {
  index_hydro_location_distance_impl(positions, hydro_locations, chain_frame(x))
}

#' Shared body for index_hydro_location_distance
#' @noRd
index_hydro_location_distance_impl <- function(positions, hydro_locations, fr) {

  ref <- position_to_km(hydro_locations$id, hydro_locations$id_measure, fr)

  ref_tbl <- tibble(aggregate_id = hydro_locations$aggregate_id,
    hydro_location_id = hydro_locations$hydro_location_id,
    ref_km = ref$km)

  key_in  <- paste(positions$aggregate_id, positions$hydro_location_id)
  key_ref <- paste(ref_tbl$aggregate_id, ref_tbl$hydro_location_id)

  m <- match(key_in, key_ref)
  ref_km <- ref_tbl$ref_km[m]

  target_km <- ref_km + positions$distance_km

  placed <- km_to_position(positions$aggregate_id, target_km, fr)

  # a referent that is not in x gives hydro_location_not_in_x, overriding
  # the km mapping's own status
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
