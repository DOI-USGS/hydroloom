utils::globalVariables(c(".span", "agg_chr", "bound", "from_km", "hi", "lo",
  "reversed", "segment_order", "to_km"))

##### get_subpath.R -- subpaths of an aggregate flowpath #####
# The subpath between two positions on one aggregate flowpath of x, as an
# ordered list of pieces, each a flowline id with a from and to measure. Works
# kilometer values from the aggregate flowpath axis; reads geometry only to
# cut pieces when x is sf and lwgeom is installed. See the mainstem
# referencing article for the design.

#' Get the subpath between two positions
#'
#' @description Returns the subpath between two positions on one aggregate
#' flowpath. An aggregate flowpath is the run of flowlines that share an
#' `aggregate_id` and link end to end, such as an identified mainstem. A
#' subpath is the part of it between two positions, each given as a flowline
#' `id` and `id_measure`. The result lists the flowlines of the subpath from
#' downstream to upstream, with the two end flowlines cut at the positions'
#' measures. This is how to pull a stretch of river, such as the river between
#' a gage and a confluence, out of any network that carries `aggregate_id`.
#'
#' Both positions must be on the same aggregate flowpath. A stretch that
#' crosses a confluence of two mainstems takes two calls, one for each
#' mainstem, split at the confluence.
#'
#' Support for cross-aggregate_id paths may be included in a future release.
#'
#' @details
#' `from` and `to` can be given in either order. Each position is converted to
#' kilometers along the aggregate flowpath axis (the aggregate flowpath
#' measured from its downstream end in `x`), and the subpath covers the
#' kilometer span between the two. Pieces always come back downstream to
#' upstream. If a `from` position is upstream of its `to`, `reversed` is
#' `TRUE` on every row for that pair.
#'
#' `snap_m` keeps small offsets from cutting slivers. An end within `snap_m`
#' meters of a flowline boundary moves to that boundary before cutting, so a node a
#' few meters inside a flowline does not leave a sliver, and two source
#' flowlines that share a node share one cut. Zero-length pieces are dropped.
#' If both ends land on one point after snapping, the result for that
#' `line_id` is a single row with an `NA` `id` and zero `length_km`.
#'
#' When `x` is `sf` and \pkg{lwgeom} is installed, each piece's geometry is cut
#' with `lwgeom::st_linesubstring()` and attached.
#'
#' @param from data.frame. One end of each subpath, with `id` and
#' `id_measure`; the ends are unordered. An optional `line_id` column labels
#' the output; without it the row number is used.
#' @param to data.frame. The other end, row-aligned with `from`.
#' @param x `hy_topo` or `hy_node`, or a data.frame coercible with [hy()].
#' Carries `aggregate_id` and `length_km`. A `topo_sort` column keeps building
#' the aggregate flowpath axis cheap; without it a full topological sort is
#' run.
#' @param snap_m numeric. An end within this many meters of a flowline
#' boundary snaps to it. Default 0.
#' @returns data.frame with `line_id`, `segment_order` (1 at the downstream
#' end), `id`, `id_from_measure`, `id_to_measure`, `length_km` and `reversed`,
#' one or more rows per subpath, plus piece geometry when `x` is `sf` and
#' \pkg{lwgeom} is installed.
#' @inheritSection index_points_to_lines Measures
#' @name get_subpath
#' @export
#' @examples
#' g <- sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom"))
#' csv <- utils::read.csv(
#'   system.file("extdata/new_hope_mainstems.csv", package = "hydroloom"))
#'
#' g$mainstemid <- csv$mainstem_uri[match(g$COMID, csv$comid)]
#' g$REACHCODE <- NULL
#'
#' g <- sf::st_cast(g, "LINESTRING", warn = FALSE)
#'
#' net <- add_toids(hy(g))
#'
#' # flowlines of one mainstem, bottom and top by topo_sort
#' nh <- "https://geoconnex.us/ref/mainstems/2408056"
#' on_nh <- net[!is.na(net$aggregate_id) & net$aggregate_id == nh, ]
#'
#' from <- data.frame(id = on_nh$id[which.min(on_nh$topo_sort)], id_measure = 0)
#' to <- data.frame(id = on_nh$id[which.max(on_nh$topo_sort)], id_measure = 100)
#'
#' get_subpath(from, to, net)
get_subpath <- function(from, to, x, snap_m = 0) {
  UseMethod("get_subpath", x)
}

#' @name get_subpath
#' @export
get_subpath.data.frame <- function(from, to, x, snap_m = 0) {
  get_subpath(from, to, hy(x), snap_m)
}

#' @name get_subpath
#' @export
get_subpath.hy <- function(from, to, x, snap_m = 0) {
  hy_classify_and_redispatch(x, "get_subpath", "hy_topo or hy_node",
    hy_guidance_topo, from = from, to = to, snap_m = snap_m)
}

#' @name get_subpath
#' @export
get_subpath.hy_topo <- function(from, to, x, snap_m = 0) {
  get_subpath_impl(from, to, x, aggregate_flowpath_axis(x), snap_m)
}

#' @name get_subpath
#' @export
get_subpath.hy_node <- function(from, to, x, snap_m = 0) {
  get_subpath_impl(from, to, x, aggregate_flowpath_axis(x), snap_m)
}

#' Shared body for get_subpath
#' @description Vectorized over all subpaths. Converts both ends to
#' kilometers on the aggregate flowpath axis, snaps each to a flowline boundary
#' within snap_m, then in one join finds the flowlines of each subpath's
#' aggregate flowpath whose intervals overlap the subpath's kilometer span,
#' cutting the end pieces by measure and dropping zero-length pieces. Geometry
#' is cut with lwgeom when x is sf.
#' @noRd
get_subpath_impl <- function(from, to, x, axis, snap_m) {

  # the axis is in kilometers; snap_m is meters
  snap_km <- snap_m / 1000

  if (nrow(from) != nrow(to))
    stop("from and to must have the same number of rows.", call. = FALSE)

  line_id <- if ("line_id" %in% names(from)) from$line_id else seq_len(nrow(from))

  from_pos <- position_to_km(from$id, from$id_measure, axis)
  to_pos <- position_to_km(to$id, to$id_measure, axis)

  if (any(is.na(from_pos$aggregate_id) | is.na(to_pos$aggregate_id)))
    stop("every position must be on a flowline of x with an aggregate_id.",
      call. = FALSE)

  if (any(from_pos$aggregate_id != to_pos$aggregate_id))
    stop("the two positions of a subpath must lie on one aggregate flowpath ",
      "(one aggregate_id); a stretch that crosses a confluence is two ",
      "calls, one per aggregate flowpath.",
      call. = FALSE)

  agg <- from_pos$aggregate_id

  from_km <- snap_to_boundary(from_pos$km, agg, axis, snap_km)
  to_km <- snap_to_boundary(to_pos$km, agg, axis, snap_km)

  reversed <- from_km > to_km

  lo <- pmin(from_km, to_km)
  hi <- pmax(from_km, to_km)

  spans <- data.table::data.table(.span = seq_along(line_id),
    aggregate_id = as.character(agg), lo = lo, hi = hi,
    reversed = reversed)

  empty <- spans[spans$lo >= spans$hi, ]
  nonempty <- spans[spans$lo < spans$hi, ]

  axis_dt <- data.table::as.data.table(axis)
  axis_dt$agg_chr <- as.character(axis_dt$aggregate_id)

  pieces <- subpath_pieces(nonempty, axis_dt)

  # empty subpaths: one row for the flowline the single point sits on, NA id
  # and zero length, so a caller still gets a row keyed by line_id
  empty_rows <- empty_span_rows(empty, axis_dt)

  out <- data.table::rbindlist(list(pieces, empty_rows), use.names = TRUE)
  data.table::setorderv(out, c(".span", "segment_order"))

  result <- tibble(
    line_id = line_id[out$.span],
    segment_order = out$segment_order,
    id = out$id,
    id_from_measure = out$id_from_measure,
    id_to_measure = out$id_to_measure,
    length_km = out$length_km,
    reversed = out$reversed)

  if (inherits(x, "sf") && requireNamespace("lwgeom", quietly = TRUE))
    result <- attach_piece_geometry(result, x)

  result
}

#' Snap kilometer values to a flowline boundary within snap_km
#' @description For each km, the nearest from_km or to_km of its aggregate
#' flowpath's flowlines; the km moves there when it is within snap_km,
#' otherwise it is left as is. One rolling join to the nearest boundary over
#' all km at once.
#' @noRd
snap_to_boundary <- function(km, agg, axis, snap_km) {

  if (snap_km <= 0) return(km)

  # every boundary of every aggregate flowpath, keyed for a nearest rolling
  # join; the boundary is kept under a second column so the join returns it
  # rather than the query km (which shares the roll-column name)
  bounds <- data.table::data.table(
    aggregate_id = as.character(rep(axis$aggregate_id, 2L)),
    bound = c(axis$from_km, axis$to_km),
    boundary = c(axis$from_km, axis$to_km))
  data.table::setkey(bounds, aggregate_id, bound)

  q <- data.table::data.table(.row = seq_along(km),
    aggregate_id = as.character(agg), bound = km)

  near <- bounds[q, on = c("aggregate_id", "bound"), roll = "nearest",
    mult = "first"]
  data.table::setorder(near, .row)

  nearest_bound <- near$boundary

  move <- !is.na(km) & !is.na(agg) & !is.na(nearest_bound) &
    abs(nearest_bound - km) <= snap_km

  out <- km
  out[move] <- nearest_bound[move]

  out
}

#' Pieces of each non-empty subpath
#' @description For each subpath, the flowlines of its aggregate flowpath whose
#' from_km-to_km interval overlaps the subpath's lo-to-hi kilometer span;
#' the end pieces cut to that span, zero-length pieces dropped, numbered from
#' 1 at the downstream end.
#' @noRd
subpath_pieces <- function(spans, axis_dt) {

  empty_piece <- data.table::data.table(.span = integer(0),
    segment_order = integer(0), id = axis_dt$id[0],
    id_from_measure = numeric(0), id_to_measure = numeric(0),
    length_km = numeric(0), reversed = logical(0))

  if (nrow(spans) == 0) return(empty_piece)

  # overlapping (flowline interval, subpath) pairs only, via an interval
  # join; foverlaps keys the axis on (agg_chr, from_km, to_km) and the
  # subpaths on (aggregate_id, lo, hi). Intervals are closed, so a
  # subpath's kilometer span touching a flowline end yields a zero-length
  # pair the piece_len > 0 filter drops.
  spans_key <- data.table::copy(spans)
  data.table::setnames(spans_key, "aggregate_id", "agg_chr")
  data.table::setkey(spans_key, agg_chr, lo, hi)
  data.table::setkey(axis_dt, agg_chr, from_km, to_km)

  j <- data.table::foverlaps(axis_dt, spans_key, type = "any", nomatch = NULL)

  # cut the end pieces to the subpath's kilometer span
  piece_from <- pmax(j$from_km, j$lo)
  piece_to <- pmin(j$to_km, j$hi)

  piece_len <- piece_to - piece_from
  j <- j[piece_len > 0, ]
  piece_from <- piece_from[piece_len > 0]
  piece_to <- piece_to[piece_len > 0]
  piece_len <- piece_len[piece_len > 0]

  # interpolative measures along the flowline: 0 at its outlet (from_km),
  # 100 at its head (to_km)
  flow_len <- j$to_km - j$from_km
  m_from <- (piece_from - j$from_km) / flow_len * 100
  m_to <- (piece_to - j$from_km) / flow_len * 100

  out <- data.table::data.table(.span = j$.span, from_km = piece_from,
    id = j$id, id_from_measure = m_from, id_to_measure = m_to,
    length_km = piece_len, reversed = j$reversed)

  # 1 at the downstream (smallest from_km) end, increasing upstream
  data.table::setorderv(out, c(".span", "from_km"))
  out[, segment_order := seq_len(.N), by = ".span"]

  out[, list(.span, segment_order, id, id_from_measure, id_to_measure,
    length_km, reversed)]
}

#' One row per empty subpath
#' @description A subpath whose two ends fall on one point returns one row
#' for the flowline holding that point, with NA id-measures, NA id and zero
#' length.
#' @noRd
empty_span_rows <- function(spans, axis_dt) {

  empty_piece <- data.table::data.table(.span = integer(0),
    segment_order = integer(0), id = axis_dt$id[0],
    id_from_measure = numeric(0), id_to_measure = numeric(0),
    length_km = numeric(0), reversed = logical(0))

  if (nrow(spans) == 0) return(empty_piece)

  na_id <- axis_dt$id[0][1]

  data.table::data.table(.span = spans$.span,
    segment_order = 1L, id = na_id,
    id_from_measure = NA_real_, id_to_measure = NA_real_,
    length_km = 0, reversed = spans$reversed)
}

#' Attach piece geometry cut with lwgeom
#' @description For each piece, lwgeom::st_linesubstring() on its flowline
#' between 1 - id_to_measure/100 and 1 - id_from_measure/100.
#' st_linesubstring() measures fractions from the first vertex, which is the
#' flowline's inlet (id_measure 100), so the measures are flipped; the piece
#' keeps the flowline's upstream-to-downstream vertex order. Empty-subpath rows
#' (NA id) get empty geometry. st_linesubstring() takes a single from/to
#' applied to all of x, so the cut is made one piece at a time; this is
#' geometry realization, not the arithmetic on the axis, which is vectorized
#' above.
#' @noRd
attach_piece_geometry <- function(result, x) {

  geom <- sf::st_zm(sf::st_geometry(x))
  m <- match(result$id, x$id)

  have <- !is.na(m) & !is.na(result$id_from_measure)

  # start every row empty, then overwrite the real pieces
  g <- sf::st_sfc(rep(list(sf::st_linestring()), nrow(result)),
    crs = sf::st_crs(geom))

  if (any(have)) {
    cut <- mapply(function(line, from, to) {
      lwgeom::st_linesubstring(line, from, to)
    },
      geom[m[have]],
      1 - result$id_to_measure[have] / 100,
      1 - result$id_from_measure[have] / 100,
      SIMPLIFY = FALSE)
    # cut holds sfg linestrings; c() on sfg would merge them into one
    # multilinestring, so build the sfc directly
    g[have] <- sf::st_sfc(cut, crs = sf::st_crs(geom))
  }

  sf::st_sf(result, geometry = g)
}
