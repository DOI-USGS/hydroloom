utils::globalVariables(c(".span", "bound", "from_km", "reversed",
  "segment_order", "to_km"))

##### get_substring.R -- substrings between two positions on a mainstem #####
# The span between two positions on one mainstem of x, as an ordered list of
# pieces, each a flowline id with a from and to measure. Works kilometer
# values from the step 3 chain frame; reads geometry only to cut pieces when
# x is sf and lwgeom is installed. See the "Step 4. Substrings" section of the
# mainstem translation workplan for the design.

#' Substring between two positions on a mainstem
#'
#' @description Returns the pieces between two positions on one mainstem of
#' `x`: an ordered list of flowlines, the end ones cut by measure. Through
#' the chain frame the two positions are kilometer values; the span runs from
#' the smaller to the larger, pieces come back downstream to upstream, and
#' `reversed` is `TRUE` on every row of a pair whose `from` position sat above
#' its `to`. The two positions are unordered ends and must lie on one
#' mainstem; a stretch that crosses a confluence is two calls, one per
#' mainstem.
#'
#' An end within `snap_km` of a flowline boundary moves to that boundary
#' first, so a node a few meters inside a flowline does not cut a sliver and
#' two source flowlines sharing a node share one cut. Zero-length pieces are
#' dropped. A span that is empty after snapping (both ends on one point)
#' returns one row for that `line_id` with `NA` `id` and zero `length_km`.
#'
#' When `x` is `sf` and \pkg{lwgeom} is installed the piece geometry from
#' `lwgeom::st_linesubstring()` is attached.
#'
#' @param from data.frame of downstream-or-either positions with `id` and
#' `id_measure`, one row per span. An optional `line_id` column labels the
#' output; without it the row number is used.
#' @param to data.frame of the other end, row-aligned with `from`.
#' @param x `hy_topo` or `hy_node` carrying `aggregate_id` and `length_km`, or
#' a data.frame coercible with [hy()].
#' @param snap_km numeric. An end within this many kilometers of a flowline
#' boundary snaps to it. Default 0.
#' @returns data.frame with `line_id`, `segment_order` (1 at the downstream
#' end), `id`, `id_from_measure`, `id_to_measure`, `length_km` and `reversed`,
#' one or more rows per span, plus piece geometry when `x` is `sf` and
#' \pkg{lwgeom} is installed.
#' @inheritSection index_points_to_lines Measures
#' @name get_substring
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
#' get_substring(from, to, net)
get_substring <- function(from, to, x, snap_km = 0) {
  UseMethod("get_substring", x)
}

#' @name get_substring
#' @export
get_substring.data.frame <- function(from, to, x, snap_km = 0) {
  get_substring(from, to, hy(x), snap_km)
}

#' @name get_substring
#' @export
get_substring.hy <- function(from, to, x, snap_km = 0) {
  hy_classify_and_redispatch(x, "get_substring", "hy_topo or hy_node",
    hy_guidance_topo, from = from, to = to, snap_km = snap_km)
}

#' @name get_substring
#' @export
get_substring.hy_topo <- function(from, to, x, snap_km = 0) {
  get_substring_impl(from, to, x, chain_frame(x), snap_km)
}

#' @name get_substring
#' @export
get_substring.hy_node <- function(from, to, x, snap_km = 0) {
  get_substring_impl(from, to, x, chain_frame(x), snap_km)
}

#' Shared body for get_substring
#' @description Vectorized over all spans. Converts both ends to kilometers in
#' the chain frame, snaps each to a flowline boundary within snap_km, then in
#' one join finds the flowlines of each span's mainstem whose intervals
#' overlap its span, cutting the end pieces by measure and dropping zero-length
#' pieces. Geometry is cut with lwgeom when x is sf.
#' @noRd
get_substring_impl <- function(from, to, x, fr, snap_km) {

  if (nrow(from) != nrow(to))
    stop("from and to must have the same number of rows.", call. = FALSE)

  line_id <- if ("line_id" %in% names(from)) from$line_id else seq_len(nrow(from))

  from_pos <- position_to_km(from$id, from$id_measure, fr)
  to_pos <- position_to_km(to$id, to$id_measure, fr)

  if (any(is.na(from_pos$aggregate_id) | is.na(to_pos$aggregate_id)))
    stop("every position must be on an identified mainstem of x.",
      call. = FALSE)

  if (any(from_pos$aggregate_id != to_pos$aggregate_id))
    stop("the two positions of a span must lie on one mainstem; a stretch ",
      "that crosses a confluence is two calls, one per mainstem.",
      call. = FALSE)

  agg <- from_pos$aggregate_id

  from_km <- snap_to_boundary(from_pos$km, agg, fr, snap_km)
  to_km <- snap_to_boundary(to_pos$km, agg, fr, snap_km)

  reversed <- from_km > to_km

  lo <- pmin(from_km, to_km)
  hi <- pmax(from_km, to_km)

  spans <- data.table::data.table(.span = seq_along(line_id),
    aggregate_id = as.character(agg), lo = lo, hi = hi,
    reversed = reversed)

  empty <- spans[spans$lo >= spans$hi, ]
  nonempty <- spans[spans$lo < spans$hi, ]

  frdt <- data.table::as.data.table(fr)
  frdt$agg_chr <- as.character(frdt$aggregate_id)

  pieces <- substring_pieces(nonempty, frdt)

  # empty spans: one row for the flowline the single point sits on, NA id and
  # zero length, so a caller still gets a row keyed by line_id
  empty_rows <- empty_span_rows(empty, frdt)

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
#' @description For each km, the nearest from_km or to_km of its mainstem's
#' flowlines; the km moves there when it is within snap_km, otherwise it is
#' left as is. One rolling join to the nearest boundary over all km at once.
#' @noRd
snap_to_boundary <- function(km, agg, fr, snap_km) {

  if (snap_km <= 0) return(km)

  # every boundary of every mainstem, keyed for a nearest rolling join; the
  # boundary is kept under a second column so the join returns it rather than
  # the query km (which shares the roll-column name)
  bounds <- data.table::data.table(
    aggregate_id = as.character(rep(fr$aggregate_id, 2L)),
    bound = c(fr$from_km, fr$to_km),
    boundary = c(fr$from_km, fr$to_km))
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

#' Pieces of each non-empty span
#' @description For each span, the flowlines of its mainstem whose
#' from_km-to_km interval overlaps the span's lo-to-hi range; the end pieces
#' cut to the span, zero-length pieces dropped, numbered from 1 at the
#' downstream end.
#' @noRd
substring_pieces <- function(spans, frdt) {

  empty_piece <- data.table::data.table(.span = integer(0),
    segment_order = integer(0), id = frdt$id[0],
    id_from_measure = numeric(0), id_to_measure = numeric(0),
    length_km = numeric(0), reversed = logical(0))

  if (nrow(spans) == 0) return(empty_piece)

  # every flowline of each span's mainstem, then kept where its interval
  # overlaps the span
  j <- frdt[spans, on = c(agg_chr = "aggregate_id"), allow.cartesian = TRUE,
    nomatch = NULL]
  j <- j[j$to_km > j$lo & j$from_km < j$hi, ]

  # cut the end pieces to the span
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

#' One row per empty span
#' @description A span whose two ends fall on one point returns one row for the
#' flowline holding that point, with NA id-measures, NA id and zero length.
#' @noRd
empty_span_rows <- function(spans, frdt) {

  empty_piece <- data.table::data.table(.span = integer(0),
    segment_order = integer(0), id = frdt$id[0],
    id_from_measure = numeric(0), id_to_measure = numeric(0),
    length_km = numeric(0), reversed = logical(0))

  if (nrow(spans) == 0) return(empty_piece)

  na_id <- frdt$id[0][1]

  data.table::data.table(.span = spans$.span,
    segment_order = 1L, id = na_id,
    id_from_measure = NA_real_, id_to_measure = NA_real_,
    length_km = 0, reversed = spans$reversed)
}

#' Attach piece geometry cut with lwgeom
#' @description For each piece, lwgeom::st_linesubstring() on its flowline
#' between id_from_measure/100 and id_to_measure/100. Empty-span rows (NA id)
#' get empty geometry. st_linesubstring() takes a single from/to applied to
#' all of x, so the cut is made one piece at a time; this is geometry
#' realization, not the frame arithmetic, which is vectorized above.
#' @noRd
attach_piece_geometry <- function(result, x) {

  geom <- sf::st_zm(sf::st_geometry(x))
  m <- match(result$id, x$id)

  have <- !is.na(m) & !is.na(result$id_from_measure)

  # start every row empty, then overwrite the real pieces
  g <- sf::st_sfc(rep(list(sf::st_linestring()), nrow(result)),
    crs = sf::st_crs(geom))

  if (any(have)) {
    cut <- mapply(function(line, from, to)
      lwgeom::st_linesubstring(line, from, to),
      geom[m[have]],
      result$id_from_measure[have] / 100,
      result$id_to_measure[have] / 100,
      SIMPLIFY = FALSE)
    g[have] <- do.call(c, cut)
  }

  sf::st_sf(result, geometry = g)
}
