test_that("a flowline's own two ends return itself at 0 to 100", {

  base <- dplyr::tibble(
    id   = c("a1", "a2", "a3"),
    toid = c("",   "a1", "a2"),
    aggregate_id = c("A", "A", "A"),
    length_km = c(1, 2, 3))

  h <- hy(base)

  from <- dplyr::tibble(id = "a2", id_measure = 0)
  to   <- dplyr::tibble(id = "a2", id_measure = 100)

  sub <- get_subpath(from, to, h)

  expect_equal(nrow(sub), 1)
  expect_equal(sub$id, "a2")
  expect_equal(sub$id_from_measure, 0)
  expect_equal(sub$id_to_measure, 100)
  expect_equal(sub$length_km, 2)
  expect_equal(sub$segment_order, 1L)
  expect_false(sub$reversed)
})

test_that("a span across three flowlines returns ordered pieces cut at the ends", {

  base <- dplyr::tibble(
    id   = c("a1", "a2", "a3"),
    toid = c("",   "a1", "a2"),
    aggregate_id = c("A", "A", "A"),
    length_km = c(1, 1, 1))

  h <- hy(base)

  # from the middle of a1 (km 0.5) to the middle of a3 (km 2.5)
  from <- dplyr::tibble(id = "a1", id_measure = 50)
  to   <- dplyr::tibble(id = "a3", id_measure = 50)

  sub <- get_subpath(from, to, h)

  expect_equal(sub$id, c("a1", "a2", "a3"))
  expect_equal(sub$segment_order, 1:3)
  expect_equal(sub$id_from_measure, c(50, 0, 0))
  expect_equal(sub$id_to_measure, c(100, 100, 50))
  expect_equal(sub$length_km, c(0.5, 1, 0.5))
  expect_true(all(!sub$reversed))

  # the piece lengths sum to the distance between the two positions
  expect_equal(sum(sub$length_km), 2)
})

test_that("from above to returns the same pieces with reversed TRUE", {

  base <- dplyr::tibble(
    id   = c("a1", "a2", "a3"),
    toid = c("",   "a1", "a2"),
    aggregate_id = c("A", "A", "A"),
    length_km = c(1, 1, 1))

  h <- hy(base)

  up   <- dplyr::tibble(id = "a3", id_measure = 50)
  down <- dplyr::tibble(id = "a1", id_measure = 50)

  forward  <- get_subpath(dplyr::tibble(id = "a1", id_measure = 50),
    dplyr::tibble(id = "a3", id_measure = 50), h)
  backward <- get_subpath(up, down, h)

  expect_equal(backward$id, forward$id)
  expect_equal(backward$id_from_measure, forward$id_from_measure)
  expect_equal(backward$id_to_measure, forward$id_to_measure)
  expect_equal(backward$length_km, forward$length_km)
  expect_true(all(backward$reversed))
})

test_that("two coincident ends return the empty row", {

  base <- dplyr::tibble(
    id   = c("a1", "a2", "a3"),
    toid = c("",   "a1", "a2"),
    aggregate_id = c("A", "A", "A"),
    length_km = c(1, 1, 1))

  h <- hy(base)

  from <- dplyr::tibble(id = "a2", id_measure = 50)
  to   <- dplyr::tibble(id = "a2", id_measure = 50)

  sub <- get_subpath(from, to, h)

  expect_equal(nrow(sub), 1)
  expect_true(is.na(sub$id))
  expect_equal(sub$length_km, 0)
})

test_that("snap_m moves a near-boundary end to the boundary", {

  base <- dplyr::tibble(
    id   = c("a1", "a2", "a3"),
    toid = c("",   "a1", "a2"),
    aggregate_id = c("A", "A", "A"),
    length_km = c(1, 1, 1))

  h <- hy(base)

  # a2 at measure 2 is km 1.02, within 50 m of the a1/a2 boundary at km 1;
  # snapping drops the a1 sliver so the span starts clean at a2
  from <- dplyr::tibble(id = "a2", id_measure = 2)
  to   <- dplyr::tibble(id = "a3", id_measure = 50)

  snapped <- get_subpath(from, to, h, snap_m = 50)

  expect_equal(snapped$id, c("a2", "a3"))
  expect_equal(snapped$id_from_measure[1], 0)
})

test_that("multiple spans are keyed by line_id", {

  base <- dplyr::tibble(
    id   = c("a1", "a2", "a3"),
    toid = c("",   "a1", "a2"),
    aggregate_id = c("A", "A", "A"),
    length_km = c(1, 1, 1))

  h <- hy(base)

  from <- dplyr::tibble(line_id = c("x", "y"),
    id = c("a1", "a2"), id_measure = c(0, 0))
  to <- dplyr::tibble(id = c("a2", "a3"), id_measure = c(100, 100))

  sub <- get_subpath(from, to, h)

  expect_setequal(unique(sub$line_id), c("x", "y"))
  expect_equal(sub$id[sub$line_id == "x"], c("a1", "a2"))
  expect_equal(sub$id[sub$line_id == "y"], c("a2", "a3"))
})

test_that("the V2 flowlines of 2408056 indexed onto 3DHP tile the 3DHP aggregate flowpath", {

  d3 <- read_3dhp(keep_geometry = TRUE)
  if (is.null(d3)) skip("3DHP sample not available")
  if (!requireNamespace("lwgeom", quietly = TRUE)) skip("Missing lwgeom")

  v2 <- read_v2()
  v2 <- sf::st_cast(v2, "LINESTRING", warn = FALSE)

  # V2 flowlines on New Hope River
  v2_nh <- v2[!is.na(v2$aggregate_id) & v2$aggregate_id == nh, ]

  d3 <- sf::st_cast(d3, "LINESTRING", warn = FALSE)
  d3 <- sf::st_transform(d3, sf::st_crs(v2_nh))

  sr <- units::set_units(500, "m")

  # each V2 flowline's two ends indexed onto the 3DHP New Hope aggregate flowpath;
  # index_points_to_lines drops unplaced points, so join start and end by
  # point_id and keep flowlines placed at both ends
  starts <- get_node(v2_nh, position = "start")
  ends   <- get_node(v2_nh, position = "end")

  idx_start <- index_points_to_lines(d3, sf::st_geometry(starts),
    aggregate_ids = rep(nh, nrow(v2_nh)), search_radius = sr)
  idx_end   <- index_points_to_lines(d3, sf::st_geometry(ends),
    aggregate_ids = rep(nh, nrow(v2_nh)), search_radius = sr)

  both <- intersect(idx_start$point_id, idx_end$point_id)
  expect_true(length(both) > 0)

  se <- idx_start[match(both, idx_start$point_id), ]
  en <- idx_end[match(both, idx_end$point_id), ]

  from <- dplyr::tibble(line_id = both,
    id = en$id, id_measure = en$id_measure)
  to <- dplyr::tibble(id = se$id, id_measure = se$id_measure)

  sub <- suppressWarnings(get_subpath(from, to, d3, snap_m = 50))

  # pieces that are real (not empty-span rows) tile the 3DHP aggregate flowpath between the
  # first and last cut: their kilometer intervals cover [min, max] with no gap
  # and no overlap, so the piece lengths sum to the covered span
  axis3 <- hydroloom:::aggregate_flowpath_axis(d3)
  real <- sub[!is.na(sub$id), ]
  km_from <- hydroloom:::position_to_km(real$id, real$id_from_measure, axis3)$km
  km_to   <- hydroloom:::position_to_km(real$id, real$id_to_measure, axis3)$km

  lo <- pmin(km_from, km_to)
  hi <- pmax(km_from, km_to)

  # the pieces tile the aggregate flowpath between the first and last cut: sorted by lo,
  # each piece starts no higher than the running maximum of the ends reached
  # so far, so the covered interval has no gap. A small overlap is allowed
  # because NHDPlusV2 and 3DHP draw 2408056 differently: four V2 flowlines
  # index onto 3DHP with their own upstream end below their downstream end
  # (self-inverting), and a five-flowline side channel sits 100-336 m off the
  # 3DHP line, so those pieces double-cover a short stretch. This is real
  # cross-representation geometry, not a get_subpath() defect; the
  # piece-length-to-span ratio is reported, not asserted.
  ord <- order(lo)
  lo <- lo[ord]
  hi <- hi[ord]
  running_max <- cummax(c(hi[1], hi[-length(hi)]))
  gap <- lo[-1] - running_max[-1]

  covered <- max(hi) - min(lo)
  total <- sum(hi - lo)
  message("piece length sum / covered span = ", round(total / covered, 4))
  expect_true(all(gap <= 1e-3))
})

test_that("piece geometry end points coincide with the positions' points", {

  d3 <- read_3dhp(keep_geometry = TRUE)
  if (is.null(d3)) skip("3DHP sample not available")
  if (!requireNamespace("lwgeom", quietly = TRUE)) skip("Missing lwgeom")

  d3 <- sf::st_cast(d3, "LINESTRING", warn = FALSE)

  h3 <- hy(sf::st_drop_geometry(d3))
  axis <- hydroloom:::aggregate_flowpath_axis(h3)
  nh_rows <- axis[axis$aggregate_id == nh, ]
  nh_rows <- nh_rows[order(nh_rows$from_km), ]

  # a span from a quarter of the way up the bottom NH flowline to the middle
  # of the one above it; the asymmetric cut tells the two ends apart
  from <- dplyr::tibble(id = nh_rows$id[1], id_measure = 25)
  to   <- dplyr::tibble(id = nh_rows$id[2], id_measure = 50)

  sub <- get_subpath(from, to, d3)

  expect_true(inherits(sub, "sf"))

  # one LINESTRING per piece, not every cut merged into each row
  expect_true(all(sf::st_geometry_type(sub) == "LINESTRING"))

  # the from position's point via get_hydro_location; the flowline id is given
  # under the comid alias so get_hydro_location keeps it (it renames an `id`
  # column to point_id), and the aggregate measures span the whole flowline
  fp <- d3
  fp$comid <- fp$id
  fp$aggregate_id_from_measure <- 0
  fp$aggregate_id_to_measure <- 100
  from_pt <- get_hydro_location(
    dplyr::tibble(comid = from$id, aggregate_id_measure = from$id_measure),
    dplyr::select(fp, comid, aggregate_id_from_measure,
      aggregate_id_to_measure))

  # the downstream end of the first piece is the from position's point; the
  # piece keeps the flowline's vertex order, upstream to downstream
  # (id_measure 100 at the first vertex, 0 at the last), so the measure-25
  # cut sits at the piece's last vertex
  first_geom <- sf::st_geometry(sub)[sub$segment_order == 1]
  first_coords <- sf::st_coordinates(first_geom)
  down_end <- first_coords[nrow(first_coords), c("X", "Y")]
  from_coords <- sf::st_coordinates(from_pt)[1, c("X", "Y")]

  # the st_linesubstring cut and the get_hydro_location interpolation agree
  expect_equal(unname(down_end), unname(from_coords), tolerance = 1e-6)
})

test_that("the top COMID of Morgan Creek indexed onto 3DHP closes at the head", {

  d3 <- read_3dhp(keep_geometry = TRUE)
  if (is.null(d3)) skip("3DHP sample not available")
  if (!requireNamespace("lwgeom", quietly = TRUE)) skip("Missing lwgeom")

  v2 <- read_v2()
  v2 <- sf::st_cast(v2, "LINESTRING", warn = FALSE)

  v2_mc <- v2[!is.na(v2$aggregate_id) & v2$aggregate_id == mc, ]
  axis_v2 <- hydroloom:::aggregate_flowpath_axis(v2_mc)
  top_id <- axis_v2$id[which.max(axis_v2$to_km)]
  top <- v2_mc[v2_mc$id == top_id, ]

  d3 <- sf::st_cast(d3, "LINESTRING", warn = FALSE)
  d3 <- sf::st_transform(d3, sf::st_crs(v2_mc))

  axis3 <- hydroloom:::aggregate_flowpath_axis(sf::st_drop_geometry(hy(d3)))
  head3_id <- axis3$id[axis3$aggregate_id == mc][which.max(
    axis3$to_km[axis3$aggregate_id == mc])]

  start <- get_node(top, position = "start")
  end   <- get_node(top, position = "end")

  # a radius wide enough to reach the 3DHP head
  sr <- units::set_units(2000, "m")
  idx_start <- index_points_to_lines(d3, sf::st_geometry(start),
    aggregate_ids = mc, search_radius = sr)
  idx_end   <- index_points_to_lines(d3, sf::st_geometry(end),
    aggregate_ids = mc, search_radius = sr)

  expect_equal(nrow(idx_start), 1)
  expect_equal(nrow(idx_end), 1)

  sub <- suppressWarnings(get_subpath(
    dplyr::tibble(id = idx_end$id, id_measure = idx_end$id_measure),
    dplyr::tibble(id = idx_start$id, id_measure = idx_start$id_measure),
    d3, snap_m = 50))

  # the upstream end of the span closes at the 3DHP head flowline
  top_piece <- sub[sub$segment_order == max(sub$segment_order), ]
  expect_equal(top_piece$id, head3_id)
})
