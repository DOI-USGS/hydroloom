# Builds the conformance fixtures (SPEC S-5, S-6; VERIFICATION 2.1) and their
# index, index.yaml. Run from the repository root:
#
#   Rscript inst/extdata/conformance/fixtures/make_fixtures.R
#
# Every fixture is one GeoPackage holding one layer named for the file stem.
# Attribute-only fixtures are aspatial tables; geometry fixtures are stored in
# EPSG:5070 except the state-plane copy, whose point is its CRS. Sources are
# read from the checkout (tests/testthat/data, inst/extdata), not from an
# installed package. The 3DHP subset needs the hydrogeofetch sample; when it is
# unavailable the existing fixture and its index entry are kept.
#
# GeoPackage files are not byte-identical across runs (GDAL writes
# timestamps); their contents are.
#
# To build only some fixtures, name their stems in HYDROLOOM_FIXTURES,
# separated by commas or spaces:
#
#   HYDROLOOM_FIXTURES="hy_worked_example,hy_large_ids" Rscript inst/extdata/conformance/fixtures/make_fixtures.R
#
# Every other fixture file is left as it is and keeps its index entry, so
# existing GeoPackages are not rewritten (D2-20). Unset, every fixture is
# rebuilt.

out_dir <- file.path("inst", "extdata", "conformance", "fixtures")
rds <- function(name) readRDS(file.path("tests", "testthat", "data", paste0(name, ".rds")))

stopifnot(dir.exists(out_dir))

only <- strsplit(Sys.getenv("HYDROLOOM_FIXTURES"), "[,[:space:]]+")[[1]]
only <- only[nzchar(only)]

build <- function(name) length(only) == 0 || name %in% only

index <- list()

# Write one fixture and record its index entry. A fixture not selected by
# HYDROLOOM_FIXTURES is skipped before x is evaluated.
fixture <- function(x, name, source, exercises, rules = character(),
  layer_options = NULL) {

  if (!build(name)) return(invisible(NULL))

  if (!inherits(x, "sf")) x <- as.data.frame(x)
  attr(x, "orig_names") <- NULL
  rownames(x) <- NULL

  f <- file.path(out_dir, paste0(name, ".gpkg"))
  if (file.exists(f)) unlink(f)

  sf::st_write(x, f, layer = name, quiet = TRUE, layer_options = layer_options)

  index[[name]] <<- list(
    file = paste0(name, ".gpkg"),
    source = source,
    rows = nrow(x),
    geometry = if (inherits(x, "sf")) {
      paste0("EPSG:", sf::st_crs(x)$epsg)
    } else {
      "none"
    },
    columns = setdiff(names(x), attr(x, "sf_column")),
    exercises = exercises,
    rules = as.list(rules),
    bytes = as.integer(file.size(f)))

  invisible(f)
}

lines_5070 <- function(coords) {
  sf::st_sfc(lapply(coords, function(m) sf::st_linestring(matrix(m, ncol = 2, byrow = TRUE))),
    crs = 5070)
}

# Real subsets (VERIFICATION 2.1) -----------------------------------------

# New Hope, assembled as tests/testthat/helper-mainstems.R does through
# inst/extdata/new_hope_mainstems.R: mainstem URIs joined on as mainstemid
# (the aggregate_id alias), REACHCODE dropped so hy() maps mainstemid rather
# than REACHCODE to aggregate_id, LINESTRING, no Z or M.
nh <- sf::read_sf(file.path("inst", "extdata", "new_hope.gpkg"))
ms <- utils::read.csv(file.path("inst", "extdata", "new_hope_mainstems.csv"))

nh$mainstemid <- ms$mainstem_uri[match(nh$COMID, ms$comid)]
nh$REACHCODE <- NULL
nh <- sf::st_zm(sf::st_cast(nh, "LINESTRING", warn = FALSE))
nh <- sf::st_transform(nh, 5070)

fixture(nh, "new_hope",
  "inst/extdata/new_hope.gpkg with mainstemid from inst/extdata/new_hope_mainstems.csv (helper-mainstems.R read_v2 assembly, before hy())",
  "the default NHDPlusV2 network; nhdplus aliases; mainstemid as aggregate_id; divergences; 32-bit integer identifiers")

fixture(sf::st_transform(nh, 2264), "new_hope_state_plane",
  "new_hope fixture transformed to EPSG:2264, NAD83 / North Carolina (ftUS)",
  "one network duplicated in a feet-based state plane zone; exact-factor unit conversion against the EPSG:5070 copy",
  "G-4")

# NHDPlus HR: tests/testthat/data/hr.rds stores NHDPlusID as COMID; it is
# renamed back so the nhdplushr alias is the one exercised. Columns trimmed to
# the network attributes.
hr <- rds("hr")
hr <- hr[, c("COMID", "Permanent_Identifier", "GNIS_ID", "GNIS_Name", "LENGTHKM", "REACHCODE",
  "FTYPE", "StreamLeve", "StreamOrde", "StreamCalc", "FromNode", "ToNode", "Hydroseq",
  "LevelPathI", "Pathlength", "TerminalPa", "ArbolateSu", "Divergence", "StartFlag",
  "TerminalFl", "UpLevelPat", "UpHydroseq", "DnLevelPat", "DnHydroseq", "DnMinorHyd",
  "AreaSqKM", "TotDASqKM")]
names(hr)[names(hr) == "COMID"] <- "NHDPlusID"

fixture(hr, "nhdplushr",
  "tests/testthat/data/hr.rds (COMID renamed NHDPlusID; network columns only)",
  "identifiers outside 32-bit range (character in R by I-2); nhdplushr aliases",
  c("I-1", "I-2"))

# 3DHP: the hydrogeofetch New Hope sample, toid from dnhydrosequence as
# helper-mainstems.R read_3dhp does, source column names kept.
d3_file <- if (build("new_hope_3dhp")) tryCatch({
  e <- new.env()
  source(system.file("extdata", "new_hope_3dhp.R", package = "hydrogeofetch"), local = e)
  e$sample_new_hope_3dhp
}, error = function(e) NULL)

if (!is.null(d3_file) && file.exists(d3_file)) {
  d3 <- sf::read_sf(d3_file)

  m <- match(d3$dnhydrosequence, d3$hydrosequence)
  d3$toid <- d3$id3dhp[m]
  d3$toid[is.na(d3$toid)] <- ""

  d3 <- sf::st_drop_geometry(d3)
  d3 <- d3[, c("id3dhp", "toid", "mainstemid", "hydrosequence", "dnhydrosequence",
    "lengthkm", "divergence")]

  fixture(d3, "new_hope_3dhp",
    "hydrogeofetch sample_new_hope_3dhp (helper-mainstems.R read_3dhp: toid from dnhydrosequence, outlet toid \"\"); geometry dropped",
    "string identifiers; 3dhp aliases; mainstemid as aggregate_id")
} else if (build("new_hope_3dhp")) {
  message("hydrogeofetch 3DHP sample unavailable; new_hope_3dhp fixture not rebuilt")
}

# Test-suite networks, aspatial as stored.
fixture(rds("network"), "network",
  "tests/testthat/data/network.rds",
  "NHDPlusV2 attribute table with divergences, no geometry; nhdplus aliases")

fixture(rds("coastal"), "coastal",
  "tests/testthat/data/coastal.rds",
  "many coastal terminals with missing tocomid (I-5 toid fill)",
  "I-5")

fixture(rds("loop"), "loop",
  "tests/testthat/data/loop.rds (hy attributes dropped)",
  "non-dendritic network with repeated id and character identifiers; cycles",
  c("I-6", "D-8"))

fixture(rds("loop2"), "loop2",
  "tests/testthat/data/loop2.rds",
  "braced GUID string identifiers; repeated id; cycles",
  c("I-1", "I-6", "D-8"))

fixture(rds("sort_test"), "sort_test",
  "tests/testthat/data/sort_test.rds",
  "integer ID/toNexID with -1 outlet value (foreign reserved toid)",
  "I-4")

fixture(rds("sort_network_dups"), "sort_network_dups",
  "tests/testthat/data/sort_network_dups.rds",
  "nextgen wb-/nex- string identifiers with repeated id",
  c("I-1", "I-6"))

# Synthetic networks (VERIFICATION 2.1) ------------------------------------

# Node topology shared by the two divergence fixtures. Three divergences:
# at node 104 decided by name (1a), at 107 by feature type (2), and at 108 a
# full tie where R's smallest-id rule picks 9 and identifier byte order picks
# "10" (D-9 fallthrough).
div <- data.frame(
  id = 1:13,
  fromnode = c(101, 103, 102, 104, 104, 106, 105, 107, 108, 108, 112, 111, 107),
  tonode = c(102, 102, 104, 105, 106, 105, 107, 108, 111, 112, 111, 113, 110),
  name = c("A", NA, "A", "A", "B", "B", "A", NA, NA, NA, NA, NA, NA),
  ftype = c(rep("StreamRiver", 12), "CanalDitch"),
  length_km = 1,
  da_sqkm = 1)

fixture(div, "divergence_no_attribute",
  "synthetic",
  "divergences without a divergence attribute: name match at node 104, feature type at 107, full tie at 108 between ids 9 and 10",
  "D-9")

div$divergence <- c(0L, 0L, 0L, 2L, 1L, 0L, 0L, 1L, 2L, 1L, 0L, 0L, 2L)

fixture(div, "divergence_attribute",
  "synthetic (divergence_no_attribute with a divergence column)",
  "a divergence attribute that disagrees with the rule sequence at every divergence: the source attribute wins",
  "D-9")

fixture(data.frame(
  id = 1:10,
  fromnode = c(101, 102, 103, 104, 105, 102, 106, 106, 108, 109),
  tonode = c(102, 103, 104, 105, 109, 106, 104, 108, 105, 110),
  divergence = c(0L, 1L, 0L, 0L, 0L, 2L, 1L, 2L, 0L, 0L),
  length_km = 1),
  "return_divergence",
  "synthetic",
  "a minor path leaving at node 102 that returns at 104 (id 7) and, after a second divergence, at 105 (id 9)")

# The divergence at node 103: id 3 reaches the inland sink (6), id 4 the
# coast (5), so the coastal rule and the smallest-id rule disagree. Ids 7
# and 8 are a separate network ending inland.
fixture(data.frame(
  id = 1:8,
  fromnode = c(101, 102, 103, 103, 104, 106, 201, 202),
  tonode = c(102, 103, 106, 104, 105, 107, 202, 203),
  name = NA_character_,
  ftype = "StreamRiver"),
  "coastal_inland",
  "synthetic; case args coastal_outlet_ids = 5, inland_outlet_ids = [6, 8]",
  "coastal terminal beside an inland one; coastal_outlet_ids / inland_outlet_ids in add_divergence")

fixture(data.frame(
  id = 1:12,
  toid = c(3, 3, 6, 5, 6, 7, 10, 9, 10, 11, 0, 0),
  fromnode = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
  tonode = c(3, 3, 6, 5, 6, 7, 10, 9, 10, 11, 13, 14),
  length_km = 1,
  da_sqkm = 1),
  "isolated_flowline",
  "synthetic",
  "a flowline (id 12) with no edges inside a larger network")

fixture(data.frame(
  id = 1:7,
  toid = c(3, 3, 4, 0, 6, 5, 5)),
  "reciprocal_pair",
  "synthetic",
  "a two-flowline cycle (ids 5 and 6), labeled reciprocal, beside an acyclic tree",
  "D-8")

fixture(data.frame(
  id = 1:6,
  toid = c(2, 3, 4, 2, 6, 0)),
  "three_cycle",
  "synthetic",
  "a three-flowline cycle (ids 2, 3, 4) the pairwise test could not see",
  "D-8")

fixture(data.frame(
  id = 1:5,
  toid = c(2, 3, 3, 5, 0)),
  "self_loop",
  "synthetic",
  "a self-loop (id 3, toid == id), a size-one component with a self-edge",
  "D-8")

# Four parallel lines in integer coordinates. Ids 20 and 10 are equidistant
# (10 m) from the point at (50, 0); 20 comes first in row order, 10 first in
# identifier byte order.
fixture(sf::st_sf(
  id = c(20L, 10L, 30L, 40L),
  toid = 0L,
  length_km = 0.1,
  geom = lines_5070(list(c(0, 10, 100, 10), c(0, -10, 100, -10),
    c(0, 50, 100, 50), c(0, -50, 100, -50)))),
  "point_index_tie",
  "synthetic; points in point_index_tie_points.gpkg",
  "exact point-indexing tie from integer coordinates: tree order and identifier order disagree",
  c("D-10", "G-7"))

fixture(sf::st_sf(
  point_id = 1L,
  geom = sf::st_sfc(sf::st_point(c(50, 0)), crs = 5070)),
  "point_index_tie_points",
  "synthetic; second table for point_index_tie",
  "the point equidistant from ids 20 and 10",
  c("D-10", "G-7"))

# Boundary fixtures (SPEC S-6) ---------------------------------------------

fixture(sf::st_sf(
  id = 1L, toid = 0L, fromnode = 1L, tonode = 2L,
  name = "A", divergence = 0L, length_km = 1, da_sqkm = 1, total_da_sqkm = 1,
  geom = lines_5070(list(c(0, 1000, 0, 0)))),
  "single_flowline",
  "synthetic",
  "S-6 single-row network: defined values on every n = 1 path (indptr length 2, one-node DFS, level path of one)",
  "S-6")

# Identifiers whose byte order (100, 2, 30, 4, 7) differs from their numeric
# order and from row order.
fixture(sf::st_sf(
  id = c(30L, 4L, 100L, 2L, 7L), toid = 0L,
  fromnode = 1:5, tonode = 6:10,
  name = c("A", "B", "C", "D", "E"),
  length_km = 1, da_sqkm = 1, total_da_sqkm = 1,
  geom = lines_5070(lapply(0:4, function(i) c(i * 2000, 1000, i * 2000, 0)))),
  "all_terminals",
  "synthetic",
  "S-6 all terminals: five flowlines, no edges; identifier order from sort_network, five singleton level paths, zero path length",
  c("S-6", "D-4"))

fixture(data.frame(
  id = c(1L, 1L, 2L),
  toid = c(2L, 2L, 0L)),
  "duplicated_edge",
  "synthetic",
  "S-6 duplicated edge in a flownetwork (the 1 -> 2 row repeated): raises",
  c("S-6", "I-6"))

fixture(data.frame(
  id = 1:3, toid = c(3L, 3L, 0L),
  fromnode = c(1L, 2L, 3L), tonode = c(3L, 3L, 4L),
  name = c("A", "B", "A"),
  da_sqkm = c(1, NA, 1)),
  "missing_weight",
  "synthetic",
  "S-6 missing value in the weight attribute (da_sqkm, row 2): raises naming the row in accumulate_downstream and add_levelpaths",
  "S-6")

fixture(sf::st_sf(
  id = 1L, toid = 0L, length_km = 0,
  geom = lines_5070(list(c(0, 0, 0, 0)))),
  "zero_length",
  "synthetic; points in zero_length_points.gpkg",
  "S-6 zero-length geometry (both vertices at the origin): measure 0 with a warning",
  "S-6")

fixture(sf::st_sf(
  point_id = 1L,
  geom = sf::st_sfc(sf::st_point(c(0, 0)), crs = 5070)),
  "zero_length_points",
  "synthetic; second table for zero_length",
  "a point on the zero-length line",
  "S-6")

fixture(data.frame(
  id = 1:2,
  name = c("", "New Hope River")),
  "empty_name",
  "synthetic",
  "S-6 empty name in string_score: score 0",
  "S-6")

fixture(data.frame(id = integer(), toid = integer()),
  "empty_network",
  "synthetic",
  "I-11 zero-row input: zero-row output with all computed columns present and typed",
  "I-11")

# Function fixtures: hy (algorithms/hy.md) ---------------------------------

fixture(nh[, setdiff(names(nh), "Divergence")], "new_hope_no_divergence",
  "new_hope fixture with the Divergence column removed",
  "node topology without a divergence attribute: add_topo keeps every downstream connection, so id repeats")

# A fixed row permutation, no seed (D-15): stepping through the rows by 389,
# which shares no factor with 746.
perm <- ((seq_len(nrow(nh)) - 1) * 389) %% nrow(nh) + 1
stopifnot(!anyDuplicated(perm))

fixture(nh[perm, ], "new_hope_shuffled",
  "new_hope fixture with rows permuted: row i is new_hope row ((i - 1) * 389) %% 746 + 1",
  "the same network in a different row order; output values do not depend on row order",
  "D-7")

fixture(data.frame(
  COMID = c(101, 102, 103),
  ToCOMID = c(102, 103, NA),
  Notes = c("a", "b", "c")),
  "hy_worked_example",
  "synthetic; the worked example in algorithms/hy.md",
  "whole-valued REAL identifiers under nhdplus aliases, a missing toid, and a column outside the vocabulary",
  c("I-2", "I-5"))

fixture(data.frame(comid = 1:2, tocomid = c(2L, NA)),
  "hy_toid_missing_integer",
  "synthetic",
  "INTEGER identifiers with a missing toid: filled with 0",
  "I-5")

fixture(data.frame(comid = c("a", "b"), tocomid = c("b", NA)),
  "hy_toid_missing_character",
  "synthetic",
  "TEXT identifiers with a missing toid: filled with \"\"",
  "I-5")

fixture(data.frame(
  nhdplusid = c(5000000001, 5000000002, 2),
  toid = c(5000000002, NA, 5000000002)),
  "hy_large_ids",
  "synthetic",
  "REAL identifiers beyond 32 bits beside a small one in the same column",
  c("I-1", "I-2"))

fixture(data.frame(id = 1:2, toid = c("2", NA)),
  "hy_id_integer_toid_character",
  "synthetic",
  "INTEGER id with TEXT toid and a missing toid: the outlet fill meets two types",
  c("I-2", "I-5"))

fixture(data.frame(ID = 1:2, ToID = c(2L, 0L), MyCol = 3:4),
  "hy_mixed_case_names",
  "synthetic",
  "hy names in mixed case become lowercase; a non-vocabulary name keeps its case")

fixture(data.frame(permanent_identifier = c("a", "b"), comid = 1:2, toid = c(2L, 0L)),
  "hy_permanent_identifier_tie",
  "synthetic",
  "permanent_identifier beside comid: permanent_identifier drops out and keeps its name")

fixture(data.frame(nhdplusid = 1:2, featureid = 3:4, toid = c(2L, 0L)),
  "hy_duplicate_alias_tie",
  "synthetic",
  "two id aliases: the later in alias-map order (featureid) wins, with a warning")

fixture(data.frame(comid = 1:2, id = 1:2, toid = c(2L, 0L)),
  "hy_name_conflict",
  "synthetic",
  "an alias (comid) beside its hy name (id): raises")

fixture(data.frame(id = c(1, 1, 2, 3), toid = c(2, 3, 0, 0)),
  "hy_duplicated_id",
  "synthetic",
  "repeated id with toid and no divergence column: a flownetwork",
  "I-6")

fixture(data.frame(
  id = c(1L, 1L, 2L, 3L),
  toid = c(2L, 3L, 0L, 0L),
  upmain = c(TRUE, FALSE, TRUE, FALSE),
  downmain = c(TRUE, TRUE, TRUE, TRUE)),
  "hy_flownetwork_downmain_conflict",
  "synthetic",
  "flownetwork with two downmain rows for id 1: raises")

fixture(data.frame(
  id = c(1L, 1L, 2L, 3L, 4L),
  toid = c(3L, 4L, 3L, 0L, 0L),
  upmain = c(TRUE, FALSE, TRUE, TRUE, TRUE),
  downmain = c(TRUE, FALSE, TRUE, TRUE, TRUE)),
  "hy_flownetwork_upmain_conflict",
  "synthetic",
  "flownetwork with two upmain rows into toid 3: raises")

fixture(data.frame(
  id = 1:3, toid = c(2L, 3L, 0L), topo_sort = 3:1,
  levelpath = 1L, levelpath_outlet_id = 3L),
  "hy_leveled",
  "synthetic",
  "id, toid, topo_sort, levelpath, and levelpath_outlet_id present: leveled")

fixture(data.frame(id = 1:2, toid = c(2L, NA), fromnode = 1:2, tonode = 2:3),
  "hy_toid_and_nodes",
  "synthetic",
  "toid beside fromnode and tonode: toid outranks the nodes and add_topo leaves it")

fixture(data.frame(id = c(3L, 1L, 2L), fromnode = c(9L, 1L, 2L), tonode = c(NA, 2L, 3L)),
  "hy_add_topo_isolated_first",
  "synthetic",
  "node topology whose first row has a missing tonode: add_topo moves it to the end",
  "D-14")

# Function fixtures: hy_reverse (algorithms/hy_reverse.md) -------------------

fixture(data.frame(ID = 1:3, toID = c(2L, 3L, NA), extra = c("a", "b", "c")),
  "hy_reverse_renamed",
  "synthetic; the worked example in algorithms/hy_reverse.md",
  "user-style names (ID, toID) restored by hy_reverse; a column outside the vocabulary; a missing toid filled on ingest and not restored",
  "I-5")

fixture(data.frame(ID = integer(), toID = integer(), extra = character()),
  "hy_reverse_zero_rows",
  "synthetic",
  "zero rows under user-style names: names and column types restored",
  "I-11")

fixture(data.frame(ID = c(1L, 1L, 2L, 3L, 4L), toID = c(2L, 3L, 4L, 4L, NA)),
  "hy_reverse_divergence",
  "synthetic",
  "a divergence (1 -> 2 and 1 -> 3, rejoining at 4) as one row per downstream flowline in user-style names: hy() gives a flownetwork",
  "I-6")

# Function fixtures: is.hy (algorithms/is.hy.md) ----------------------------

fixture(data.frame(comid = 1:3, tocomid = c(2L, 3L, NA)),
  "is_hy_source_names",
  "synthetic; the worked example in algorithms/is.hy.md",
  "a table in NHDPlusV2 source names with a missing tocomid: not hy as read",
  "I-4")

fixture(data.frame(id = c("1", "2", "3"), toid = c("2", "3", NA)),
  "is_hy_hy_names_na_toid",
  "synthetic",
  "hydroloom names, TEXT identifiers, and a missing toid: not hy as read in R",
  c("I-1", "I-4"))

# Function fixtures: align_names (algorithms/align_names.md) -----------------

# A fixed row permutation, no seed (D-15): stepping through the rows by 389,
# which shares no factor with 707.
net <- rds("network")
perm_net <- ((seq_len(nrow(net)) - 1) * 389) %% nrow(net) + 1
stopifnot(!anyDuplicated(perm_net))

fixture(net[perm_net, ], "network_shuffled",
  "network fixture with rows permuted: row i is network row ((i - 1) * 389) %% 707 + 1",
  "the NHDPlusV2 attribute table in a different row order; names do not depend on row order",
  "D-7")

fixture(data.frame(featureid = 3:4, nhdplusid = 1:2, toid = c(2L, 0L)),
  "align_names_duplicate_alias_swapped",
  "synthetic (hy_duplicate_alias_tie with the id aliases swapped)",
  "two id aliases in the other column order: featureid still wins, with a warning")

fixture(data.frame(COMID = 1:2, TotDASqKM = c(1.5, 3), TotDA = c(1.5, 3)),
  "align_names_totda",
  "synthetic",
  "two total_da_sqkm aliases (TotDASqKM, TotDA): totda wins, with a warning")

fixture(data.frame(comid = 1:2, nhdplusid = 3:4, featureid = 5:6, tocomid = c(2L, 0L)),
  "align_names_three_aliases",
  "synthetic",
  "three id aliases: raises after two warnings")

fixture(data.frame(permanent_identifier = c("a", "b"), tocomid = 1:2,
  to_permanent_identifier = c("x", "y")),
  "align_names_permanent_identifier_elsewhere",
  "synthetic",
  "permanent_identifier drops because toid has two candidates; to_permanent_identifier wins toid; no id column")

fixture(data.frame(permanent_identifier = c("a", "b"), to_permanent_identifier = c("b", "")),
  "align_names_permanent_identifier_pair",
  "synthetic",
  "permanent_identifier and to_permanent_identifier alone: id and toid, no message or warning")

fixture(data.frame(permanent_identifier = c("a", "b"), tocomid = 1:2),
  "align_names_permanent_identifier_alone",
  "synthetic",
  "permanent_identifier the only id alias: becomes id")

fixture(data.frame(id = 1:2, tocomid = c(2L, 0L), toid = c(2L, 0L)),
  "align_names_conflict_toid",
  "synthetic",
  "an alias (tocomid) beside its hy name (toid): raises")

fixture(sf::st_sf(
  COMID = 1:2,
  Shape = sf::st_sfc(sf::st_point(c(0, 0)), sf::st_point(c(1, 1)), crs = 5070)),
  "align_names_geometry_shape",
  "synthetic; geometry column written as Shape (GEOMETRY_NAME=Shape)",
  "sf input whose geometry column name has an uppercase letter: R stops inside sf",
  layer_options = "GEOMETRY_NAME=Shape")

# Function fixtures: hy_network_type (algorithms/hy_network_type.md) ---------

fixture(data.frame(id = c(1L, 1L, 2L), fromnode = c(10L, 10L, 11L), tonode = c(11L, 11L, 12L)),
  "hy_network_type_node_repeated_id",
  "synthetic",
  "repeated id with fromnode and tonode and no toid: neither flownetwork nor node, plain hy")

# Function fixtures: hy_capabilities (algorithms/hy_capabilities.md) ---------

fixture(data.frame(
  id = 1:3, toid = c(2L, 3L, 0L), topo_sort = 3:1,
  levelpath = 1L, levelpath_outlet_id = 3L, divergence = 0L),
  "capabilities_leveled",
  "synthetic; hy_capabilities worked example",
  "hy_leveled with a divergence column (all 0, no branching): to_flownetwork, add_pfafstetter, add_streamlevel available")

# Function fixtures: add_toids (algorithms/add_toids.md) ---------------------

# Flowline 1 splits at node 20 into main path 2 and minor path 3, which
# rejoin through 4 and 5 at node 50 above the outlet 6.
atw <- data.frame(
  id = 1:6,
  fromnode = c(10L, 20L, 20L, 30L, 40L, 50L),
  tonode = c(20L, 30L, 40L, 50L, 50L, 60L),
  divergence = c(0L, 1L, 2L, 0L, 0L, 0L))

fixture(atw, "add_toids_worked_example",
  "synthetic; the worked example in algorithms/add_toids.md",
  "a divergence at node 20 (main 2, minor 3) rejoining at node 50 above one outlet")

fixture(atw[c(1, 3, 2, 4, 5, 6), ], "add_toids_downstream_swapped",
  "add_toids_worked_example with input rows 2 and 3 swapped",
  "downstream connections at a divergence follow input row order, not identifier order",
  "D-7")

atw5 <- atw[1:5, ]

fixture(within(atw5, divergence[2] <- NA_integer_), "add_toids_missing_divergence",
  "first five rows of add_toids_worked_example with id 2 divergence missing",
  "a missing divergence below a divergence: in R 1.3.0 the flowline receives no upstream connection")

fixture(within(atw5, divergence[3] <- 1L), "add_toids_divergence_without_minor",
  "first five rows of add_toids_worked_example with id 3 divergence 1",
  "both downstream flowlines coded main (1): the upstream flowline repeats under return_dendritic = TRUE")

fixture(within(atw5, divergence[2] <- 2L), "add_toids_divergence_without_main",
  "first five rows of add_toids_worked_example with id 2 divergence 2",
  "both downstream flowlines coded minor (2): the upstream flowline becomes an outlet")

fixture(data.frame(
  id = c(9L, 1L, 2L, 3L),
  fromnode = c(90L, 1L, 2L, 5L),
  tonode = c(NA, 2L, 3L, 77L),
  divergence = 0L),
  "add_toids_missing_tonode_first",
  "synthetic",
  "a missing tonode on the first row (moves last, D-14 departure) and an unmatched tonode (id 3)",
  "D-14")

fixture(data.frame(
  id = 1:3,
  fromnode = 1:3,
  tonode = c(2L, NA, 2L),
  divergence = 0L),
  "add_toids_missing_tonode_target",
  "synthetic",
  "a flowline (id 2) with a missing tonode whose fromnode two flowlines reach: in R 1.3.0 never a target")

fixture(transform(atw, id = as.character(id)), "add_toids_character_id",
  "add_toids_worked_example with id TEXT",
  "TEXT identifiers: outlet toid \"\"",
  "I-5")

fixture(data.frame(id = 1L, fromnode = 1L, tonode = 2L, divergence = 0L),
  "add_toids_single_row",
  "synthetic",
  "S-6 single row on the node pattern",
  "S-6")

fixture(data.frame(
  id = c(30L, 4L, 100L, 2L, 7L),
  fromnode = 1:5, tonode = 6:10,
  divergence = 0L),
  "add_toids_no_connections",
  "synthetic (all_terminals identifiers and nodes, no toid)",
  "S-6 all terminals on the node pattern: every toid is the outlet value, input order",
  "S-6")

fixture(data.frame(id = integer(), fromnode = integer(), tonode = integer(),
  divergence = integer()),
  "add_toids_zero_rows",
  "synthetic",
  "I-11 zero rows on the node pattern: toid typed",
  "I-11")

# Index ---------------------------------------------------------------------

index_file <- file.path(out_dir, "index.yaml")

# A fixture not rebuilt this run (not selected, or the 3DHP source missing)
# keeps its previous index entry while its file exists.
if (file.exists(index_file)) {
  for (x in yaml::read_yaml(index_file)$fixtures) {
    stem <- sub("\\.gpkg$", "", x$file)
    x$rules <- as.list(unlist(x$rules))
    if (!stem %in% names(index) && file.exists(file.path(out_dir, x$file))) index[[stem]] <- x
  }
}

entries <- unname(index[sort(names(index), method = "radix")])

writeLines(c(
  "# Conformance fixtures (SPEC S-5, S-6; VERIFICATION 2.1).",
  "# Generated by make_fixtures.R; do not edit by hand.",
  "# rules: the SPEC anchors a fixture exercises; bytes: file size when built.",
  yaml::as.yaml(list(fixtures = entries))),
  index_file)

message(length(entries), " fixtures, ", sum(vapply(entries, function(x) x$bytes, 0)), " bytes")
