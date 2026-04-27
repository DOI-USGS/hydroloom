# Tests covering the six outlet conventions hydroloom accepts:
#   1. numeric 0 (canonical numeric sentinel)
#   2. character "" (canonical character sentinel)
#   3. NA
#   4. implicit absence (toid = a value not in id, no special sentinel)
#   5. foreign sentinel (e.g., -1)
#   6. unique-per-outlet identifiers (each outlet has a distinct toid value
#      that is not present in id; useful for keeping outlets individually
#      addressable)
#
# Each convention is exercised through the core pipeline: hy(), is_outlet(),
# sort_network(), add_levelpaths(), accumulate_downstream(), and
# make_node_topology().

# Reference dendritic network: 4 -> 5 -> outlet, 1 -> 2 -> 3 -> 5
#   id     1 2 3 4 5
#   toid   2 3 5 5 OUTLET
build_dendritic <- function(id_vec, toid_vec, var = c(1, 2, 3, 4, 1)) {
  data.frame(id = id_vec, toid = toid_vec, var = var)
}

test_that("numeric 0 sentinel: full pipeline", {
  x <- build_dendritic(1:5, c(2, 3, 5, 5, 0))
  h <- hy(x)
  expect_equal(hydroloom:::is_outlet(h), c(FALSE, FALSE, FALSE, FALSE, TRUE))
  expect_no_warning(s <- sort_network(h))
  expect_equal(tail(s$id, 1), 5)
})

test_that("character '' sentinel: full pipeline", {
  x <- build_dendritic(as.character(1:5), c("2", "3", "5", "5", ""))
  h <- hy(x)
  expect_equal(hydroloom:::is_outlet(h), c(FALSE, FALSE, FALSE, FALSE, TRUE))
  expect_no_warning(s <- sort_network(h))
  expect_equal(tail(s$id, 1), "5")
})

test_that("NA outlet: full pipeline (hy() normalizes NA -> sentinel)", {
  x <- build_dendritic(1:5, c(2, 3, 5, 5, NA))
  h <- hy(x)
  # hy() preserves backward-compat by replacing NA with the canonical sentinel.
  expect_equal(h$toid[5], 0)
  expect_true(hydroloom:::is_outlet(h)[5])
  expect_no_warning(s <- sort_network(h))
  expect_equal(tail(s$id, 1), 5)
})

test_that("NA outlet preserved when bypassing hy() normalization", {
  # Direct construction without hy() -- NA in toid stays NA.
  x <- build_dendritic(1:5, c(2, 3, 5, 5, NA))
  expect_equal(hydroloom:::is_outlet(x), c(FALSE, FALSE, FALSE, FALSE, TRUE))
})

test_that("implicit absence (toid points to non-existent id)", {
  x <- build_dendritic(1:5, c(2, 3, 5, 5, 999))
  h <- hy(x)
  expect_equal(hydroloom:::is_outlet(h), c(FALSE, FALSE, FALSE, FALSE, TRUE))
  expect_no_warning(s <- sort_network(h))
  expect_equal(tail(s$id, 1), 5)
})

test_that("foreign sentinel (-1)", {
  x <- build_dendritic(1:5, c(2, 3, 5, 5, -1))
  h <- hy(x)
  expect_equal(hydroloom:::is_outlet(h), c(FALSE, FALSE, FALSE, FALSE, TRUE))
  expect_no_warning(s <- sort_network(h))
  expect_equal(tail(s$id, 1), 5)
  # The outlet's downstream identifier is preserved (-1, not 0).
  expect_equal(s$toid[s$id == 5], -1)
})

test_that("unique-per-outlet identifiers preserved", {
  # Two outlets with distinct downstream identifiers.
  x <- data.frame(
    id   = c("hw", "A", "B", "t1", "t2"),
    toid = c("A", "t1", "t2", "OUT_001", "OUT_002"),
    var  = c(1, 2, 3, 4, 5)
  )
  h <- hy(x)
  expect_equal(hydroloom:::is_outlet(h),
    c(FALSE, FALSE, FALSE, TRUE, TRUE))
  expect_no_warning(s <- sort_network(h))

  # Both outlet identifiers survive the sort -- distinct, not collapsed.
  expect_setequal(s$toid[s$id %in% c("t1", "t2")],
    c("OUT_001", "OUT_002"))
  expect_equal(s$toid[s$id == "t1"], "OUT_001")
  expect_equal(s$toid[s$id == "t2"], "OUT_002")
})

test_that("multi-terminal: get_bridge_flowlines keeps independent terminals distinct (numeric ids)", {
  # Numeric counterpart of the 65f6220 character-id regression test in
  # test_get_bridges.R.
  x <- data.frame(
    id       = c(100, 1, 2, 3, 11, 12, 13),
    fromnode = c(0,   1, 1, 1, 2,  3,  4),
    tonode   = c(1,   2, 3, 4, 5,  6,  7)
  )
  x <- add_toids(hy(x), return_dendritic = FALSE)
  result <- get_bridge_flowlines(x)
  expect_setequal(result, c(100, 1, 2, 3, 11, 12, 13))
})

test_that("multi-terminal: get_bridge_flowlines keeps independent terminals distinct (unique outlet ids)", {
  # Each terminal has a distinct outlet identifier; make_nondendritic_topology
  # must still give them per-feature node ids.
  x <- data.frame(
    id   = c("hw", "A", "B", "C", "t1", "t2", "t3"),
    toid = c("A",  "t1","t2","t3","OUT_t1", "OUT_t2", "OUT_t3")
  )
  result <- get_bridge_flowlines(hy(x))
  expect_setequal(result, c("hw", "A", "B", "C", "t1", "t2", "t3"))
})

test_that("make_node_topology accepts NA outlet", {
  x <- build_dendritic(1:5, c(2, 3, 5, 5, NA))
  expect_no_error(topo <- make_node_topology(x, add = FALSE))
  expect_equal(nrow(topo), 5)
  expect_true(all(c("fromnode", "tonode") %in% names(topo)))
})

test_that("make_node_topology accepts orphan outlet toid", {
  x <- build_dendritic(1:5, c(2, 3, 5, 5, 99999))
  expect_no_error(topo <- make_node_topology(x, add = FALSE))
  expect_equal(nrow(topo), 5)
})

test_that("check_hy_outlets warns on type mismatch", {
  # Numeric id with character toid -- a real bug condition.
  x <- structure(list(
    id   = c(1, 2, 3),
    toid = c("2", "3", "")
  ), class = "data.frame", row.names = 1:3)
  expect_warning(hydroloom:::check_hy_outlets(x),
    "incompatible types")
})

test_that("check_hy_outlets does NOT warn on non-canonical valid outlets", {
  # A foreign sentinel in a type-consistent table is a valid outlet marker
  # under the is_outlet() rule.
  x <- build_dendritic(1:5, c(2, 3, 5, 5, -1))
  expect_no_warning(hydroloom:::check_hy_outlets(hy(x)))
})

test_that("check_hy_outlets fix=TRUE canonicalizes outlets (destroys per-outlet ids)", {
  x <- data.frame(
    id   = c("hw", "A", "t1", "t2"),
    toid = c("A", "t1", "OUT_001", "OUT_002")
  )
  fixed <- hydroloom:::check_hy_outlets(hy(x), fix = TRUE)
  # All outlet markers collapsed to "" (canonical character sentinel).
  expect_equal(fixed$toid[fixed$id %in% c("t1", "t2")], c("", ""))
})
