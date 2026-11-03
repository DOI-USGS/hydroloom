test_that("align_names", {
  x <- sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom"))

  x$TotDA <- x$TotDASqKM

  expect_warning(x <- align_names(x), "Duplicate names found when aligning with hydroloom")

  expect_true(is.character(hydroloom_name_definitions))

})

test_that("hydroloom_names", {
  hln <- hydroloom_names()

  expect_equal(hln, hydroloom:::hydroloom_env$hydroloom_name_map)

  hln_add <- hydroloom_names(c(foo = "id"))

  expect_error(hydroloom_names(c("foobar")), "input must be named")

  expect_true(hln_add[["foo"]] == "id")
  expect_true(hydroloom:::hydroloom_env$hydroloom_name_map[["foo"]] == "id")

  hln_clear <- hydroloom_names(clear = TRUE)

  expect_equal(hln_clear, hydroloom:::vocab_alias_map())
  expect_equal(hydroloom:::hydroloom_env$hydroloom_name_map, hln_clear)
})

test_that("hydroloom_names clear = TRUE resets to the vocabulary aliases plus x (H-03, D3-20)", {
  on.exit(hydroloom_names(clear = TRUE))

  hydroloom_names(c(foo = "toid"))

  out <- hydroloom_names(c(FlowlineID = "id"), clear = TRUE)

  expect_equal(out, c(hydroloom:::vocab_alias_map(), c(flowlineid = "id")))
  expect_false("foo" %in% names(out))
  expect_equal(hydroloom:::hydroloom_env$hydroloom_name_map, out)

  x <- data.frame(FlowlineID = c(1, 2), TOCOMID = c(2, 0))

  expect_equal(names(hy(x))[1:2], c("id", "toid"))
})

test_that("hydroloom_names lowercases a registered alias (D3-65)", {
  on.exit(hydroloom_names(clear = TRUE))

  out <- hydroloom_names(c(FlowlineID = "id"))

  expect_equal(out[[length(out)]], "id")
  expect_equal(names(out)[length(out)], "flowlineid")

  x <- data.frame(FlowlineID = c(1, 2), toid = c(2, 0))

  expect_equal(names(hy(x))[1], "id")
})

test_that("hydroloom_names stops on a value that is not a hydroloom name", {
  on.exit(hydroloom_names(clear = TRUE))

  before <- hydroloom_names(c(flowline_id = "id"))

  expect_error(hydroloom_names(c(foo = "bar"), clear = TRUE),
    "not hydroloom names: foo = bar")

  expect_equal(hydroloom_names(), before)
})

test_that("hydroloom_names stops on an alias that is another hydroloom name", {
  on.exit(hydroloom_names(clear = TRUE))

  before <- hydroloom_names(c(flowline_id = "id"))

  expect_error(hydroloom_names(c(TOID = "id"), clear = TRUE),
    "mapped to a different hydroloom name: TOID = id")

  expect_equal(hydroloom_names(), before)

  out <- hydroloom_names(c(id = "id"))

  expect_equal(out[[length(out)]], "id")
})

test_that("a registered alias wins over another term's vocabulary alias (REVIEW_11032026 item 4)", {
  on.exit(hydroloom_names(clear = TRUE))

  hydroloom_names(c(comid = "toid"))

  x <- data.frame(COMID = c(1, 2), other = c("a", "b"))

  expect_equal(names(align_names(x)), c("toid", "other"))
})

test_that("permanent_identifier beside id returns unchanged (align_names.md step 3)", {
  x <- data.frame(id = c(1, 2), Permanent_Identifier = c("a", "b"))

  expect_message(out <- align_names(x), "permanent_identifier")

  expect_identical(out, x)

  x <- data.frame(id = 1:2, Permanent_Identifier = c("a", "b"), toid = c(2L, 0L))

  once <- suppressMessages(align_names(x))

  expect_identical(suppressMessages(align_names(once)), once)
})
