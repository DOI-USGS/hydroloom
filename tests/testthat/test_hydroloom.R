test_that("s3 class creation", {
  x <- data.frame(comid = 1, fromnode = 2, tonode = 3)

  y <- hy(x)

  expect_equal(names(y), c("id", "fromnode", "tonode"))

  expect_s3_class(y, "hy")

  x <- dplyr::tibble(comid = c(1, 2), tocomid = c(2, NA), fromnode = c(1, 2), tonode = c(2, 3))

  y <- hy(x)

  expect_true("orig_names" %in% names(attributes(y)))

  expect_true(inherits(y, "tbl"))

  expect_message(expect_false(is.hy(unclass(y))), "no hy class attribute")

  # NA toid is a valid outlet marker under the is_outlet() rule;
  # is.hy() accepts it.
  y$toid[1] <- NA

  expect_true(is.hy(y))

  y <- hy(x)

  attr(y, "orig_names") <- NULL

  expect_message(expect_false(is.hy(y)), "no original names attribute")

  x <- sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom"))

  expect_s3_class(hy(x), "sf")

  expect_false(inherits(hy(x, clean = TRUE), "sf"))

  expect_true(inherits(hy(x), "tbl"))

  expect_message(expect_error(x <- hy_reverse(x)), "no hy class attribute")

  x <- sf::st_sf(dplyr::as_tibble(x))

  # 1/0 flags become logical in hy() (M-18)
  x$StartFlag <- x$StartFlag == 1
  x$TerminalFl <- x$TerminalFl == 1

  expect_equal(x, hy_reverse(hy(x)))
})

test_that("hy ingest: infinite identifier stops, non-binary flag warns", {

  # I2-3: rows named by position; the identifier value is the problem
  x <- data.frame(COMID = c(1, 2, Inf), ToCOMID = c(2, 3, NA))

  expect_error(hy(x), "Column COMID holds an infinite number \\(Inf or -Inf\\) on these rows: 3\\.")

  x <- data.frame(COMID = c(1, 2, 3), ToCOMID = c(2, -Inf, NA))

  expect_error(hy(x), "Column ToCOMID holds an infinite number \\(Inf or -Inf\\) on these rows: 2\\.")

  # NaN stays missing (D3-180)
  x <- data.frame(COMID = c(1, 2, 3), ToCOMID = c(2, 3, NaN))

  expect_identical(hy(x)$toid, c(2L, 3L, 0L))

  # D3-152: passes through, warning names the column and rows by id
  x <- data.frame(COMID = c(10L, 20L, 30L), ToCOMID = c(20L, 30L, NA),
    TerminalFl = c(0L, 2L, 3L), StartFlag = c(1L, 0L, NA))

  expect_warning(y <- hy(x),
    "Flag column TerminalFl holds values other than 0, 1, or missing on these rows: 20, 30\\.")

  expect_identical(y$terminal_flag, c(0L, 2L, 3L))
  expect_identical(y$start_flag, c(TRUE, FALSE, NA))
})

test_that("test print", {

  p <- capture_output(print(hydroloom_name_definitions))

  expect_true(grepl("topo_sort", p))

})
