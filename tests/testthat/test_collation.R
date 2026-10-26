# String collation is byte order, never locale (SPEC D-1). Ships with the
# package and runs under R CMD check.

test_that("sorts put strings in C (byte) order", {
  x <- c("a", "B", "_c")
  c_order <- c("B", "_c", "a")

  expect_identical(dplyr::arrange(data.frame(s = x), s)$s, c_order)

  d <- data.table::data.table(s = x)
  data.table::setorder(d, s)
  expect_identical(d$s, c_order)

  d <- data.table::data.table(s = x)
  data.table::setorderv(d, "s")
  expect_identical(d$s, c_order)
})
