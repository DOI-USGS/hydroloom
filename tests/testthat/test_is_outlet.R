test_that("is_outlet detects canonical numeric sentinel", {
  x <- data.frame(id = 1:4, toid = c(2, 3, 4, 0))
  expect_equal(hydroloom:::is_outlet(x), c(FALSE, FALSE, FALSE, TRUE))
})

test_that("is_outlet detects canonical character sentinel", {
  x <- data.frame(id = c("a", "b", "c"), toid = c("b", "c", ""))
  expect_equal(hydroloom:::is_outlet(x), c(FALSE, FALSE, TRUE))
})

test_that("is_outlet detects NA outlet", {
  x <- data.frame(id = 1:3, toid = c(2, 3, NA))
  expect_equal(hydroloom:::is_outlet(x), c(FALSE, FALSE, TRUE))
})

test_that("is_outlet detects implicit absence", {
  x <- data.frame(id = 1:3, toid = c(2, 3, 99))
  expect_equal(hydroloom:::is_outlet(x), c(FALSE, FALSE, TRUE))
})

test_that("is_outlet detects foreign sentinel", {
  x <- data.frame(id = 1:3, toid = c(2, 3, -1))
  expect_equal(hydroloom:::is_outlet(x), c(FALSE, FALSE, TRUE))
})

test_that("is_outlet detects unique-per-outlet identifiers", {
  x <- data.frame(
    id   = c("hw", "A", "B", "t1", "t2"),
    toid = c("A", "t1", "t2", "OUT_t1", "OUT_t2")
  )
  expect_equal(hydroloom:::is_outlet(x),
    c(FALSE, FALSE, FALSE, TRUE, TRUE))
})

test_that("is_outlet handles multiple outlets", {
  x <- data.frame(id = 1:6, toid = c(2, 0, 4, 0, 6, 0))
  expect_equal(hydroloom:::is_outlet(x),
    c(FALSE, TRUE, FALSE, TRUE, FALSE, TRUE))
})
