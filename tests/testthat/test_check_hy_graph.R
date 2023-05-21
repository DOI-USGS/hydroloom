test_that("base check_hy__graph", {
  test_data <- data.frame(id = c(1, 2, 3, 4, 6, 7, 8, 9),
                          toid = c(2, 3, 4, 9, 7, 8, 9, 4))

  remove <- check_hy_graph(test_data)

  expect_equal(remove$row, c(4, 8))
})

# test_that("loop check", {
#   test_data <- data.frame(id = c(1, 2, 3, 5, 5, 4),
#                         toid = c(3, 5, 5, 4, 6, 3))
#
#   remove <- check_hy_graph(test_data, loop_check = TRUE)
#
#   remove
# })
