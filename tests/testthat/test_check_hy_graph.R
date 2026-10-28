test_that("divergence check requires its attributes", {
  expect_error(
    check_hy_graph(data.frame(id = 1:2, toid = c(2, 0)),
      divergence_check = TRUE),
    "requires attributes: fromnode, divergence")
})

test_that("divergence check rows carry no row column (D3-160)", {

  div_data <- data.frame(id = c(1, 2, 3, 4),
    toid = c(2, 4, 0, 0),
    fromnode = c(1, 2, 2, 3),
    tonode = c(2, 3, 4, 5),
    divergence = c(0, 2, 2, 0))

  out <- check_hy_graph(div_data, divergence_check = TRUE)

  expect_false("row" %in% names(out))
  expect_equal(out$id, c(2, 3))
  expect_true(all(nzchar(out$divergence_issue)))

})
