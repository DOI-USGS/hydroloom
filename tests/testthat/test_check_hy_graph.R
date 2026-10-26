test_that("divergence check requires its attributes", {
  expect_error(
    check_hy_graph(data.frame(id = 1:2, toid = c(2, 0)),
      divergence_check = TRUE),
    "requires attributes: fromnode, divergence")
})
