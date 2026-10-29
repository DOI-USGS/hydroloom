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

test_that("divergence check counts each flowline once per fromnode (D3-213)", {

  # flowline 1 diverges to 2 (main) and 3 (minor): two rows at fromnode 1
  fn <- data.frame(id = c(1, 1, 2, 3, 4),
    toid = c(2, 3, 4, 0, 0),
    fromnode = c(1, 1, 2, 2, 3),
    tonode = c(2, 2, 3, 4, 5),
    divergence = c(0, 0, 1, 2, 0),
    divergence_fraction = c(1, 1, 0.7, 0.3, 1))

  expect_true(check_hy_graph(fn, divergence_check = TRUE))

  # the divergence result is classed by hy() on the subset, as the cycle
  # result is
  bad <- fn
  bad$divergence[1:2] <- 1
  out <- check_hy_graph(bad, divergence_check = TRUE)

  expect_equal(out$id, c(1, 1))
  expect_s3_class(out, "hy_flownetwork")

  bad <- fn
  bad$divergence[5] <- 1
  out <- check_hy_graph(bad, divergence_check = TRUE)

  expect_equal(out$id, 4)
  expect_s3_class(out, "hy_topo")

})
