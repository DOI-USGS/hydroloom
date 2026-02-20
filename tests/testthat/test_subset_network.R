test_that("subset_network basic", {

  x <- sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom"))

  # data.frame interface without pre-adding toids
  sub <- subset_network(x, 8893420)

  expect_true("COMID" %in% names(sub))
  expect_true(nrow(sub) > 0)
  expect_true(nrow(sub) <= nrow(x))
  expect_true(8893420 %in% sub$COMID)
  expect_false("toid" %in% names(sub))

  # hy interface: canonical names
  sub_hy <- subset_network(hy(x), 8893420)

  expect_true("id" %in% names(sub_hy))
  expect_equal(nrow(sub), nrow(sub_hy))

  # geometry preserved
  expect_s3_class(sub, "sf")

})

test_that("subset_network only_up", {

  x <- sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom"))

  sub_full <- subset_network(x, 8893420, only_up = FALSE)

  expect_message(
    sub_up <- subset_network(x, 8893420, only_up = TRUE),
    "disconnecting"
  )

  expect_true(nrow(sub_up) > 0)
  expect_true(nrow(sub_up) <= nrow(sub_full))

})

test_that("subset_network closed basin", {

  # Network topology:
  #
  #   1 (10->20) --> 2 (20->30, div=1)   main outlet
  #                \
  #                 3 (20->40, div=2) --> 4 (40->50) --> 5 (50->60)  endorheic terminal
  #                                            ^
  #                                            |
  #                                      6 (55->50)  tributary to closed basin
  #
  # Upstream from outlet (id=2) finds {1, 2}.
  # id=3 shares fromnode=20 with the upstream set but was not reached.
  # Downstream from 3 reaches the terminal at id=5 (toid=0).
  # Upstream from 5 captures the closed basin {3, 4, 5, 6}.

  x <- data.frame(
    id        = c(1,  2,  3,  4,  5,  6),
    fromnode  = c(10, 20, 20, 40, 50, 55),
    tonode    = c(20, 30, 40, 50, 60, 50),
    divergence = c(0,  1,  2,  0,  0,  0)
  )

  # full subset captures the closed basin
  sub <- subset_network(x, outlet = 2)

  expect_equal(sort(sub$id), c(1, 2, 3, 4, 5, 6))

  # only_up misses the closed basin
  expect_message(
    sub_up <- subset_network(x, outlet = 2, only_up = TRUE),
    "disconnecting 1 missed diversion"
  )

  expect_equal(sort(sub_up$id), c(1, 2))

})

test_that("subset_network error on missing names", {

  x <- data.frame(id = 1:3, toid = c(2, 3, 0))

  expect_error(subset_network(hy(x), 1), "subset_network requires")

})
