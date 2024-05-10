test_that("to_flownetwork", {

  f <- sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom"))

  x <- to_flownetwork(f)

  dm <- x[x$downmain,]
  um <- x[x$upmain, ]

  expect_false(any(duplicated(dm$id)))
  expect_false(any(duplicated(um$toid)))

  expect_equal(names(x), c("id", "toid", "upmain", "downmain"))

  expect_equal(nrow(x), 832)

  f <- select(f, -StreamLeve)

  y <- to_flownetwork(f)

  expect_equal(hy_reverse(x), hy_reverse(y))

  x <- select(f, -LevelPathI)

  expect_error(to_flownetwork(x), "either stream_level of levelpath")

  x <- select(f, -Divergence)

  expect_error(to_flownetwork(x), "divergence")
})
