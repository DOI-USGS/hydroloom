test_that("to_flownetwork", {

  f <- sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom"))

  x <- to_flownetwork(f)

  dm <- x[x$downmain,]
  um <- x[x$upmain, ]

  expect_false(any(duplicated(dm$id)))
  expect_false(any(duplicated(um$toid)))

  expect_equal(names(x), c("id", "toid", "upmain", "downmain"))

  expect_equal(nrow(x), 832)

  expect_true(x$upmain[x$id == 8894360])
  expect_true(x$upmain[x$id == 8894356])

  expect_true(x$upmain[x$id == 8893396])
  expect_false(x$upmain[x$id == 8894154])

  expect_true(x$downmain[x$toid == 8893428])
  expect_false(x$downmain[x$toid == 8893442])

  x <- select(f, -Divergence)

  expect_error(to_flownetwork(x), "divergence")
})
