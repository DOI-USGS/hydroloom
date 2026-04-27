test_that("to_flownetwork", {

  f <- sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom"))

  expect_warning(to_flownetwork(select(f, -LevelPathI)),
    "converting hy_node to non-dendritic edge list")

  x <- add_toids(f, return_dendritic = TRUE)

  expect_warning(to_flownetwork(x), "toid was provided and appears to be dendritic.")

  x <- to_flownetwork(f)

  dm <- x[x$downmain, ]
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

  expect_warning(to_flownetwork(x), "converting hy_node to non-dendritic edge list")
})

test_that("to_flownetwork hy_node without divergence/levelpath warns and returns flownetwork", {
  # Issue: dev/issue-hy-node-to-topo-conversion.md
  edges <- data.frame(
    id       = 1:5,
    fromnode = c("a", "b", "c", "b", "d"),
    tonode   = c("b", "c", "e", "d", "c")
  )

  expect_warning(x <- to_flownetwork(edges),
    "converting hy_node to non-dendritic edge list")

  expect_s3_class(x, "hy_flownetwork")
  expect_equal(names(x), c("id", "toid"))
  # flownetwork is topology-only: source hy attrs are not carried through
  expect_false("fromnode" %in% names(x))
  expect_null(attr(x, "orig_names"))
})
