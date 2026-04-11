test_that("make_node_topology", {
  nhf <- sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom"))

  d <- add_toids(
    dplyr::select(nhf, COMID, FromNode, ToNode, Divergence, FTYPE,
      AreaSqKM, LENGTHKM, GNIS_ID),
  )

  expect_error(make_node_topology(d), "fromnode or tonode already in data")

  x <- dplyr::select(d, -FromNode, -ToNode)

  y <- hy(x)
  y$toid[1] <- NA
  expect_error(make_node_topology(y), "NA toids found -- must be 0")

  y <- x
  y$toid[1] <- 12345
  expect_error(make_node_topology(y), "Not all non zero toids are in ids")

  y <- make_node_topology(x)

  expect_s3_class(y, "sf")

  expect_equal(names(y), c("COMID", "toid", "fromnode", "tonode",
    "Divergence", "FTYPE", "AreaSqKM", "LENGTHKM",
    "GNIS_ID", "geom"))

  y <- make_node_topology(x, add = FALSE)

  expect_s3_class(y, "tbl_df")

  expect_equal(names(y), c("COMID", "fromnode", "tonode"))

  # we expect the same number of tonodes
  expect_equal(length(unique(d$ToNode)), length(unique(y$tonode)))

  # we expect more tonodes because we lost divergences.
  expect_equal(length(unique(nhf$FromNode)) + sum(nhf$Divergence == 2),
    length(unique(y$fromnode)))

  # just the divergences which have unique fromids in x but don't in new hope.
  add_div <- add_toids(st_drop_geometry(dplyr::select(nhf,
    COMID, FromNode, ToNode)),
  return_dendritic = FALSE)
  add_div <- add_div[add_div$toid %in%
    nhf$COMID[nhf$Divergence == 2], ]

  y <- make_node_topology(x)

  # we need to get the node the divergences upstream neighbor goes to
  # first get the new outlet nodes for our old ids
  div <- dplyr::left_join(dplyr::select(add_div, COMID, toid),
    dplyr::select(y, COMID, tonode), by = "COMID")

  # now join upstream renaming the tonode to fromnode
  y <- dplyr::left_join(y, dplyr::select(div, toid, new_fromnode = tonode),
    by = c("COMID" = "toid"))

  y <- dplyr::mutate(y, fromnode = ifelse(!is.na(new_fromnode),
    new_fromnode, fromnode))

  y <- dplyr::select(y, -new_fromnode)

  y <- dplyr::distinct(y)

  expect_equal(y$tonode[y$COMID == 8893700], y$fromnode[y$COMID == 8893714])
  expect_equal(y$tonode[y$COMID == 8893700], y$fromnode[y$COMID == 8893706])

  # we would now expect the same number of fromnodes in each network
  expect_equal(length(unique(nhf$FromNode)),
    length(unique(y$fromnode)))

  z <- make_node_topology(dplyr::select(x, -Divergence), add_div = add_div)

  expect_equal(z$fromnode, y$fromnode)
  expect_equal(z$tonode, y$tonode)

  # if we put these in the same order, the new divergence attribute should match
  z <- dplyr::left_join(dplyr::select(st_drop_geometry(x), COMID), z, by = "COMID")
  expect_equal(z$divergence, x$Divergence)

  # nolint start
  # the below was used for testing
  # check <- sf::st_drop_geometry(dplyr::left_join(new_hope_flowline, x, by = c("COMID" = "id")))
  # check <- select(check, COMID, FromNode, ToNode, fromnode, tonode)
  #
  # nodes <- data.frame(node = unique(c(check$FromNode, check$ToNode)))
  #
  # nodes <- left_join(nodes,
  #                    dplyr::select(check, fromcomid = COMID, node = ToNode))
  #
  # nodes <- left_join(nodes,
  #                    dplyr::select(check, tocomid = COMID, node = FromNode))
  #
  #
  # n2 <- data.frame(node = unique(c(check$fromnode, check$tonode)))
  #
  # n2 <- left_join(n2,
  #                    dplyr::select(check, fromcomid = COMID, node = tonode))
  #
  # n2 <- left_join(n2,
  #                    dplyr::select(check, tocomid = COMID, node = fromnode))
  #
  # matches <- lapply(1:nrow(n2), function(x) {
  #   any(sapply(1:nrow(nodes), function(y) {
  #     n2$fromcomid[x] == nodes$fromcomid[y] &
  #       n2$tocomid[x] == nodes$tocomid[y]
  #   }))
  # })
  #
  # matches <- as.logical(matches)
  #
  # any(!matches, na.rm = TRUE)
  # nolint end

})

test_that("no duplicates in nodes", {
  # includes a node with both convergent and divergent flowlines.
  sub <- dplyr::tibble(
    id = c(
      "{D08E57F9-54FB-4565-9F12-36B7BA864674}", "3fbc6c54-634d-425d-a109-81b27b1cdaf0",
      "{22DFB1D5-7181-49E2-B764-0B8536180297}", "{22DFB1D5-7181-49E2-B764-0B8536180297}",
      "ba3da26d-7e1a-472f-95dd-4b1d1e73f5f8", "ba3da26d-7e1a-472f-95dd-4b1d1e73f5f8"
    ),
    toid = c(
      "3fbc6c54-634d-425d-a109-81b27b1cdaf0", "5e09c754-23e6-4ac7-b8e8-3c21efb05f0d",
      "{D08E57F9-54FB-4565-9F12-36B7BA864674}", "d0b1d83c-b184-42a1-bd94-b3f92f9f6621",
      "d0b1d83c-b184-42a1-bd94-b3f92f9f6621", "{D08E57F9-54FB-4565-9F12-36B7BA864674}"
    ),
  )

  expect_true(!any(duplicated(hydroloom::make_node_topology(sub, add_div = TRUE)$id)))

})
