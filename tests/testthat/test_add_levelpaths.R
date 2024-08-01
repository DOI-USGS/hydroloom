test_that("add_levelpaths example", {

  g <- sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom"))

  test_flowline <- add_toids(g)

  expect_error(add_levelpaths(test_flowline, "borked", "ArbolateSu"),
               "name and weight attribute must be in x")

  lp1 <- add_levelpaths(test_flowline, "GNIS_ID", "ArbolateSu")

  expect_equal(names(lp1)[1:7], c("COMID", "toid", "levelpath_outlet_id", "Hydroseq", "LevelPathI", "geom", "GNIS_ID"))

  lp2 <- add_levelpaths(hy(test_flowline), "GNIS_ID", "arbolate_sum")

  expect_equal(names(lp2)[1:7], c("id", "toid", "levelpath_outlet_id", "topo_sort", "levelpath", "geom", "GNIS_ID"))

})

test_that("add_levelpaths non dendritic", {
  g <- sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom"))

  test_flowline <- add_toids(g, return_dendritic = FALSE)

  test_flowline <- hy(test_flowline)

  test_flowline <- dplyr::select(test_flowline, id, toid, divergence, GNIS_ID, arbolate_sum)

  expect_error(add_levelpaths(dplyr::select(test_flowline, -divergence),
                              "GNIS_ID", "arbolate_sum"),
               "divergence attribute must be included")

  lp1 <- add_levelpaths(test_flowline, "GNIS_ID", "arbolate_sum")

  expect_equal(nrow(lp1), nrow(g))

  test_flowline <- add_topo_sort(test_flowline) |>
    filter(!toid %in% test_flowline$id[test_flowline$divergence > 1]) |>
    select(-divergence)

  lp2 <- add_levelpaths(test_flowline, "GNIS_ID", "arbolate_sum")

  expect_equal(lp1, lp2)
})

test_that("reweight", {
  x <- readRDS(list.files(pattern = "reweight_test.rds",
                          full.names = TRUE, recursive = TRUE))
  w <- hydroloom:::reweight(x, nat = "nameid", wat = "weight",
                            override_factor = 5)
  expect_equal(w$weight[w$nameid == w$ds_nameid], 2)

  w <- hydroloom:::reweight(x, nat = "nameid", wat = "weight",
                            override_factor = 2)
  expect_equal(w$weight[w$nameid == w$ds_nameid], 1)
})

test_that("calculate level path", {
  x <- sf::read_sf(system.file("extdata", "walker.gpkg", package = "hydroloom"))

  x <- add_toids(x)

  y <- add_levelpaths(dplyr::select(x, "COMID", "toid", "GNIS_ID", "ArbolateSu"),
                      "GNIS_ID", "ArbolateSu", status = TRUE)

  nhdp_lp <- sort(unique(x$LevelPathI))
  nhdt_lp <- sort(unique(y$levelpath))

  expect_true(length(nhdp_lp) == length(nhdt_lp))

  for(lp in seq_along(nhdp_lp)) {
    nhdp <- dplyr::filter(x, LevelPathI == nhdp_lp[lp])
    outlet_comid <- dplyr::filter(nhdp, Hydroseq == min(Hydroseq))$COMID
    nhdt <- dplyr::filter(y, levelpath_outlet_id == outlet_comid)
    expect(all(nhdp$COMID %in% nhdt$COMID), paste("Mismatch in", nhdp_lp[lp],
                                               "level path from NHDPlus."))
  }

  # break the data
  x$GNIS_ID[x$COMID == 5329293] <- " "
  x$GNIS_ID[x$COMID == 5329295] <- "255208"
  z <- add_levelpaths(dplyr::select(x, "COMID", "toid", "GNIS_ID", "ArbolateSu"),
                      "GNIS_ID", "ArbolateSu", status = TRUE)

  expect_equal(z$levelpath[z$COMID == 5329295], 1)

  z <- add_levelpaths(dplyr::select(x, "COMID", "toid", "GNIS_ID", "ArbolateSu"),
                      "GNIS_ID", "ArbolateSu", override_factor = 10, status = TRUE)

  expect_equal(z$levelpath, y$levelpath)
})

test_that("degenerate levelpath", {
  x <- structure(list(ID = c(203071, 202863, 202883, 205509, 203069, 202875, 942110034),
                      toID = c(202863, 202883, 205509, 203069, 202875, 942110034, 0),
                      nameID = c(630020286, 630020286, 630020286, 630020286, 630020286, 630020286, 630020286),
                      weight = c(14.962, 19.843, 33.047, 35.101, 44.702, 47.595, 58.583)),
                      row.names = c(NA, 7L), class = "data.frame")

  names(x) <- tolower(names(x))

  y <- add_levelpaths(x, "nameid", "weight")

  expect_equal(nrow(y), nrow(x))
})

test_that("from vignette works", {
  g <- sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom"))

  suppressWarnings(
  f <- add_toids(g) |>
    sf::st_cast("LINESTRING") |>
      dplyr::select(-ToNode, -FromNode, -Divergence, -FTYPE) |>
      sort_network(split = TRUE))

  f[["arbolate_sum"]] <- accumulate_downstream(f, "LENGTHKM")

  expect_error(
  add_levelpaths(f, "GNIS_NAME", "arbolate_sum", status = FALSE),
  "Problem aligning names")

  lp <- add_levelpaths(dplyr::select(f, "COMID", "toid", "GNIS_NAME", "arbolate_sum"),
                      "GNIS_NAME", "arbolate_sum", status = FALSE)

  expect_equal(names(lp),
               c("COMID", "toid", "levelpath_outlet_id", "topo_sort", "levelpath",
                 "geom", "GNIS_NAME", "arbolate_sum"))

  expect_equal(length(unique(lp$levelpath)),
               length(unique(g$LevelPathI)))

  expect_equal(length(unique(lp$levelpath)),
               length(unique(lp$levelpath_outlet_id)))

  # TODO:
  # plus <- add_plus_network_attributes(dplyr::select(fpath, comid, tocomid,
  #                                                   lengthkm, areasqkm,
  #                                                   nameID = gnis_id),
  #                                     status = FALSE)
  #
  # expect_s3_class(plus, "sf")
})

test_that("degenerate", {
  net <- structure(list(ID = 11000020, toID = 0, nameID = "constant",
                        lengthkm = 12.2243026760847, areasqkm = 54.2851667150928,
                        weight = 12.2243026760847, terminalID = 11000020),
                   row.names = 2938080L, class = "data.frame")

  er <- add_levelpaths(net, "nameID", "weight", 5)

  expect_equal(er$topo_sort, 1)

  expect_equal(er$levelpath, 1)
})

test_that("hr levelpath", {
  skip_on_cran()

  x <- readRDS(list.files(pattern = "hr.rds", full.names = TRUE, recursive = TRUE))

  x <- dplyr::select(x, -Permanent_Identifier)

  x <- add_toids(x)

  lp <- add_levelpaths(x, "GNIS_Name", "ArbolateSu", status = TRUE)

  # Same number of total flowlines
  expect_equal(length(unique(x$LevelPathI)), length(unique(lp$LevelPathI)))

  # follows a semi tricky mainstem the same as HR
  expect_equal(lp[lp$COMID == 15000500039693, ]$LevelPathI, lp[lp$COMID == 15000500039696, ]$LevelPathI)

})
