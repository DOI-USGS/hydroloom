# source(system.file("extdata", "sample_flines.R", package = "nhdplusTools"))
#
# x <- sf::st_drop_geometry(sample_flines[, 1:40])
#
# x <- dplyr::select(x, COMID, LevelPathI, Hydroseq, Pathlength,
#                    DnHydroseq, DnMinorHyd, DnLevelPat, LENGTHKM, TerminalFl,
#                    Divergence, FromNode, ToNode, AreaSqKM, TotDASqKM)
#
# saveRDS(x, "tests/testthat/data/network.rds")

x <- readRDS(list.files(pattern = "network.rds", recursive = TRUE, full.names = TRUE))

test_that("get_DM works normal", {
  expect_error(navigate_hydro_network(dplyr::select(x, -COMID), mode = "DM"),
               "DM requires.*")

  expect_error(navigate_hydro_network(mode = "BK"),
               "must choose mode input from: 'UM', 'DM', 'UT', 'DD'")

  expect_error(navigate_hydro_network(mode = "DD"), "DD requires.*")

  result <- navigate_hydro_network(x, 11689050, "DM")
  expect_equal(length(result), 26)
})

test_that("get_DM works short", {
  result <- navigate_hydro_network(x, 11690570, "DM")
  expect_equal(length(result), 6)
})

test_that("get_DM works for no divergence", {
  result <- navigate_hydro_network(x, 11688810, "DM")
  expect_true(!11688828 %in% result)
  expect_equal(length(result), 35)
})

test_that("get_DM works upstream of diversion", {
  result <- navigate_hydro_network(x, 11689280, "DM")
  expect_true(!11689758 %in% result)
  expect_true(11689286 %in% result)
  expect_equal(length(result), 29)
})

test_that("get_DM with distance 0 returns 1 comid", {
  result <- navigate_hydro_network(x, 11688810, "DM", distance = 0)
  expect_equal(length(result), 1)
})

test_that("get_DM with distance 2 returns specific COMIDs", {
  result <- navigate_hydro_network(x, 11688810, "DM", distance = 2)
  expect_equal(length(result), 3)
  expect_true(all(c(11688810, 11688826, 11688884) %in% result))

})

test_that("get_DM with distance big returns specific same as no distance", {
  result <- navigate_hydro_network(x, 11688810, "DM", distance = 999)
  result2 <- navigate_hydro_network(x, 11688810, "DM")
  expect_equal(result,  result2)
})

test_that("get_DM works upstream of diversion", {
  result <- navigate_hydro_network(x, 11689280, "DM")
  expect_true(!11689758 %in% result)
  expect_true(11689286 %in% result)
  expect_equal(length(result), 29)
})

test_that("get_UM works short", {
  result <- navigate_hydro_network(x, 11689050, "UM")
  expect_equal(length(result), 18)
})

test_that("get_UM works long", {
  result <- navigate_hydro_network(x, 11690570, "UM")
  expect_equal(length(result), 80)
})

test_that("get_UM returns 1 for distance 0", {
  result <- navigate_hydro_network(x, 11690570, "UM", distance = 0)
  expect_equal(length(result), 1)
})

test_that("get_UM returns a certain length for given distance", {
  result <- navigate_hydro_network(x, 11690570, "UM", distance = 10)
  expect_equal(length(result), 12)
})

test_that("get_UT works", {
  result <- navigate_hydro_network(x, 11687180, "UT")
  expect_equal(length(result), 5)
  result <- navigate_hydro_network(x, 11687224, "UT")
  expect_equal(length(result), 7)

  test_error <- rbind(x, x)

  expect_error(navigate_hydro_network(test_error, 11687224, "UT"),
               "Found duplicate id for starting catchment. Duplicate rows in network")
})

test_that("get_UT works with distance", {
  result <- navigate_hydro_network(x, 11689276, "UT", distance = 0)
  expect_equal(result, 11689276)
  result <- navigate_hydro_network(x, 11689276, "UT", distance = 3)
  expect_equal(length(result), 6)
})

test_that("get_UT works with distance specific", {
  result <- navigate_hydro_network(x, 11687180, "UT", distance = 2)
  expect_equal(length(result), 3)
})

test_that("get_UT returns diverted paths.", {
  result <- navigate_hydro_network(x, 11690184, "UT")
  expect_true(all(c(11689276, 11690200) %in% result),
              "missing a diverted or main path")
})

test_that("get_DD works with two divergences", {
  result <- navigate_hydro_network(x, 11689316, "DD")
  expect_true(all(c(11689294, 11690224, 11689268, 11689758, 11689276) %in% result))
  expect_equal(length(result), 43)
})

test_that("get_DD works", {
  result <- navigate_hydro_network(x, 11688810, "DD")
  expect_true(11688810 %in% result)
  expect_true(11688828 %in% result)
  expect_equal(length(result), 36)
})

test_that("get_DD works upstream of diversion", {
  result <- navigate_hydro_network(x, 11689280, "DD")
  expect_true(11689758 %in% result)
  expect_true(11689286 %in% result)
  expect_equal(length(result), 33)
})

test_that("get_DD with distance 0.2 returns 3", {
  result <- navigate_hydro_network(x, 11688810, "DD", distance = 0.5)
  expect_equal(length(result), 3)

  result <- navigate_hydro_network(x, 11688810, "DD", distance = 0.1)
  expect_equal(length(result), 1)
})

test_that("get_DD with distance 2 returns 4 specific", {
  result <- navigate_hydro_network(x, 11688810, "DD", distance = 2)
  expect_equal(length(result), 4)
  expect_true(all(c(11688810, 11688826, 11688828, 11688884) %in% result))
})

test_that("get_DM works if missing the outlet", {
  x_borkd <- dplyr::filter(x, TerminalFl == 0)
  result <- navigate_hydro_network(x_borkd, 11688810, "DM")
  expect_equal(length(result), 34)
})

