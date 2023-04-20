test_that("get_pfaf", {

  work_dir <- nhdplusTools::nhdplusTools_data_dir()

  source(system.file("extdata/nhdplushr_data.R", package = "nhdplusTools"))

  expect_message(hy(hr_data$NHDFlowline), "defaulting to comid rather than permanent_identifier")

  hr_data$NHDFlowline <- dplyr::select(hr_data$NHDFlowline, -Permanent_Identifier)

  fl <- hy(hr_data$NHDFlowline) |> add_toids()

  fl <- dplyr::select(fl, id, toid, da_sqkm)

  # level according to drainage area
  fl$name <- ""
  fl$total_da_sqkm <- accumulate_downstream(fl, "da_sqkm")
  fl <- add_levelpaths(fl, "name", "total_da_sqkm")
  pfaf <- add_pfafstetter(fl, max_level = 2)

  expect_equal(pfaf[pfaf$id == 15000500028335,	]$pf_level_1, 5)
  expect_equal(pfaf[pfaf$id == 15000500028335,	]$pf_level_2, 51)

  pfaf <- add_pfafstetter(fl, max_level = 4)

  expect_equal(sum(!is.na(c(pfaf$pf_level_1, pfaf$pf_level_4))), 4496)

  expect_equal(pfaf$pf_level_3[pfaf$id == 15000500061836], 611)

  expect_equal(pfaf$pf_level_3[pfaf$id == 15000500028338], 591)
  expect_equal(pfaf$pf_level_3[pfaf$id == 15000500050711], 592)
  expect_equal(pfaf$pf_level_3[pfaf$id == 15000500028337], 593)

  expect_equal(pfaf$pf_level_3[pfaf$id == 15000500072804], 151)
  expect_equal(pfaf$pf_level_4[pfaf$id == 15000500072804], 1511)

  expect_equal(pfaf$pf_level_3[pfaf$id == 15000500084318], 181)
  expect_equal(pfaf$pf_level_3[pfaf$id == 15000500028332], 161)

  fl <- sf::read_sf(system.file("extdata", "walker.gpkg", package = "hydroloom"))

  fl <- add_toids(hy(fl))

  fl <- dplyr::select(fl, id, toid, da_sqkm)

  # level according to drainage area
  fl$name <- ""
  fl$total_da_sqkm <- accumulate_downstream(fl, "da_sqkm")
  fl <- add_levelpaths(fl, "name", "total_da_sqkm")

  pfaf <- add_pfafstetter(fl, max_level = 2)

  expect_equal(sum(!is.na(pfaf$pf_level_2)), 57)
})
