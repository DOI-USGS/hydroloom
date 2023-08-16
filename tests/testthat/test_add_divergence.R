test_that("add_return_diversion", {
  x <- sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom"))

  x <- hy(x)

  x <- add_return_divergence(x)

  expect_equal(sum(x$return_divergence == x$RtnDiv), 745)
})

test_that("complex split", {
  # these test if junctions with more than on upstream are handled.
  # base case
  x <- data.frame(id = c(1, 2, 3, 4, 5, 6, 7),
            fromnode = c(1, 2, 3, 3, 4, 5, 6),
              tonode = c(3, 3, 4, 5, 6, 6, 7),
            name = c("", "", "", "", "", "", ""),
            type = c(1, 1, 1, 1, 1, 1, 1))

  x <- add_divergence(x, 7, c(), name_attr = "name", type_attr = "type", major_types = 1)

  expect_equal(x$divergence[c(3,4)], c(1,2))

  # the rest of these switch from the base case
  # one matching name use it.
  x <- data.frame(id = c(1, 2, 3, 4, 5, 6, 7),
                  fromnode = c(1, 2, 3, 3, 4, 5, 6),
                  tonode = c(3, 3, 4, 5, 6, 6, 7),
                  name = c("test", "test2", "bbb", "test2", "", "", ""),
                  type = c(1, 1, 1, 1, 1, 1, 1))

  x <- add_divergence(x, 7, c(), name_attr = "name", type_attr = "type", major_types = 1)

  expect_equal(x$divergence[c(3,4)], c(2, 1))

  # one name at all
  x <- data.frame(id = c(1, 2, 3, 4, 5, 6, 7),
                  fromnode = c(1, 2, 3, 3, 4, 5, 6),
                  tonode = c(3, 3, 4, 5, 6, 6, 7),
                  name = c("test2", "", "", "test2", "", "", ""),
                  type = c(1, 1, 1, 1, 1, 1, 1))

  x <- add_divergence(x, 7, c(), name_attr = "name", type_attr = "type", major_types = 1)

  expect_equal(x$divergence[c(3,4)], c(2, 1))

  # major type
  x <- data.frame(id = c(1, 2, 3, 4, 5, 6, 7),
                  fromnode = c(1, 2, 3, 3, 4, 5, 6),
                  tonode = c(3, 3, 4, 5, 6, 6, 7),
                  name = c("test1", "test2", "test1", "test2", "", "", ""),
                  type = c(1, 2, 2, 1, 1, 1, 1))

  x <- add_divergence(x, 7, c(), name_attr = "name", type_attr = "type", major_types = 1)

  expect_equal(x$divergence[c(3,4)], c(2, 1))
})

test_that("add down main", {

  f <- system.file("extdata/coastal_example.gpkg", package = "hydroloom")

  g <- sf::read_sf(f)
  g <- g[g$FTYPE != "Coastline", ]

  outlets <- g$COMID[!g$ToNode %in% g$FromNode]

  d <- dplyr::select(g, COMID, gnis_id, FTYPE,
                     FromNode, ToNode)

  d <- add_divergence(d,
                      coastal_outlet_ids = outlets,
                      inland_outlet_ids = c(),
                      name_attr = "gnis_id",
                      type_attr = "FTYPE",
                      major_types = c("StreamRiver", "ArtificialPath", "Connector"))

  expect_equal(d$COMID, g$COMID)
  expect_equal(d$divergence, g$Divergence)

  expect_equal(d$divergence[d$COMID == 2544459], 2)
  expect_equal(d$divergence[d$COMID == 2544461], 1)

})

test_that("base down_level", {

  a <- dplyr::tibble(
    id = c("00BE2E87-F8A4-4D53-8CE6-E192469D86EE", "00BE2E87-F8A4-4D53-8CE6-E192469D86EE"),
    toid = c("2DAE89DC-2C4B-43A1-AB19-E4BAA997C9D2", "72498570-8338-400C-A167-EB3AA589F18B"),
    coastal = c(TRUE, TRUE),
    name_att = c("Stuhini Creek", "Stuhini Creek"),
    ftype = c(558L, 558L),
    dn_name_att = c(NA, "Stuhini Creek"),
    dn_ftype = c(558L, 558L),
    major_type = c(TRUE, TRUE),
  )

  testthat::expect_equal(hydroloom:::down_level(a), "72498570-8338-400C-A167-EB3AA589F18B")

  b <- dplyr::tibble(
    id = c("120497331", "120497331"),
    toid = c("120513267", "120497186"),
    coastal = c(TRUE, TRUE),
    name_att = c(NA_character_, NA_character_),
    ftype = c(460L, 460L),
    dn_name_att = c(NA, "Carroll Creek"),
    dn_ftype = c(460L, 460L),
    major_type = c(TRUE, TRUE),
  )

  testthat::expect_equal(hydroloom:::down_level(b), "120497186")

  c <- dplyr::tibble(
    id = c("67193536", "67193536"),
    toid = c("67194746", "67194750"),
    coastal = c(FALSE, FALSE),
    name_att = c("East Fork Tsivat River", "East Fork Tsivat River"),
    ftype = c(460L, 460L),
    dn_name_att = c(NA, "East Fork Tsivat River"),
    dn_ftype = c(460L, 460L),
    major_type = c(TRUE, TRUE),
  )

  testthat::expect_equal(hydroloom:::down_level(c), "67194750")

  d <- dplyr::tibble(
    id = c("{0691ff49-3167-4387-9d59-bd0e69f23ba0}", "{0691ff49-3167-4387-9d59-bd0e69f23ba0}"),
    toid = c("{6cc577b7-98b2-4352-9edc-5ec342e93078}", "{a1654838-6109-47b2-b5fd-07d329ff502b}"),
    coastal = c(TRUE, TRUE),
    name_att = c(NA_character_, NA_character_),
    ftype = c(336L, 336L),
    dn_name_att = c(NA_character_, NA_character_),
    dn_ftype = c(460L, 336L),
    major_type = c(TRUE, FALSE),
  )

  testthat::expect_equal(hydroloom:::down_level(d), "{6cc577b7-98b2-4352-9edc-5ec342e93078}")

  e <- dplyr::tibble(
    id = c("{3BBADBC2-10CD-42C4-963E-2CC3EBCAB96B}", "{3BBADBC2-10CD-42C4-963E-2CC3EBCAB96B}"),
    toid = c("00", "102984973"),
    coastal = c(TRUE, TRUE),
    name_att = c("South Fork Craig River", "South Fork Craig River"),
    ftype = c(460L, 460L),
    dn_name_att = c("South Fork Craig River", "South Fork Craig River"),
    dn_ftype = c(460L, 460L),
    major_type = c(TRUE, TRUE),
  )

  testthat::expect_equal(hydroloom:::down_level(e), "00")

  f <- dplyr::tibble(
    id = c("1", "1"),
    toid = c("5", "4"),
    coastal = c(TRUE, FALSE),
    name_att = c("South Fork Craig River", "South Fork Craig River"),
    ftype = c(460L, 460L),
    dn_name_att = c("South Fork Craig River", "South Fork Craig River"),
    dn_ftype = c(460L, 460L),
    major_type = c(TRUE, TRUE),
  )

  testthat::expect_equal(hydroloom:::down_level(f), "5")

  g <- dplyr::tibble(
    id = c("1", "1"),
    toid = c("5", "4"),
    coastal = c(TRUE, FALSE),
    name_att = c(NA_character_, NA_character_),
    ftype = c(460L, 460L),
    dn_name_att = c("South Fork Craig River", "check"),
    dn_ftype = c(460L, 460L),
    major_type = c(FALSE, FALSE),
  )

  testthat::expect_equal(hydroloom:::down_level(g), "5")

  h <- dplyr::tibble(
    id = c("1", "1"),
    toid = c("5", "4"),
    coastal = c(FALSE, FALSE),
    name_att = c(NA_character_, NA_character_),
    ftype = c(460L, 460L),
    dn_name_att = c("South Fork Craig River", NA_character_),
    dn_ftype = c(460L, 460L),
    major_type = c(TRUE, TRUE),
  )

  testthat::expect_equal(hydroloom:::down_level(h), "5")

  i <- dplyr::tibble(
    id = c("1", "1"),
    toid = c("5", "4"),
    coastal = c(FALSE, FALSE),
    name_att = c(NA_character_, NA_character_),
    ftype = c(460L, 460L),
    dn_name_att = c(NA_character_, NA_character_),
    dn_ftype = c(460L, 460L),
    major_type = c(TRUE, FALSE),
  )

  testthat::expect_equal(hydroloom:::down_level(i), "5")

  j <- dplyr::tibble(
    id = c("1", "1"),
    toid = c("5", "4"),
    coastal = c(FALSE, FALSE),
    name_att = c(NA_character_, NA_character_),
    ftype = c(460L, 460L),
    dn_name_att = c("test", NA_character_),
    dn_ftype = c(460L, 460L),
    major_type = c(FALSE, FALSE),
  )

  testthat::expect_equal(hydroloom:::down_level(j), "5")

  k <- dplyr::tibble(
    id = c("1", "1"),
    toid = c("5", "4"),
    coastal = c(FALSE, FALSE),
    name_att = c(NA_character_, NA_character_),
    ftype = c(460L, 460L),
    dn_name_att = c(NA_character_, NA_character_),
    dn_ftype = c(460L, 460L),
    major_type = c(FALSE, FALSE),
  )

  testthat::expect_equal(hydroloom:::down_level(k), "4")

  k <- dplyr::tibble(
    id = c("146073319", "146073319"),
    name_att = c("00246611", "00246611"),
    type_att = c(336L, 336L),
    toid = c("7ff2749a-869f-480f-a092-cbe3a4446ef0", "6f0fb7e8-b38c-4252-a9a2-3865d94987d5"),
    dn_name_att = c("00246611", "00249685"),
    dn_type_att = c(336L, 336L),
    coastal = c(FALSE, FALSE),
    major_type = c(FALSE, FALSE),
  )

  testthat::expect_equal(hydroloom:::down_level(k), "7ff2749a-869f-480f-a092-cbe3a4446ef0")

})

test_that("winnow works with name count", {
  x_orig <- dplyr::tibble(
    id = c("167280297", "167280300", "167280301", "167282662"),
    fromnode = c(41971, 43610, 41973, 41971),
    tonode = c(41968, 41971, 41971, 43712),
    name_att = c("00881819", "00875875", "00881819", "00875875"),
    type_att = c(558L, 558L, 558L, 558L),
    direction = c(709L, 709L, 709L, 709L),
  ) |>
    structure(
      class = c("hy", "tbl_df", "tbl", "data.frame"),
      orig_names = c(
        id = "id", fromnode = "fromnode", tonode = "tonode", gnis_id = "gnis_id",
        ftype = "feature_type", direction = "direction"
      )
    )

  n <- 41971

  major_types <- c(460, 558, 334)

  name_count <- c("00875875" = 38L, "00881819" = 68L) |>
    structure(
      dim = 2L,
      dimnames = list(c("00875875", "00881819")) |>
        structure(names = ""),
      class = "table"
    )

  expect_equal(hydroloom:::winnow_upstream(n, x_orig, major_types, name_count)$name_att[1],
               "00881819")

  unlink("divergence_checks.txt")
})
