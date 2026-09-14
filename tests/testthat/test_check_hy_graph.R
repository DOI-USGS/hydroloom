test_that("base check_hy_graph", {
  test_data <- data.frame(id = c(1, 2, 3, 4, 6, 7, 8, 9),
    toid = c(2, 3, 4, 9, 7, 8, 9, 4))

  remove <- check_hy_graph(test_data)

  expect_true(all(c(4, 8) %in% remove$row))
})

test_that("divergence check leaves default behavior alone", {
  test_data <- data.frame(id = c(1, 2, 3, 4, 6, 7, 8, 9),
    toid = c(2, 3, 4, 9, 7, 8, 9, 4))

  expect_identical(check_hy_graph(test_data),
    check_hy_graph(test_data, divergence_check = FALSE))

  # a network with no divergence attributes is untouched when the check is off
  expect_true(all(c(4, 8) %in% check_hy_graph(test_data)$row))
})

test_that("divergence check requires its attributes", {
  expect_error(
    check_hy_graph(data.frame(id = 1:2, toid = c(2, 0)),
      divergence_check = TRUE),
    "requires attributes: fromnode, divergence")
})

test_that("valid divergence coding passes", {
  # node 2 splits into a main path (id 2) and a diversion (id 3)
  test_data <- data.frame(id = c(1, 2, 3, 4),
    toid = c(2, 4, 0, 0),
    fromnode = c(1, 2, 2, 3),
    tonode = c(2, 3, 4, 5),
    divergence = c(0, 1, 2, 0))

  expect_true(check_hy_graph(test_data, divergence_check = TRUE))

  # any number of diverted paths is valid -- only one main path is required
  fan <- data.frame(id = c(1, 2, 3, 4),
    toid = c(2, 0, 0, 0),
    fromnode = c(1, 2, 2, 2),
    tonode = c(2, 3, 4, 5),
    divergence = c(0, 1, 2, 2))

  expect_true(check_hy_graph(fan, divergence_check = TRUE))
})

test_that("divergence coding problems are found", {
  test_data <- data.frame(id = c(1, 2, 3, 4),
    toid = c(2, 4, 0, 0),
    fromnode = c(1, 2, 2, 3),
    tonode = c(2, 3, 4, 5),
    divergence = c(0, 1, 2, 0))

  # demoting the main path leaves node 2 with no primary outlet -- the whole
  # group is reported because the counts don't say which member is wrong
  no_main <- test_data
  no_main$divergence[2] <- 2

  check <- check_hy_graph(no_main, divergence_check = TRUE)

  expect_equal(check$row, c(2, 3))
  expect_true(all(grepl("exactly one divergence == 1", check$divergence_issue)))

  # undivided flag at a split
  at_split <- test_data
  at_split$divergence[3] <- 0

  check <- check_hy_graph(at_split, divergence_check = TRUE)

  expect_equal(check$row, 3)
  expect_match(check$divergence_issue, "divergence == 0 at a divergence")

  # divergence flag where flow does not split
  flagged <- test_data
  flagged$divergence[4] <- 1

  check <- check_hy_graph(flagged, divergence_check = TRUE)

  expect_equal(check$row, 4)
  expect_match(check$divergence_issue, "where flow does not split")

  # an incomplete divergence attribute breaks accumulate_downstream
  na_flag <- test_data
  na_flag$divergence[4] <- NA

  check <- check_hy_graph(na_flag, divergence_check = TRUE)

  expect_equal(check$row, 4)
  expect_match(check$divergence_issue, "not in \\{0, 1, 2\\}")
})

test_that("divergence_fraction is checked when present", {
  test_data <- data.frame(id = c(1, 2, 3, 4),
    toid = c(2, 4, 0, 0),
    fromnode = c(1, 2, 2, 3),
    tonode = c(2, 3, 4, 5),
    divergence = c(0, 1, 2, 0),
    divergence_fraction = c(1, 0.6, 0.4, 1))

  expect_true(check_hy_graph(test_data, divergence_check = TRUE))

  # fractions must sum to 1 across a fromnode group
  short <- test_data
  short$divergence_fraction[3] <- 0.2

  check <- check_hy_graph(short, divergence_check = TRUE)

  expect_equal(check$row, c(2, 3))
  expect_true(all(grepl("does not sum to 1", check$divergence_issue)))

  # tolerance is respected
  near <- test_data
  near$divergence_fraction[3] <- 0.4 + 1e-9

  expect_true(check_hy_graph(near, divergence_check = TRUE))
  expect_equal(nrow(check_hy_graph(near, divergence_check = TRUE,
    fraction_tol = 1e-12)), 2)

  # out of range values are reported alongside the sum they break
  over <- test_data
  over$divergence_fraction[3] <- 1.4

  check <- check_hy_graph(over, divergence_check = TRUE)

  expect_true(3 %in% check$row)
  expect_match(check$divergence_issue[check$row == 3],
    "divergence_fraction not in \\[0, 1\\]")

  # a diversion may not take the whole flow
  all_flow <- test_data
  all_flow$divergence_fraction[2] <- 0
  all_flow$divergence_fraction[3] <- 1

  check <- check_hy_graph(all_flow, divergence_check = TRUE)

  expect_equal(check$row, 3)
  expect_match(check$divergence_issue, "takes all flow")
})

test_that("features with no fromnode are not pooled into one node", {
  # NA fromnode would otherwise group together and report a bogus divergence
  test_data <- data.frame(id = c(1, 2, 3),
    toid = c(0, 0, 0),
    fromnode = c(NA, NA, NA),
    tonode = c(1, 2, 3),
    divergence = c(0, 0, 0))

  expect_true(check_hy_graph(test_data, divergence_check = TRUE))
})

test_that("loop check", {
  old_opts <- pbapply::pboptions()
  pbapply::pboptions(type = "none")

  # makes a loop of three features 3 -> 5 -> 4 -> 3
  test_data <- data.frame(id = c(1, 2, 3, 4, 5, 5, 6),
    toid = c(3, 5, 5, 3, 4, 6, 0))

  g <- make_index_ids(test_data, mode = "both")

  suppressMessages(
    expect_warning(expect_warning(expect_warning(hydroloom:::check_hy_graph_internal(g, c(1, 2)),
      "loop"), "loop"), "loop"))

  suppressMessages(suppressWarnings(remove <- check_hy_graph(test_data, loop_check = TRUE)))

  expect_equal(remove$id, 3)
  expect_equal(remove$toid, 5)

  test_data <- data.frame(id = c(1, 1, 2, 3, 4, 5, 6, 6, 7, 8),
    toid = c(2, 3, 4, 7, 5, 6, 2, 7, 8, 0))

  g <- make_index_ids(test_data, mode = "both")

  suppressWarnings(expect_warning(hydroloom:::check_hy_graph_internal(g, 1),
    "loop"))

  suppressWarnings(remove <- check_hy_graph(test_data, loop_check = TRUE))

  expect_equal(remove$id, 2)
  expect_equal(remove$toid, 4)

  test_data <- data.frame(id = c(1, 1, 3, 2, 4, 5),
    toid = c(3, 2, 4, 4, 5, 0))

  g <- make_index_ids(test_data, mode = "both")

  check <- hydroloom:::check_hy_graph_internal(g, 1)

  expect_equal(check, numeric())

  expect_true(check_hy_graph(test_data, loop_check = TRUE))

  pbapply::pboptions(old_opts)
})

test_that("more check", {
  g <- sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom"))
  f <- add_toids(g)
  expect_true(check_hy_graph(f, loop_check = TRUE))
})

test_that("big_check", {
  g <- readRDS(list.files(pattern = "loop.rds", recursive = TRUE, full.names = TRUE))

  g$toid[!g$toid %in% g$id] <- ""

  g$toid[g$id == "31325125"] <- ""

  gi <- make_index_ids(g)

  expect_true(hydroloom::check_hy_graph(g, loop_check = TRUE))

  g <- readRDS(list.files(pattern = "loop2.rds", recursive = TRUE, full.names = TRUE))

  g$toid[!g$toid %in% g$id] <- ""

  g$toid[g$id == "{9C33A204-E0C5-4047-A81E-23647ED06E2A}"] <- ""

  # found all these loops with some investigation of output but the behavior isn't great
  # can use this to improve hanlding when loops are found
  g$toid[g$id == "{4E7E9D38-373D-4FDA-A920-00EA2AE826EA}"] <- "{89B98684-C80B-4BFB-ADE7-604E87539981}"
  g$toid[g$id == "{B3F20B3F-9141-4009-8935-4930DE818192}"] <- "{89B98684-C80B-4BFB-ADE7-604E87539981}"

  g$toid[g$id == "{99E532FB-6ADD-4DD2-B10F-E0A687590428}"] <- "{0874C471-4F58-471B-B2A0-6140D6332E7D}"

  g$toid[g$id == "{782C5B65-07A0-48BF-9A84-969CEF510DFC}"] <- "{9662E643-E8B7-4B94-B23A-4141E486D5B3}"

  g$toid[g$id == "{2BB601E0-6711-4B5E-92F3-852227D46BE0}"] <- "{B9A43856-3A49-4DF7-8B05-51A26781F539}"

  g$toid[g$id == "{4E7E9D38-373D-4FDA-A920-00EA2AE826EA}"] <- "{B9A43856-3A49-4DF7-8B05-51A26781F539}"
  g$toid[g$id == "{B3F20B3F-9141-4009-8935-4930DE818192}"] <- "{B9A43856-3A49-4DF7-8B05-51A26781F539}"

  g <- distinct(g)

  expect_true(hydroloom::check_hy_graph(g, loop_check = TRUE))

})

test_that("recombine", {
  # this could be used as a test later -- the downstream recombinations look like
  # loops unless you track parents.

  test_data <- data.frame(id = c(1, 2, 2, 3, 4, 4, 5, 6, 7, 9, 8, 10),
    toid = c(2, 3, 7, 4, 5, 8, 6, 0, 9, 5, 10, 6))

  #nolint start
  # igraph::plot.igraph(igraph::graph_from_data_frame(test_data))
  #nolint end

  g <- make_index_ids(test_data, mode = "both")

  expect_equal(hydroloom:::check_hy_graph_internal(g, which(g$to$to_list$id == 1)),
    numeric())
})
