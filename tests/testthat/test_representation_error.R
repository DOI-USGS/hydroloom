# SPEC A-10: representation and dispatch errors carry one condition class,
# hydroloom_representation_error, with message text unchanged.

rep_class <- "hydroloom_representation_error"

test_that("dispatch errors carry the representation class", {

  plain <- data.frame(id = 1:3, name = c("a", "b", "c"))

  expect_error(sort_network(plain), class = rep_class,
    regexp = "sort_network() requires hy_topo.\n  Current input is: hy.", fixed = TRUE)

  expect_error(add_topo_sort(plain), class = rep_class,
    regexp = "add_topo_sort() requires hy_topo.", fixed = TRUE)

  expect_error(make_index_ids(plain), class = rep_class,
    regexp = "make_index_ids() requires hy_topo.", fixed = TRUE)

  expect_error(navigate_network_dfs(plain, 1), class = rep_class,
    regexp = "make_index_ids() requires hy_topo.", fixed = TRUE)

  # needs_node: no fromnode/tonode, and a repeated id
  expect_error(add_toids(plain), class = rep_class,
    regexp = "add_toids() requires hy_node.", fixed = TRUE)

  expect_error(add_toids(data.frame(id = c(1, 1, 2), fromnode = c(1, 1, 2),
    tonode = c(2, 2, 3), divergence = 0)), class = rep_class,
  regexp = "add_toids() requires hy_node.", fixed = TRUE)

  err <- tryCatch(sort_network(plain), error = function(e) e)

  expect_s3_class(err, c(rep_class, "error", "condition"), exact = TRUE)
  expect_null(conditionCall(err))

})

test_that("toid already present carries the representation class", {

  expect_error(add_toids(data.frame(id = 1:2, toid = c(2, 0))), class = rep_class,
    regexp = "This network already has toid (class: hy_topo).", fixed = TRUE)

  x <- hy(data.frame(id = 1:2, toid = c(2, 0), fromnode = 1:2, tonode = 2:3))

  expect_error(add_toids(hydroloom:::as_hy_node(x)), class = rep_class,
    regexp = "network already contains a toid attribute", fixed = TRUE)

})

test_that("hy_reverse on a non-hy object carries the representation class", {

  err <- tryCatch(suppressMessages(hy_reverse(data.frame(id = 1))),
    error = function(e) e)

  expect_s3_class(err, rep_class)
  expect_identical(conditionMessage(err), "must be an hy object")
  expect_identical(deparse(conditionCall(err)), "hy_reverse(data.frame(id = 1))")

})

test_that("check_hy_graph without toid carries the representation class", {

  expect_error(check_hy_graph(data.frame(id = 1:3, name = c("a", "b", "c"))),
    class = rep_class, regexp = "check_hy_graph requires toid.", fixed = TRUE)

})

test_that("constructor unique-id errors carry the representation class", {

  dup <- structure(data.frame(id = c(1, 1), toid = c(0, 0), fromnode = c(1, 1),
    tonode = c(2, 2)), class = c("hy", "data.frame"))

  expect_error(hydroloom:::new_hy_topo(dup), class = rep_class,
    regexp = "hy_topo requires unique id.", fixed = TRUE)

  expect_error(hydroloom:::new_hy_node(dup), class = rep_class,
    regexp = "hy_node requires unique id.", fixed = TRUE)

})

test_that("other errors do not carry the representation class", {

  x <- hy(data.frame(id = 1:2, toid = c(2, 0)))

  err <- tryCatch(make_index_ids(x, mode = "sideways"), error = function(e) e)

  expect_false(inherits(err, rep_class))

})
