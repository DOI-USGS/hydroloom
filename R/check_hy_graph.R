utils::globalVariables(c("n_out", "n_main", "frac_sum"))

required_atts_check_divergence <- c("id", "fromnode", "divergence")

#' Check hy Graph
#' @description check that a network graph doesn't contain localized loops
#' and, optionally, that its divergence attributes are internally consistent.
#' @param x data.frame network compatible with \link{hydroloom_names}.
#' @details
#'
#' Required attributes: `id`, `toid`
#'
#' For `divergence_check`: `id`, `fromnode`, `divergence` and,
#' conditionally, `divergence_fraction`.
#'
#' The divergence check enforces the attribute contract that
#' [accumulate_downstream()] documents and depends on: 0 indicates a
#' catchment that is not downstream of a divergence, 1 the primary path
#' downstream of a divergence, and 2 a diverted path. Total upstream
#' routing needs a complete and consistent `divergence` attribute to avoid
#' double counting through systems of diverted channels. Use
#' [add_divergence()] to derive the attribute where it is absent.
#'
#' Divergence checks, grouping features by shared `fromnode`:
#'
#' \enumerate{
#'   \item `divergence` is populated and in `{0, 1, 2}`.
#'   \item Every node with more than one outgoing feature has exactly one
#'         `divergence == 1`.
#'   \item No `divergence == 0` on a node with more than one outgoing
#'         feature.
#'   \item No `divergence` of 1 or 2 on a node with a single outgoing
#'         feature.
#' }
#'
#' A node may have any number of diverted paths, so more than one
#' `divergence == 2` at a node is valid and is not flagged.
#'
#' When a `divergence_fraction` attribute is present, three further checks
#' apply:
#'
#' \enumerate{
#'   \item `divergence_fraction` is populated and in `[0, 1]`.
#'   \item `divergence_fraction` sums to 1 across each `fromnode` group.
#'   \item No `divergence == 2` feature takes the entire flow.
#' }
#'
#' @param loop_check logical if TRUE, the entire network is walked from
#' top to bottom searching for loops. This loop detection algorithm visits
#' a node in the network only once all its upstream neighbors have been
#' visited. A complete depth first search is performed at each node, searching
#' for paths that lead to an already visited (upstream) node. This algorithm
#' is often referred to as "recursive depth first search".
#' @param divergence_check logical if TRUE, `divergence` and, where
#' present, `divergence_fraction` are checked for internal consistency as
#' described in details.
#' @param fraction_tol numeric tolerance used when checking that
#' `divergence_fraction` sums to 1 across a `fromnode` group.
#' @returns if no problems are found, returns TRUE. If problems are found,
#' problem rows with a row number added. Rows returned by the divergence
#' check carry a `divergence_issue` column naming the failed checks. Checks
#' run in order and the first one to find a problem returns, so a single
#' call reports one class of problem at a time.
#' @export
#' @seealso [accumulate_downstream()] for the routing that depends on this
#' contract and [add_divergence()] for deriving the attribute.
#' @examples
#' # notice that row 4 (id = 4, toid = 9) and row 8 (id = 9, toid = 4) is a loop.
#' test_data <- data.frame(id = c(1, 2, 3, 4, 6, 7, 8, 9),
#'   toid = c(2, 3, 4, 9, 7, 8, 9, 4))
#' check_hy_graph(test_data)
#'
#' # node 2 splits to a main path (id 2) and a diversion (id 3) -- valid
#' div_data <- data.frame(id = c(1, 2, 3, 4),
#'   toid = c(2, 4, 0, 0),
#'   fromnode = c(1, 2, 2, 3),
#'   tonode = c(2, 3, 4, 5),
#'   divergence = c(0, 1, 2, 0))
#' check_hy_graph(div_data, divergence_check = TRUE)
#'
#' # demote the main path and node 2 has no primary outlet
#' div_data$divergence[2] <- 2
#' check_hy_graph(div_data, divergence_check = TRUE)
#'
check_hy_graph <- function(x, loop_check = FALSE, divergence_check = FALSE,
                           fraction_tol = 1e-6) {

  if (!inherits(x, c("hy", "hy_flownetwork"))) {
    x <- hy(x)
  }

  if (divergence_check) {

    check <- check_divergence_internal(x, fraction_tol)

    if (!is.null(check)) return(check)

  }

  if (loop_check) {
    index_ids <- make_index_ids(x, mode = "both")

    starts <- index_ids$to$to_list$indid[index_ids$to$to_list$id %in% x$id[!x$id %in% x$toid]]

    check <- check_hy_graph_internal(index_ids, starts)

    check <- unlist(check)

    if (any(!is.na(check))) {
      return(filter(x, id %in% check))
    }

  }

  x <- merge(data.table(mutate(x, row = seq_len(n()))),
    data.table(rename(st_drop_geometry(x), toid_check = toid)),
    by.x = "toid", by.y = "id", all.x = TRUE)

  x <- as_tibble(x)

  check <- x$id == x$toid_check

  if (any(check, na.rm = TRUE)) {

    filter(x, check)

  } else {

    TRUE

  }

}

# Accumulate an issue label onto the rows named by `hit`.
add_divergence_issue <- function(issue, hit, msg) {

  hit <- which(hit)

  if (!length(hit)) return(issue)

  issue[hit] <- ifelse(nzchar(issue[hit]),
    paste(issue[hit], msg, sep = "; "), msg)

  issue

}

# Check divergence coding against the attribute contract documented in
# accumulate_downstream(). Returns NULL when clean, otherwise the problem
# rows of x with `row` and `divergence_issue` appended.
check_divergence_internal <- function(x, fraction_tol = 1e-6) {

  missing_atts <- setdiff(required_atts_check_divergence, names(x))

  if (length(missing_atts)) {
    stop("check_hy_graph: divergence_check requires attributes: ",
      paste(missing_atts, collapse = ", "), call. = FALSE)
  }

  d <- data.table(fromnode = x[[fromnode]],
    divergence = x[[divergence]])

  # Features with no fromnode are not at a node, so the group checks below
  # can't speak to them. data.table would otherwise pool them all into one
  # NA group and report a network-sized bogus divergence.
  at_node <- !is.na(d$fromnode)

  d[at_node, n_out := .N, by = fromnode]
  d[at_node, n_main := sum(divergence == 1, na.rm = TRUE), by = fromnode]

  issue <- rep("", nrow(d))

  issue <- add_divergence_issue(issue,
    is.na(d$divergence) | !d$divergence %in% c(0, 1, 2),
    "divergence not in {0, 1, 2}")

  # Flags the whole group: which member is miscoded is not knowable from
  # the counts alone, only that the group disagrees.
  issue <- add_divergence_issue(issue,
    at_node & d$n_out > 1 & d$n_main != 1,
    "node without exactly one divergence == 1")

  issue <- add_divergence_issue(issue,
    at_node & d$n_out > 1 & d$divergence %in% 0,
    "divergence == 0 at a divergence")

  issue <- add_divergence_issue(issue,
    at_node & d$n_out == 1 & d$divergence %in% c(1, 2),
    "divergence of 1 or 2 where flow does not split")

  if (divergence_fraction %in% names(x)) {

    d$divergence_fraction <- x[[divergence_fraction]]

    d[at_node, frac_sum := sum(divergence_fraction), by = fromnode]

    issue <- add_divergence_issue(issue,
      is.na(d$divergence_fraction) | d$divergence_fraction < 0 |
        d$divergence_fraction > 1,
      "divergence_fraction not in [0, 1]")

    issue <- add_divergence_issue(issue,
      at_node & abs(d$frac_sum - 1) > fraction_tol,
      "divergence_fraction does not sum to 1 at a node")

    issue <- add_divergence_issue(issue,
      d$divergence %in% 2 & d$divergence_fraction >= 1,
      "divergence == 2 takes all flow")

  }

  hit <- nzchar(issue)

  if (!any(hit)) return(NULL)

  out <- x[hit, , drop = FALSE]

  out$row <- which(hit)
  out$divergence_issue <- issue[hit]

  out

}

check_hy_outlets <- function(x, fix = FALSE) {

  if (!inherits(x, c("hy", "hy_flownetwork"))) {
    x <- hy(x)
  }

  # Type-mismatch detection: id and toid should agree on character vs not.
  # A genuine mismatch (e.g., numeric id with character toid) breaks outlet
  # detection because %in% comparisons coerce in surprising ways.
  if (inherits(x$id, "character") != inherits(x$toid, "character")) {
    warning("id and toid have incompatible types (one character, one not). ",
      "Outlet detection may be unreliable.", call. = FALSE)
  }

  if (fix) {

    # Canonicalize outlet markers to the reserved outlet value. This destroys
    # any unique-per-outlet identifiers in the table -- intentional under
    # explicit fix = TRUE.
    check <- is_outlet(x)

    if (any(check)) {
      x$toid[check] <- rep(get_outlet_value(x), sum(check))
    }

  }

  x

}

check_hy_graph_internal <- function(g, all_starts) {

  # used to track which path tops we need to go back to
  to_visit_queue <- fastqueue(missing_default = 0)

  lapply(all_starts, function(x) to_visit_queue$add(x))

  out_stack <- faststack()

  # to track where we've been
  visited_tracker <- rep(FALSE, ncol(g$to$to))

  # Set up the starting node we change node below so this just tracks for clarity
  node <- to_visit_queue$remove()

  # trigger for making a new path
  new_path <- FALSE

  if (pbapply::dopb()) {
    pb = txtProgressBar(0, ncol(g$to$to), style = 3)
    on.exit(close(pb))
  }
  n <- 0

  while (node > 0) {

    if (!visited_tracker[node])
      n <- n + 1

    # mark it as visited
    visited_tracker[node] <- TRUE

    if (!n %% 100 && pbapply::dopb())
      setTxtProgressBar(pb, n)

    # now look at what's downtream and add to a queue
    for (to in seq_len(g$to$lengths[node])) {

      # Add the next node to visit to the tracking vector
      if (g$to$to[to, node] != 0 && !visited_tracker[g$to$to[to, node]])
        to_visit_queue$add(g$to$to[to, node])

      # stops us from visiting a node again when we revisit
      # from another upstream path.
      g$to$to[to, node] <- 0

    }

    # go to the last element added in to_visit_queue
    node <- to_visit_queue$remove()

    # if nothing there, just increment to the next visit position
    # this indicates we hit a new path
    while ((node == 0 && to_visit_queue$size() > 0) ||
      # or if we are at a node that's already been visited, skip it.
      (node != 0 && visited_tracker[node])) {

      node <- to_visit_queue$remove()

    }

    node_temp <- node

    track <- 0
    while (node != 0 &&
      g$from$lengths[node] != 0 &&
      any(!visited_tracker[
        g$from$froms[seq(1, g$from$lengths[node]), node]])) {

      to_visit_queue$add(node)
      node <- to_visit_queue$remove()

      track <- track + 1
      if (track > to_visit_queue$size()) {
        warning("stuck in a loop at ", g$to$to_list$id[node_temp])
        out_stack$push(node_temp)

        visited_tracker[node] <- TRUE

        node <- to_visit_queue$remove()

        break
      }
    }

    check <- NULL

    if (node != 0 && !visited_tracker[node])
      check <- loop_search_dfs(g, node, visited_tracker)

    if (!is.null(check)) {
      message("found loop at ", g$to$to_list$id[check])
      warning("found a loop at ", g$to$to_list$id[check])
      out_stack$push(check)
    }

  }
  if (pbapply::dopb())
    setTxtProgressBar(pb, n)

  # if we got this far, Cool!
  unique(g$to$to_list$id[as.integer(out_stack$as_list())])

}

loop_search_dfs <- function(g, node, visited_tracker) {

  # stack to track stuff we need to visit
  to_visit_stack <- faststack(missing_default = 0)

  # while we still have nodes to check
  while (node != 0) {

    # means we hit a node that we already visited
    if (visited_tracker[node]) {
      return(node)
    }

    for (to in seq_len(g$to$lengths[node])) {
      to_visit_stack$push(g$to$to[to, node])
      g$to$to[to, node] <- 0
    }

    # grab the next node
    node <- to_visit_stack$pop()

    # if it's 0 grab the next node unless it's empty
    while (!node && to_visit_stack$size() > 0) {
      node <- to_visit_stack$pop()
    }

  }

}
