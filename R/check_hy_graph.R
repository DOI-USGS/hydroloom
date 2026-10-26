utils::globalVariables(c("n_out", "n_main", "frac_sum"))

required_atts_check_divergence <- c("id", "fromnode", "divergence")

#' Check hy Graph
#' @description check that a network graph doesn't contain cycles
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
#' A cycle is a set of flowlines in which water leaving any one of them can,
#' by following `toid`, return to it: the strongly connected components of
#' the `id` to `toid` graph that hold more than one flowline, plus any
#' flowline whose `toid` is its own `id`. Every network is checked for
#' cycles.
#'
#' @param divergence_check logical if TRUE, `divergence` and, where
#' present, `divergence_fraction` are checked for internal consistency as
#' described in details.
#' @param fraction_tol numeric tolerance used when checking that
#' `divergence_fraction` sums to 1 across a `fromnode` group.
#' @returns if no problems are found, returns TRUE. If problems are found,
#' problem rows with a row number added. Rows returned by the divergence
#' check carry a `divergence_issue` column naming the failed checks. Rows
#' returned by the cycle check are every row whose `id` and `toid` lie on a
#' common cycle, in input order, with `component` (which cycle, numbered from
#' 1 in byte order of each cycle's least identifier compared as text) and
#' `reciprocal` (`TRUE` when the cycle has exactly two flowlines). Checks
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
check_hy_graph <- function(x, divergence_check = FALSE, fraction_tol = 1e-6) {

  if (!inherits(x, c("hy", "hy_flownetwork"))) {
    x <- hy(x)
  }

  if (divergence_check) {

    check <- check_divergence_internal(x, fraction_tol)

    if (!is.null(check)) return(check)

  }

  check_cycles_internal(x)

}

# Rows of x whose id and toid lie on a common cycle, with `row`, `component`,
# and `reciprocal` appended (D-8); TRUE when there is none. Builds its own
# CSR index: make_index_ids() on a hy_topo calls check_hy_graph().
check_cycles_internal <- function(x) {

  if (!"toid" %in% names(x)) stop("check_hy_graph requires toid. ", hy_guidance_topo,
    call. = FALSE)

  ids <- unique(x$id)

  from <- match(x$id, ids)
  to <- match(x$toid, ids, incomparables = NA)

  edge <- which(!is.na(to))
  edge <- edge[order(from[edge], method = "radix")]

  indptr <- c(0L, cumsum(tabulate(from[edge], nbins = length(ids))))

  comp <- scc_labels(indptr, to[edge])

  on_cycle <- !is.na(to) & comp[from] == comp[to]
  on_cycle[is.na(on_cycle)] <- FALSE

  if (!any(on_cycle)) return(TRUE)

  hit <- which(on_cycle)
  hit_comp <- comp[from[hit]]

  # components numbered by byte order of their least identifier (D-1)
  key <- as.character(x$id[hit])
  o <- order(key, method = "radix")
  comp_order <- hit_comp[o][!duplicated(hit_comp[o])]

  size <- tabulate(comp, nbins = max(comp))

  out <- x[hit, , drop = FALSE]

  out$row <- hit
  out$component <- match(hit_comp, comp_order)
  out$reciprocal <- size[hit_comp] == 2L

  classify_hy(out)

}

# Strongly connected components of a CSR graph (Tarjan 1972), iterative.
# indptr: 0-based offsets, length n + 1; indices: 1-based positions. Returns
# one component label per position. 0 in index marks an unvisited position
# (K-4, 1-based).
scc_labels <- function(indptr, indices) {

  n <- length(indptr) - 1L

  index <- integer(n)
  low <- integer(n)
  on_stack <- logical(n)
  comp <- integer(n)

  stack <- integer(n)
  sp <- 0L

  call_node <- integer(n)
  call_edge <- integer(n)
  cp <- 0L

  counter <- 0L
  n_comp <- 0L

  for (s in seq_len(n)) {

    if (index[s] != 0L) next

    counter <- counter + 1L
    index[s] <- counter
    low[s] <- counter
    sp <- sp + 1L
    stack[sp] <- s
    on_stack[s] <- TRUE
    cp <- 1L
    call_node[cp] <- s
    call_edge[cp] <- indptr[s]

    while (cp > 0L) {

      v <- call_node[cp]
      e <- call_edge[cp]

      if (e < indptr[v + 1L]) {

        call_edge[cp] <- e + 1L
        w <- indices[e + 1L]

        if (index[w] == 0L) {
          counter <- counter + 1L
          index[w] <- counter
          low[w] <- counter
          sp <- sp + 1L
          stack[sp] <- w
          on_stack[w] <- TRUE
          cp <- cp + 1L
          call_node[cp] <- w
          call_edge[cp] <- indptr[w]
        } else if (on_stack[w] && index[w] < low[v]) {
          low[v] <- index[w]
        }

      } else {

        if (low[v] == index[v]) {
          n_comp <- n_comp + 1L
          repeat {
            w <- stack[sp]
            sp <- sp - 1L
            on_stack[w] <- FALSE
            comp[w] <- n_comp
            if (w == v) break
          }
        }

        cp <- cp - 1L

        if (cp > 0L) {
          u <- call_node[cp]
          if (low[v] < low[u]) low[u] <- low[v]
        }

      }

    }

  }

  comp

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
