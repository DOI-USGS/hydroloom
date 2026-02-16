# WRITTEN WITH HELP FROM CLAUDE

#' Get Bridge Flowlines
#' @description Identifies bridge flowlines (cut edges) in the network.
#' Bridge flowlines are those whose removal would disconnect the network.
#' Flowlines are edges in the underlying node graph, so bridge detection
#' correctly identifies the sole-path flowlines that separate parts of the
#' network -- including flowlines within diversion systems that do not rejoin.
#'
#' @param x data.frame network compatible with \link{hydroloom_names}.
#' @param quiet logical quiet messages?
#' @details
#'
#' Required attributes: `id`, `toid`
#'
#' @returns vector of flowline ids that are bridge flowlines in the network
#' @name get_bridge_flowlines
#' @export
#' @examples
#'
#' x <- data.frame(
#'   id = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
#'   toid = c(2, 3, 4, 5, 0, 7, 8, 9, 4)
#' )
#'
#' # 1 -> 2 -> 3 -> 4 -> 5
#' #               ^
#' #               |
#' # 6 -> 7 -> 8 -> 9
#' #
#' # Dendritic tree: all flowlines are bridges
#' get_bridge_flowlines(x)
#'
get_bridge_flowlines <- function(x, quiet = FALSE) {
  UseMethod("get_bridge_flowlines")
}

#' @name get_bridge_flowlines
#' @export
get_bridge_flowlines.data.frame <- function(x, quiet = FALSE) {
  get_bridge_flowlines(hy(x), quiet = quiet)
}

#' @name get_bridge_flowlines
#' @export
get_bridge_flowlines.hy <- function(x, quiet = FALSE) {

  node_topo <- make_nondendritic_topology(x)

  n_flowlines <- nrow(node_topo)

  if (n_flowlines == 0) {
    return(vector(mode = class(x$id), length = 0))
  }

  result <- format_index_ids_internal(make_adj_dt(node_topo))

  # find_bridges expects 0 sentinels; format_index_ids_internal pads with NA
  adj_matrix <- result$to
  adj_matrix[is.na(adj_matrix)] <- 0L
  edge_id_matrix <- result$edge_id
  edge_id_matrix[is.na(edge_id_matrix)] <- 0L

  bridge_indices <- find_bridges(
    adj_matrix, result$lengths, edge_id_matrix, n_flowlines, quiet = quiet
  )

  if (length(bridge_indices) == 0) {
    return(vector(mode = class(x$id), length = 0))
  }

  # Map back to original flowline ids
  node_topo$id[bridge_indices]
}

#' Find Bridge Edges in an Undirected Graph
#'
#' Identifies all bridge edges (cut edges) in an undirected graph using an
#' iterative implementation of Tarjan's algorithm. A bridge edge is an edge
#' whose removal disconnects the graph.
#'
#' @param adj_matrix Integer matrix of dimensions m x n, where n is the number
#'   of nodes and m is the maximum node degree. Each column represents a node,
#'   and the rows contain the indices of its neighbors. Unused entries should be 0.
#' @param lens Integer vector of length n indicating the number of neighbors
#'   for each node. For node i, only `adj_matrix[1:lens[i], i]` contains valid
#'   neighbor indices.
#' @param edge_id_matrix Integer matrix with same dimensions as adj_matrix.
#'   Each entry contains the edge index corresponding to the adjacency entry.
#'   Used to identify which edges are bridges and to correctly handle multi-edges.
#' @param n_edges Integer, total number of edges in the graph.
#'
#' @return Integer vector of edge indices that are bridges. Returns an empty
#'   integer vector if no bridges exist.
#'
#' @details
#' Uses iterative DFS with an explicit stack. Handles multi-edges correctly
#' by tracking the parent edge ID rather than just the parent node -- two
#' parallel edges between the same nodes are neither bridges, but would be
#' misidentified if only the parent node were tracked.
#'
#' Time complexity is O(V + E) where V is the number of vertices and E is the
#' number of edges.
#'
#' @seealso [fastmap::faststack()] for the stack implementation used internally.
#'
#' @noRd
find_bridges <- function(adj_matrix, lens, edge_id_matrix, n_edges, quiet = FALSE) {
  n <- ncol(adj_matrix)

  prog <- pbapply::dopb() & !quiet & n > 10000

  if (prog) {
    pb <- txtProgressBar(0, n, style = 3)
    on.exit(close(pb))
  }

  visited <- logical(n)
  disc <- integer(n)
  low <- integer(n)
  parent_edge <- integer(n)   # edge ID used to reach each node (0 = root/unset)
  is_bridge <- logical(n_edges)

  time <- 0L

  stack <- faststack()
  state_stack <- faststack()

  for (root in seq_len(n)) {
    if (visited[root]) next

    stack$push(root)
    state_stack$push(1L)
    parent_edge[root] <- 0L

    while (stack$size() > 0) {
      node <- stack$pop()
      state <- state_stack$pop()

      if (state == 1L) {
        # guard: node may have been pushed by multiple parents before being visited
        if (visited[node]) next

        # first visit
        visited[node] <- TRUE
        time <- time + 1L

        if (!time %% 100 && prog)
          setTxtProgressBar(pb, time)

        disc[node] <- time
        low[node] <- time

        # push back to process after children
        stack$push(node)
        state_stack$push(2L)

        # push unvisited neighbors
        if (lens[node] > 0) {
          for (i in seq_len(lens[node])) {
            neighbor <- adj_matrix[i, node]
            eid <- edge_id_matrix[i, node]
            if (neighbor > 0) {
              if (!visited[neighbor]) {
                parent_edge[neighbor] <- eid
                stack$push(neighbor)
                state_stack$push(1L)
              } else if (eid != parent_edge[node]) {
                # back edge: update low via discovery time of visited neighbor
                low[node] <- min(low[node], disc[neighbor])
              }
            }
          }
        }

      } else {
        # post-visit: update low from children, check bridge condition
        if (lens[node] > 0) {
          for (i in seq_len(lens[node])) {
            neighbor <- adj_matrix[i, node]
            eid <- edge_id_matrix[i, node]
            # tree edge to child: neighbor was reached via this edge,
            # and this edge is not the one we used to reach node itself
            if (neighbor > 0 && parent_edge[neighbor] == eid && eid != parent_edge[node]) {
              low[node] <- min(low[node], low[neighbor])

              # bridge condition: strict > (vs >= for articulation points)
              if (low[neighbor] > disc[node]) {
                is_bridge[eid] <- TRUE
              }
            }
          }
        }
      }
    }
  }

  if (prog)
    setTxtProgressBar(pb, n)

  which(is_bridge)
}
