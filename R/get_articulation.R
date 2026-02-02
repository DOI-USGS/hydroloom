# WRITTEN WITH HELP FROM CLAUDE

#' Get Articulation Flowlines
#' @description Identifies articulation flowlines (cut vertices) in the network.
#' Articulation flowlines are those whose removal would disconnect the network.
#' @inheritParams add_levelpaths
#' @returns vector of flowline ids that are articulation points in the network
#' @name get_articulation_flowlines
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
#' 
#' get_articulation_flowlines(x)
#'
get_articulation_flowlines <- function(x) {
  UseMethod("get_articulation_flowlines")
}

#' @name get_articulation_flowlines
#' @export
get_articulation_flowlines.data.frame <- function(x) {
  get_articulation_flowlines(hy(x))
}

#' @name get_articulation_flowlines
#' @export
get_articulation_flowlines.hy <- function(x) {

  index_ids <- make_index_ids(x, mode = "none")

  articulation_indices <- find_articulation_points(index_ids$link, index_ids$lengths)

  if (length(articulation_indices) == 0) {
    return(vector(mode = class(x$id), length = 0))
  }

  # Map index ids back to original ids
  index_ids$link_list$id[articulation_indices]
}

#' Find Articulation Points in an Undirected Graph
#'
#' Identifies all articulation points (cut vertices) in an undirected graph
#' using an iterative implementation of Tarjan's algorithm. An articulation
#' point is a vertex whose removal disconnects the graph.
#'
#' @param adj_matrix Integer matrix of dimensions m x n, where n is the number
#'   of nodes and m is the maximum number of adjacent nodes for any node in the
#'   graph. Each column represents a node, and the rows contain the indices of
#'   its neighbors. Unused entries should be `NA_integer_`.
#' @param lens Integer vector of length n indicating the number of neighbors
#'   for each node. For node i, only `adj_matrix[1:lens[i], i]` contains valid
#'   neighbor indices.
#'
#' @return Integer vector of node indices that are articulation points. Returns
#'   an empty integer vector if no articulation points exist.
#'
#' @details
#' The function uses an iterative depth-first search with an explicit stack
#' to avoid recursion limits on large graphs. It handles disconnected graphs
#' by iterating over all unvisited nodes as potential roots.
#'
#' Time complexity is O(V + E) where V is the number of vertices and E is the
#' number of edges.
#'
#' @examples
#' # Linear graph: 1 -- 2 -- 3
#' # Node 2 is an articulation point
#' adj_matrix <- matrix(c(
#'   2L, 1L, 2L,
#'   NA_integer_, 3L, NA_integer_
#' ), nrow = 2, ncol = 3, byrow = TRUE)
#' lens <- c(1L, 2L, 1L)
#' find_articulation_points(adj_matrix, lens)
#' # Returns: 2
#'
#' # Triangle: no articulation points
#' adj_matrix <- matrix(c(
#'   2L, 1L, 1L,
#'   3L, 3L, 2L
#' ), nrow = 2, ncol = 3, byrow = TRUE)
#' lens <- c(2L, 2L, 2L)
#' find_articulation_points(adj_matrix, lens)
#' # Returns: integer(0)
#'
#' @seealso [fastmap::faststack()] for the stack implementation used internally.
#'
#' @noRd
find_articulation_points <- function(adj_matrix, lens) {
  n <- ncol(adj_matrix)

  visited <- logical(n)
  disc <- integer(n)
  low <- integer(n)
  parent <- integer(n)
  is_articulation <- logical(n)
  
  time <- 0L
  
  stack <- faststack()
  state_stack <- faststack()
  
  for (root in seq_len(n)) {
    if (visited[root]) next
    
    stack$push(root)
    state_stack$push(1L)
    parent[root] <- 0L
    
    while (stack$size() > 0) {
      node <- stack$pop()
      state <- state_stack$pop()
      
      if (state == 1L) {
        # first visit
        visited[node] <- TRUE
        time <- time + 1L
        disc[node] <- time
        low[node] <- time
        
        # push back to process after children
        stack$push(node)
        state_stack$push(2L)
        
        # push children
        if (lens[node] > 0) {
          for (i in seq_len(lens[node])) {
            neighbor <- adj_matrix[i, node]
            if(neighbor > 0) { # terminal
              if (!visited[neighbor]) {
                parent[neighbor] <- node
                stack$push(neighbor)
                state_stack$push(1L)
              } else if (neighbor != parent[node]) {
                # back edge
                low[node] <- min(low[node], disc[neighbor])
              }
            }
          }
        }
        
      } else {
        # post-visit: update low values from children
        if (lens[node] > 0) {
          for (i in seq_len(lens[node])) {
            neighbor <- adj_matrix[i, node]
            if (neighbor > 0 && parent[neighbor] == node) {
              low[node] <- min(low[node], low[neighbor])
              
              # articulation point conditions
              if (parent[node] != 0L && low[neighbor] >= disc[node]) {
                is_articulation[node] <- TRUE
              }
            }
          }
        }
      }
    }
    
    # check root separately: articulation if >1 DFS child
    root_children <- sum(parent == root)
    if (root_children > 1) {
      is_articulation[root] <- TRUE
    }
  }
  
  which(is_articulation)
}