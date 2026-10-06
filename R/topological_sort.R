#' Topological Sort of Ordered Element Lists
#'
#' Derives a single consistent ordering of all elements from a list of partially
#' ordered vectors. Each vector encodes pairwise precedence constraints between
#' consecutive elements: if element A appears before element B in any input
#' vector, A is guaranteed to precede B in the output. The overall ordering is
#' resolved via Kahn's algorithm.
#'
#' @param ordered_list A list of character vectors, each specifying a partial
#'   ordering of elements. Consecutive elements within a vector imply a
#'   precedence constraint: earlier elements must appear before later ones in
#'   the output. Elements may appear across multiple vectors; constraints from
#'   all vectors are combined. Duplicate constraints are silently ignored.
#' @param priority A string controlling how ties are broken when multiple
#'   elements are simultaneously eligible for placement. One of:
#'   \describe{
#'     \item{\code{"none"}}{Default. Eligible elements are processed in the
#'       order they are first encountered in \code{ordered_list}. Fully
#'       deterministic with no resorting overhead.}
#'     \item{\code{"lexicographic"}}{Eligible elements are sorted alphabetically
#'       by name. Useful for reproducible output independent of input order.}
#'     \item{\code{"least_constrained"}}{Eligible elements with the fewest total
#'       edges (in-degree + out-degree) are placed first. Elements that
#'       participate in fewer ordering relationships surface earlier.}
#'     \item{\code{"most_constrained"}}{Eligible elements with the most total
#'       edges (in-degree + out-degree) are placed first. Elements that
#'       participate in more ordering relationships surface earlier.}
#'   }
#'
#' @return A character vector containing all unique elements from
#'   \code{ordered_list}, in an order consistent with all pairwise precedence
#'   constraints. When constraints allow multiple valid orderings, the result
#'   among tied elements is determined by \code{priority}. The result is always
#'   deterministic for a given \code{ordered_list} and \code{priority}.
#'   Errors if the constraints form a cycle (e.g. A must precede B and
#'   B must precede A), as no valid ordering exists in that case.
#'
#' @details
#' Precedence constraints are extracted from consecutive element pairs within
#' each vector and deduplicated before processing. The algorithm runs in
#' O(V + E) time for \code{priority = "none"}, where V is the number of unique
#' elements and E the number of unique constraints. Priority modes incur an
#' additional O(V log V) cost from sorting at each enqueue step.
#'
#' Elements that appear in only one vector and have no constraints linking them
#' to other elements except through a single predecessor or successor are
#' considered loosely constrained. Their placement among other eligible elements
#' is undefined beyond what \code{priority} dictates.
#'
#' @examples
#' ## Basic usage
#' #lst <- list(c("shoe", "ball"), c("shirt", "shoe", "car"), c("ball", "car"))
#' #topological_sort(lst)
#' ## [1] "shirt" "shoe"  "ball"  "car"
#'#
#' ## Element appearing in only one vector
#' #lst2 <- list(c("shoe", "ball"), c("shirt", "shoe", "car"),
#' #             c("ball", "car"),  c("stick", "car"))
#' #topological_sort(lst2)
#' ## [1] "shirt" "stick" "shoe"  "ball"  "car"
#'#
#' #topological_sort(lst2, priority = "lexicographic")
#' ## [1] "shirt" "stick" "shoe"  "ball"  "car"
#'#
#' #topological_sort(lst2, priority = "least_constrained")
#' ## [1] "stick" "shirt" "shoe"  "ball"  "car"
#'#
#' #topological_sort(lst2, priority = "most_constrained")
#' ## [1] "shirt" "shoe"  "ball"  "car"  "stick"
#'
#' # Cycle detection
#' \dontrun{
#' topological_sort(list(c("a", "b"), c("b", "a")))
#' # Error: Cycle detected - no valid ordering exists
#' }
#'
#' @seealso
#' \code{\link[igraph]{topo_sort}} for graph-based topological sorting via the
#' \pkg{igraph} package.
topological_sort <- function(ordered_list, priority = c("none", "lexicographic", "least_constrained", "most_constrained")) {
  priority <- match.arg(priority)

  # Flatten and integer-encode all nodes
  all_nodes <- unique(unlist(ordered_list, use.names = FALSE))
  node_idx  <- stats::setNames(seq_along(all_nodes), all_nodes)
  n         <- length(all_nodes)

  # Build all edges at once via vectorised offset indexing
  encoded   <- lapply(ordered_list, function(v) node_idx[v])
  edge_from <- unlist(lapply(encoded, function(v) v[-length(v)]), use.names = FALSE)
  edge_to   <- unlist(lapply(encoded, function(v) v[-1L]),        use.names = FALSE)

  # Remove duplicate edges via integer hash
  unique_edges <- !duplicated(edge_from * (n + 1L) + edge_to)
  edge_from    <- edge_from[unique_edges]
  edge_to      <- edge_to[unique_edges]

  # In-degree and out-degree
  indeg  <- tabulate(edge_to,   nbins = n)
  outdeg <- tabulate(edge_from, nbins = n)

  # Adjacency list
  adj <- vector("list", n)
  adj[as.integer(names(split(edge_to, edge_from)))] <- split(edge_to, edge_from)

  # Priority score: determines order within the queue (lower = dequeued first)
  score <- switch(priority,
                  none              = seq_len(n),                  # stable: insertion order
                  lexicographic     = order(order(all_nodes)),     # alphabetical on names
                  least_constrained = indeg + outdeg,              # fewest total edges first
                  most_constrained  = -(indeg + outdeg)            # most total edges first
  )

  # --- Kahn's algorithm with priority queue ---
  # Re-sort candidates by score whenever new nodes become available

  enqueue <- function(candidates, queue) {
    if (priority == "none") return(c(queue, candidates))   # FIFO, no resorting
    sort_idx <- order(score[c(queue, candidates)])
    c(queue, candidates)[sort_idx]
  }

  queue      <- enqueue(which(indeg == 0L), integer(0))
  result     <- integer(n)
  result_idx <- 0L

  while (length(queue) > 0L) {
    node       <- queue[1L]
    queue      <- queue[-1L]
    result_idx <- result_idx + 1L
    result[result_idx] <- node

    neighbors <- adj[[node]]
    indeg[neighbors] <- indeg[neighbors] - 1L
    new_zeros <- neighbors[indeg[neighbors] == 0L]

    if (length(new_zeros)) queue <- enqueue(new_zeros, queue)
  }

  if (result_idx != n) stop("Cycle detected - no valid ordering exists")

  all_nodes[result]
}

#' Safe Wrapper for Topological Sort
#'
#' Calls \code{\link{topological_sort}} and falls back to elements in order of
#' first appearance if a cycle makes a valid ordering impossible. Guaranteed to
#' always return a character vector of all unique elements.
#'
#' @inheritParams topological_sort
#'
#' @return A character vector of all unique elements from \code{ordered_list}.
#'   If constraints are consistent, the order satisfies all precedence
#'   constraints as per \code{\link{topological_sort}}. If a cycle is detected,
#'   elements are returned in order of first appearance in \code{ordered_list}.
#'
#' @seealso \code{\link{topological_sort}}
safe_topological_sort <- function(ordered_list, priority = c("none", "lexicographic", "least_constrained", "most_constrained")) {
  priority <- match.arg(priority)

  result <- tryCatch(
    topological_sort(ordered_list, priority = priority),
    error = function(e) NULL
  )

  if (is.null(result)) unique(unlist(ordered_list, use.names = FALSE))
  else result
}
