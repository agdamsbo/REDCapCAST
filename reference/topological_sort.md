# Topological Sort of Ordered Element Lists

Derives a single consistent ordering of all elements from a list of
partially ordered vectors. Each vector encodes pairwise precedence
constraints between consecutive elements: if element A appears before
element B in any input vector, A is guaranteed to precede B in the
output. The overall ordering is resolved via Kahn's algorithm.

## Usage

``` r
topological_sort(
  ordered_list,
  priority = c("none", "lexicographic", "least_constrained", "most_constrained")
)
```

## Arguments

- ordered_list:

  A list of character vectors, each specifying a partial ordering of
  elements. Consecutive elements within a vector imply a precedence
  constraint: earlier elements must appear before later ones in the
  output. Elements may appear across multiple vectors; constraints from
  all vectors are combined. Duplicate constraints are silently ignored.

- priority:

  A string controlling how ties are broken when multiple elements are
  simultaneously eligible for placement. One of:

  `"none"`

  :   Default. Eligible elements are processed in the order they are
      first encountered in `ordered_list`. Fully deterministic with no
      resorting overhead.

  `"lexicographic"`

  :   Eligible elements are sorted alphabetically by name. Useful for
      reproducible output independent of input order.

  `"least_constrained"`

  :   Eligible elements with the fewest total edges (in-degree +
      out-degree) are placed first. Elements that participate in fewer
      ordering relationships surface earlier.

  `"most_constrained"`

  :   Eligible elements with the most total edges (in-degree +
      out-degree) are placed first. Elements that participate in more
      ordering relationships surface earlier.

## Value

A character vector containing all unique elements from `ordered_list`,
in an order consistent with all pairwise precedence constraints. When
constraints allow multiple valid orderings, the result among tied
elements is determined by `priority`. The result is always deterministic
for a given `ordered_list` and `priority`. Errors if the constraints
form a cycle (e.g. A must precede B and B must precede A), as no valid
ordering exists in that case.

## Details

Precedence constraints are extracted from consecutive element pairs
within each vector and deduplicated before processing. The algorithm
runs in O(V + E) time for `priority = "none"`, where V is the number of
unique elements and E the number of unique constraints. Priority modes
incur an additional O(V log V) cost from sorting at each enqueue step.

Elements that appear in only one vector and have no constraints linking
them to other elements except through a single predecessor or successor
are considered loosely constrained. Their placement among other eligible
elements is undefined beyond what `priority` dictates.

## See also

`topo_sort` for graph-based topological sorting via the igraph package.

## Examples

``` r
## Basic usage
#lst <- list(c("shoe", "ball"), c("shirt", "shoe", "car"), c("ball", "car"))
#topological_sort(lst)
## [1] "shirt" "shoe"  "ball"  "car"
#
## Element appearing in only one vector
#lst2 <- list(c("shoe", "ball"), c("shirt", "shoe", "car"),
#             c("ball", "car"),  c("stick", "car"))
#topological_sort(lst2)
## [1] "shirt" "stick" "shoe"  "ball"  "car"
#
#topological_sort(lst2, priority = "lexicographic")
## [1] "shirt" "stick" "shoe"  "ball"  "car"
#
#topological_sort(lst2, priority = "least_constrained")
## [1] "stick" "shirt" "shoe"  "ball"  "car"
#
#topological_sort(lst2, priority = "most_constrained")
## [1] "shirt" "shoe"  "ball"  "car"  "stick"

# Cycle detection
if (FALSE) { # \dontrun{
topological_sort(list(c("a", "b"), c("b", "a")))
# Error: Cycle detected - no valid ordering exists
} # }
```
