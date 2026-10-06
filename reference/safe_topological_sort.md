# Safe Wrapper for Topological Sort

Calls
[`topological_sort`](https://agdamsbo.github.io/REDCapCAST/reference/topological_sort.md)
and falls back to elements in order of first appearance if a cycle makes
a valid ordering impossible. Guaranteed to always return a character
vector of all unique elements.

## Usage

``` r
safe_topological_sort(
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

A character vector of all unique elements from `ordered_list`. If
constraints are consistent, the order satisfies all precedence
constraints as per
[`topological_sort`](https://agdamsbo.github.io/REDCapCAST/reference/topological_sort.md).
If a cycle is detected, elements are returned in order of first
appearance in `ordered_list`.

## See also

[`topological_sort`](https://agdamsbo.github.io/REDCapCAST/reference/topological_sort.md)
