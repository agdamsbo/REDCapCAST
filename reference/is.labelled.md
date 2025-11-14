# Tests for multiple label classes

Tests for multiple label classes

## Usage

``` r
is.labelled(x, classes = c("haven_labelled", "labelled"))
```

## Arguments

- x:

  data

- classes:

  classes to test

## Value

logical

## Examples

``` r
structure(c(1, 2, 3, 2, 10, 9),
  labels = c(Unknown = 9, Refused = 10),
  class = "haven_labelled"
) |> is.labelled()
#> [1] TRUE
```
