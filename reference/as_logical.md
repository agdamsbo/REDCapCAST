# Interpret specific binary values as logicals

Interpret specific binary values as logicals

## Usage

``` r
as_logical(
  x,
  values = list(c("TRUE", "FALSE"), c("Yes", "No"), c(1, 0), c(1, 2)),
  ...
)

# S3 method for class 'data.frame'
as_logical(
  x,
  values = list(c("TRUE", "FALSE"), c("Yes", "No"), c(1, 0), c(1, 2)),
  ...
)

# Default S3 method
as_logical(
  x,
  values = list(c("TRUE", "FALSE"), c("Yes", "No"), c(1, 0), c(1, 2)),
  ...
)
```

## Arguments

- x:

  vector or data.frame

- values:

  list of values to interpret as logicals. First value is

- ...:

  ignored interpreted as TRUE.

## Value

vector

## Examples

``` r
c(sample(c("TRUE", "FALSE"), 20, TRUE), NA) |>
  as_logical() |>
  class()
#> [1] "logical"
ds <- dplyr::tibble(
  B = factor(sample(c(1, 2), 20, TRUE)),
  A = factor(sample(c("TRUE", "FALSE"), 20, TRUE)),
  C = sample(c(3, 4), 20, TRUE),
  D = factor(sample(c("In", "Out"), 20, TRUE))
)
ds |>
  as_logical() |>
  sapply(class)
#>         B         A         C         D 
#> "logical" "logical" "numeric"  "factor" 
ds$A |> class()
#> [1] "factor"
sample(c("TRUE",NA), 20, TRUE) |>
  as_logical()
#>  [1] TRUE TRUE TRUE   NA TRUE TRUE TRUE TRUE   NA TRUE TRUE TRUE   NA   NA TRUE
#> [16] TRUE TRUE TRUE   NA TRUE
as_logical(0)
#> [1] FALSE
```
