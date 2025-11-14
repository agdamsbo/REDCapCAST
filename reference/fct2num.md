# Allows conversion of factor to numeric values preserving original levels

Allows conversion of factor to numeric values preserving original levels

## Usage

``` r
fct2num(data)
```

## Arguments

- data:

  vector

## Value

numeric vector

## Examples

``` r
c(1, 4, 3, "A", 7, 8, 1) |>
  as_factor() |>
  fct2num()
#> [1] 1 2 3 4 5 6 1

structure(c(1, 2, 3, 2, 10, 9),
  labels = c(Unknown = 9, Refused = 10),
  class = "haven_labelled"
) |>
  as_factor() |>
  fct2num()
#> [1]  1  2  3  2 10  9

structure(c(1, 2, 3, 2, 10, 9),
  labels = c(Unknown = 9, Refused = 10),
  class = "labelled"
) |>
  as_factor() |>
  fct2num()
#> [1]  1  2  3  2 10  9

structure(c(1, 2, 3, 2, 10, 9),
  labels = c(Unknown = 9, Refused = 10)
) |>
  as_factor() |>
  fct2num()
#> [1]  1  2  3  2 NA NA
```
