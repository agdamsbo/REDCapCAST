# Tests if vector can be interpreted as numeric without introducing NAs by coercion

Tests if vector can be interpreted as numeric without introducing NAs by
coercion

## Usage

``` r
possibly_numeric(data)
```

## Arguments

- data:

  vector

## Value

logical

## Examples

``` r
c("1","5") |> possibly_numeric()
#> [1] TRUE
c("1","5","e") |> possibly_numeric()
#> [1] FALSE
```
