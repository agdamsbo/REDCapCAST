# Test if vector can be interpreted as roman numerals

Test if vector can be interpreted as roman numerals

## Usage

``` r
possibly_roman(data)
```

## Arguments

- data:

  character vector

## Value

logical

## Examples

``` r
sample(1:100, 10) |>
  as.roman() |>
  possibly_roman()
#> [1] TRUE
sample(c(TRUE, FALSE), 10, TRUE) |> possibly_roman()
#> [1] FALSE
rep(NA, 10) |> possibly_roman()
#> [1] FALSE
```
