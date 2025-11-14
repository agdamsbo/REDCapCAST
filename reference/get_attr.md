# Extract attribute. Returns NA if none

Extract attribute. Returns NA if none

## Usage

``` r
get_attr(data, attr = NULL)
```

## Arguments

- data:

  vector

- attr:

  attribute name

## Value

character vector

## Examples

``` r
attr(mtcars$mpg, "label") <- "testing"
do.call(c, sapply(mtcars, get_attr))
#> $mpg.label
#> [1] "testing"
#> 
if (FALSE) { # \dontrun{
mtcars |>
  numchar2fct(numeric.threshold = 6) |>
  ds2dd_detailed()
} # }
```
