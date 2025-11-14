# Get named vector of factor levels and values

Get named vector of factor levels and values

## Usage

``` r
named_levels(
  data,
  label = "labels",
  na.label = NULL,
  na.value = 99,
  sort.numeric = TRUE
)
```

## Arguments

- data:

  factor

- label:

  character string of attribute with named vector of factor labels

- na.label:

  character string to refactor NA values. Default is NULL.

- na.value:

  new value for NA strings. Ignored if na.label is NULL. Default is 99.

- sort.numeric:

  sort factor levels if levels are numeric. Default is TRUE

## Value

named vector

## Examples

``` r
structure(c(1, 2, 3, 2, 10, 9),
  labels = c(Unknown = 9, Refused = 10),
  class = "haven_labelled"
) |>
  as_factor() |>
  named_levels()
#>       1       2       3 Refused Unknown 
#>       1       2       3      10       9 
structure(c(1, 2, 3, 2, 10, 9),
  labels = c(Unknown = 9, Refused = 10),
  class = "labelled"
) |>
  as_factor() |>
  named_levels()
#>       1       2       3 Refused Unknown 
#>       1       2       3      10       9 
```
