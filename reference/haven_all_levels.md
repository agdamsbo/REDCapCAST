# Finish incomplete haven attributes substituting missings with values

Finish incomplete haven attributes substituting missings with values

## Usage

``` r
haven_all_levels(data)
```

## Arguments

- data:

  haven labelled variable

## Value

named vector

## Examples

``` r
ds <- structure(c(1, 2, 3, 2, 10, 9),
  labels = c(Unknown = 9, Refused = 10),
  class = "haven_labelled"
)
haven::is.labelled(ds)
#> [1] TRUE
attributes(ds)
#> $labels
#> Unknown Refused 
#>       9      10 
#> 
#> $class
#> [1] "haven_labelled"
#> 
ds |> haven_all_levels()
#>       1       2       3 Unknown Refused 
#>       1       2       3       9      10 
```
