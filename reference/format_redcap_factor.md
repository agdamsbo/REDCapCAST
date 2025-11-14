# Converts REDCap choices to factor levels and stores in labels attribute

Applying
[as_factor](https://agdamsbo.github.io/REDCapCAST/reference/as_factor.md)
to the data.frame or variable, will coerce to a factor.

## Usage

``` r
format_redcap_factor(data, meta)
```

## Arguments

- data:

  vector

- meta:

  vector of REDCap choices

## Value

vector of class "labelled" with a "labels" attribute

## Examples

``` r
format_redcap_factor(sample(1:3, 20, TRUE), "1, First. | 2, second | 3, THIRD")
#>  [1] 3 3 3 3 2 3 3 3 2 3 2 2 1 1 2 2 1 1 3 1
#> attr(,"labels")
#> First. second  THIRD 
#>    "1"    "2"    "3" 
#> attr(,"class")
#> [1] "labelled"
```
