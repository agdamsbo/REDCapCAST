# Simple function to generate REDCap choices from character vector

Simple function to generate REDCap choices from character vector

## Usage

``` r
char2choice(data, char.split = "/", raw = NULL, .default = NA)
```

## Arguments

- data:

  vector

- char.split:

  splitting character(s)

- raw:

  specific values. Can be used for options of same length.

- .default:

  default value for missing. Default is NA.

## Value

vector

## Examples

``` r
char2choice(c("yes/no","  yep. / nope  ","",NA,"what"),.default=NA)
#> [1] "1, yes | 2, no"    "1, yep. | 2, nope" NA                 
#> [4] NA                  "1, what"          
```
