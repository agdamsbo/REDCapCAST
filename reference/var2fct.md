# Convert vector to factor based on threshold of number of unique levels

This is a wrapper of forcats::as_factor, which sorts numeric vectors
before factoring, but levels character vectors in order of appearance.

## Usage

``` r
var2fct(data, unique.n)
```

## Arguments

- data:

  vector or data.frame column

- unique.n:

  threshold to convert class to factor

## Value

vector

## Examples

``` r
sample(seq_len(4), 20, TRUE) |>
  var2fct(6) |>
  summary()
#> 1 2 3 4 
#> 7 5 6 2 
sample(letters, 20) |>
  var2fct(6) |>
  summary()
#>    Length     Class      Mode 
#>        20 character character 
sample(letters[1:4], 20, TRUE) |> var2fct(6)
#>  [1] c b b a d d c d d c b b c d b b b a a c
#> Levels: c b a d
```
