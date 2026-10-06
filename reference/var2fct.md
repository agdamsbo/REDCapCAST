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
#>  1  2  3  4 
#>  2  1  7 10 
sample(letters, 20) |>
  var2fct(6) |>
  summary()
#>    Length  N.unique   N.blank Min.nchar Max.nchar 
#>        20        20         0         1         1 
sample(letters[1:4], 20, TRUE) |> var2fct(6)
#>  [1] c c d b c d b c b c b b b b b a c d b b
#> Levels: c d b a
```
