# Named vector to REDCap choices (\`wrapping compact_vec()\`)

Named vector to REDCap choices (\`wrapping compact_vec()\`)

## Usage

``` r
vec2choice(data)
```

## Arguments

- data:

  named vector

## Value

character string

## Examples

``` r
sample(seq_len(4), 20, TRUE) |>
  as_factor() |>
  named_levels() |>
  sort() |>
  vec2choice()
#> [1] "1, 1 | 2, 2 | 3, 3 | 4, 4"
```
