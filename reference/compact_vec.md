# Compacting a vector of any length with or without names

Compacting a vector of any length with or without names

## Usage

``` r
compact_vec(data, nm.sep = ": ", val.sep = "; ")
```

## Arguments

- data:

  vector, optionally named

- nm.sep:

  string separating name from value if any

- val.sep:

  string separating values

## Value

character string

## Examples

``` r
sample(seq_len(4), 20, TRUE) |>
  as_factor() |>
  named_levels() |>
  sort() |>
  compact_vec()
#> [1] "1: 1; 2: 2; 3: 3; 4: 4"
1:6 |> compact_vec()
#> [1] "1; 2; 3; 4; 5; 6"
"test" |> compact_vec()
#> [1] "test"
sample(letters[1:9], 20, TRUE) |> compact_vec()
#> [1] "g; e; d; a; c; g; a; f; g; h; f; a; d; b; f; e; c; d; f; b"
```
