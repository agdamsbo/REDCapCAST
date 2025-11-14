# Flexible file import based on extension

Flexible file import based on extension

## Usage

``` r
read_input(file, consider.na = c("NA", "\"\"", ""))
```

## Arguments

- file:

  file name

- consider.na:

  character vector of strings to consider as NAs

## Value

tibble

## Examples

``` r
read_input("https://raw.githubusercontent.com/agdamsbo/cognitive.index.lookup/main/data/sample.csv")
#> Rows: 28 Columns: 8
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: ","
#> dbl (8): id, ab, age, imm, vis, ver, att, del
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> # A tibble: 28 × 8
#>       id    ab   age   imm   vis   ver   att   del
#>    <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#>  1     1     1    74    38    35    30    29    35
#>  2     1     2    69    36    36    21    27    37
#>  3     2     1    63    32    31    26    34    38
#>  4     2     2    64    37    34    26    36    46
#>  5     3     1    69    39    33    31    47    29
#>  6     3     2    70    40    36    31    46    25
#>  7     4     1    79    44    37    20    37    36
#>  8     4     2    81    35    36    31    37    40
#>  9     5     1    77    35    26    17    20    36
#> 10     5     2    80    29    34    18    23    36
#> # ℹ 18 more rows
```
