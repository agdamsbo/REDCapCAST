# Guess time variables based on naming pattern

This is for repairing data with time variables with appended
"1970-01-01"

## Usage

``` r
guess_time_only(
  data,
  validate.time = FALSE,
  time.var.sel.pos = "[Tt]i[d(me)]",
  time.var.sel.neg = "[Dd]at[eo]"
)
```

## Arguments

- data:

  data.frame or tibble

- validate.time:

  Flag to validate guessed time columns

- time.var.sel.pos:

  Positive selection regex string passed to \`gues_time_only_filter()\`
  as sel.pos.

- time.var.sel.neg:

  Negative selection regex string passed to \`gues_time_only_filter()\`
  as sel.neg.

## Value

data.frame or tibble

## Examples

``` r
redcapcast_data |> guess_time_only(validate.time = TRUE)
#> $is.POSIX
#> # A tibble: 25 × 2
#>    inclusion_time event_datetime     
#>    <time>         <dttm>             
#>  1 12:38:49       NA                 
#>  2 10:38:57       NA                 
#>  3       NA       NA                 
#>  4       NA       2024-01-18 12:49:42
#>  5 12:01:07       NA                 
#>  6       NA       NA                 
#>  7       NA       NA                 
#>  8       NA       2024-01-18 12:49:58
#>  9       NA       2024-01-18 12:50:01
#> 10       NA       2024-01-18 12:50:05
#> # ℹ 15 more rows
#> 
#> $is.datetime
#> # A tibble: 25 × 1
#>    event_datetime     
#>    <dttm>             
#>  1 NA                 
#>  2 NA                 
#>  3 NA                 
#>  4 2024-01-18 12:49:42
#>  5 NA                 
#>  6 NA                 
#>  7 NA                 
#>  8 2024-01-18 12:49:58
#>  9 2024-01-18 12:50:01
#> 10 2024-01-18 12:50:05
#> # ℹ 15 more rows
#> 
#> $is.time_only
#> # A tibble: 25 × 1
#>    inclusion_time
#>    <time>        
#>  1 12:38:49      
#>  2 10:38:57      
#>  3       NA      
#>  4       NA      
#>  5 12:01:07      
#>  6       NA      
#>  7       NA      
#>  8       NA      
#>  9       NA      
#> 10       NA      
#> # ℹ 15 more rows
#> 
```
