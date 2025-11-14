# Try at determining which are true time only variables

This is just a try at guessing data type based on data class and column
names hoping for a tiny bit of naming consistency. R does not include a
time-only data format natively, so the "hms" class from \`readr\` is
used. This has to be converted to character class before REDCap upload.

## Usage

``` r
guess_time_only_filter(
  data,
  validate = FALSE,
  sel.pos = "[Tt]i[d(me)]",
  sel.neg = "[Dd]at[eo]"
)
```

## Arguments

- data:

  data set

- validate:

  flag to output validation data. Will output list.

- sel.pos:

  Positive selection regex string

- sel.neg:

  Negative selection regex string

## Value

character vector or list depending on \`validate\` flag.

## Examples

``` r
data <- redcapcast_data
data |> guess_time_only_filter()
#> [1] "inclusion_time"
data |>
  guess_time_only_filter(validate = TRUE) |>
  lapply(head)
#> $is.POSIX
#> # A tibble: 6 × 2
#>   inclusion_time event_datetime     
#>   <time>         <dttm>             
#> 1 12:38:49       NA                 
#> 2 10:38:57       NA                 
#> 3       NA       NA                 
#> 4       NA       2024-01-18 12:49:42
#> 5 12:01:07       NA                 
#> 6       NA       NA                 
#> 
#> $is.datetime
#> # A tibble: 6 × 1
#>   event_datetime     
#>   <dttm>             
#> 1 NA                 
#> 2 NA                 
#> 3 NA                 
#> 4 2024-01-18 12:49:42
#> 5 NA                 
#> 6 NA                 
#> 
#> $is.time_only
#> # A tibble: 6 × 1
#>   inclusion_time
#>   <time>        
#> 1 12:38:49      
#> 2 10:38:57      
#> 3       NA      
#> 4       NA      
#> 5 12:01:07      
#> 6       NA      
#> 
```
