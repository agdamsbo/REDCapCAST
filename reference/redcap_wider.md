# Transforms list of REDCap data.frames to a single wide data.frame

Converts a list of REDCap data.frames from long to wide format. In
essence it is a wrapper for the
[pivot_wider](https://tidyr.tidyverse.org/reference/pivot_wider.html)
function applied on a REDCap output (from
[read_redcap_tables](https://agdamsbo.github.io/REDCapCAST/reference/read_redcap_tables.md))
or manually split by
[REDCap_split](https://agdamsbo.github.io/REDCapCAST/reference/REDCap_split.md).

## Usage

``` r
redcap_wider(
  data,
  event.glue = "{.value}____{redcap_event_name}",
  inst.glue = "{.value}____{redcap_repeat_instance}"
)
```

## Arguments

- data:

  A list of data frames

- event.glue:

  A [glue](https://glue.tidyverse.org/reference/glue.html) string for
  repeated events naming

- inst.glue:

  A [glue](https://glue.tidyverse.org/reference/glue.html) string for
  repeated instruments naming

## Value

data.frame in wide format

## Examples

``` r
# Longitudinal
list1 <- list(
  data.frame(
    record_id = c(1, 2, 1, 2),
    redcap_event_name = c("baseline", "baseline", "followup", "followup"),
    age = c(25, 26, 27, 28)
  ),
  data.frame(
    record_id = c(1, 2),
    redcap_event_name = c("baseline", "baseline"),
    gender = c("male", "female")
  )
)
redcap_wider(list1)
#> Joining with `by = join_by(record_id)`
#> # A tibble: 2 × 4
#>   record_id age____baseline age____followup gender
#>       <dbl>           <dbl>           <dbl> <chr> 
#> 1         1              25              27 male  
#> 2         2              26              28 female
# Simpel with two instruments
list2 <- list(
  data.frame(
    record_id = c(1, 2),
    age = c(25, 26)
  ),
  data.frame(
    record_id = c(1, 2),
    gender = c("male", "female")
  )
)
redcap_wider(list2)
#> Joining with `by = join_by(record_id)`
#>   record_id age gender
#> 1         1  25   male
#> 2         2  26 female
# Simple with single instrument
list3 <- list(data.frame(
  record_id = c(1, 2),
  age = c(25, 26)
))
redcap_wider(list3)
#>   record_id age
#> 1         1  25
#> 2         2  26
# Longitudinal with repeatable instruments
list4 <- list(
  data.frame(
    record_id = c(1, 2, 1, 2),
    redcap_event_name = c("baseline", "baseline", "followup", "followup"),
    age = c(25, 26, 27, 28)
  ),
  data.frame(
    record_id = c(1, 1, 1, 1, 2, 2, 2, 2),
    redcap_event_name = c(
      "baseline", "baseline", "followup", "followup",
      "baseline", "baseline", "followup", "followup"
    ),
    redcap_repeat_instrument = "walk",
    redcap_repeat_instance = c(1, 2, 1, 2, 1, 2, 1, 2),
    dist = c(40, 32, 25, 33, 28, 24, 23, 36)
  ),
  data.frame(
    record_id = c(1, 2),
    redcap_event_name = c("baseline", "baseline"),
    gender = c("male", "female")
  )
)
redcap_wider(list4)
#> Joining with `by = join_by(record_id)`
#> Joining with `by = join_by(record_id)`
#> # A tibble: 2 × 8
#>   record_id age____baseline age____followup dist____1____baseline
#>       <dbl>           <dbl>           <dbl>                 <dbl>
#> 1         1              25              27                    40
#> 2         2              26              28                    28
#> # ℹ 4 more variables: dist____1____followup <dbl>, dist____2____baseline <dbl>,
#> #   dist____2____followup <dbl>, gender <chr>

list5 <- list(
  data.frame(
    record_id = c(1, 2, 1, 2),
    redcap_event_name = c("baseline", "baseline", "followup", "followup")
  ),
  data.frame(
    record_id = c(1, 1, 1, 1, 2, 2, 2, 2),
    redcap_event_name = c(
      "baseline", "baseline", "followup", "followup",
      "baseline", "baseline", "followup", "followup"
    ),
    redcap_repeat_instrument = "walk",
    redcap_repeat_instance = c(1, 2, 1, 2, 1, 2, 1, 2),
    dist = c(40, 32, 25, 33, 28, 24, 23, 36)
  ),
  data.frame(
    record_id = c(1, 2),
    redcap_event_name = c("baseline", "baseline"),
    gender = c("male", "female")
  )
)
redcap_wider(list5)
#> Joining with `by = join_by(record_id)`
#> # A tibble: 2 × 6
#>   record_id dist____1____baseline dist____1____followup dist____2____baseline
#>       <dbl>                 <dbl>                 <dbl>                 <dbl>
#> 1         1                    40                    25                    32
#> 2         2                    28                    23                    24
#> # ℹ 2 more variables: dist____2____followup <dbl>, gender <chr>
```
