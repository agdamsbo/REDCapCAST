# List-base regex case_when

Mimics case_when for list of regex patterns and values. Used for
date/time validation generation from name vector. Like case_when, the
matches are in order of priority. Primarily used in REDCapCAST to do
data type coding from systematic variable naming.

## Usage

``` r
case_match_regex_list(data, match.list, .default = NA)
```

## Arguments

- data:

  vector

- match.list:

  list of case matches

- .default:

  Default value for non-matches. Default is NA.

## Value

vector

## Examples

``` r
case_match_regex_list(
  c("test_date", "test_time", "test_tida", "test_tid"),
  list(date_dmy = "_dat[eo]$", time_hh_mm_ss = "_ti[md]e?$")
)
#> [1] "date_dmy"      "time_hh_mm_ss" NA              "time_hh_mm_ss"
```
