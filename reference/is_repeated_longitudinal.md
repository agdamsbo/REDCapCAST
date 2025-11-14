# Test if repeatable or longitudinal

Test if repeatable or longitudinal

## Usage

``` r
is_repeated_longitudinal(
  data,
  generics = c("redcap_event_name", "redcap_repeat_instrument", "redcap_repeat_instance")
)
```

## Arguments

- data:

  data set

- generics:

  default is "redcap_event_name", "redcap_repeat_instrument" and
  "redcap_repeat_instance"

## Value

logical

## Examples

``` r
is_repeated_longitudinal(c("record_id", "age", "record_id", "gender"))
#> [1] FALSE
is_repeated_longitudinal(redcapcast_data)
#> [1] TRUE
is_repeated_longitudinal(list(redcapcast_data))
#> [1] TRUE
```
