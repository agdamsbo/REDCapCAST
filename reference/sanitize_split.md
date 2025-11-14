# Sanitize list of data frames

Removing empty rows

## Usage

``` r
sanitize_split(
  l,
  generic.names = c("redcap_event_name", "redcap_repeat_instrument",
    "redcap_repeat_instance"),
  drop.complete = TRUE,
  drop.empty = TRUE
)
```

## Arguments

- l:

  A list of data frames.

- generic.names:

  A vector of generic names to be excluded.

- drop.complete:

  logical to remove generic REDCap variables indicating instrument
  completion. Default is TRUE.

- drop.empty:

  logical to remove variables with only NAs Default is TRUE.

## Value

A list of data frames with generic names excluded.
