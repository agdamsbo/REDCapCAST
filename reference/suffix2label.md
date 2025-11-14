# Transfer variable name suffix to label in widened data

Transfer variable name suffix to label in widened data

## Usage

``` r
suffix2label(
  data,
  suffix.sep = "____",
  attr = "label",
  glue.str = "{label} ({paste(suffixes,collapse=', ')})"
)
```

## Arguments

- data:

  data.frame

- suffix.sep:

  string to split suffix(es). Passed to
  [strsplit](https://rdrr.io/r/base/strsplit.html)

- attr:

  label attribute. Default is "label"

- glue.str:

  glue string for new label. Available variables are "label" and
  "suffixes"

## Value

data.frame
