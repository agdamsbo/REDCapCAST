# Convert labelled vectors to factors while preserving attributes

This extends
[as_factor](https://forcats.tidyverse.org/reference/as_factor.html) as
well as
[as_factor](https://forcats.tidyverse.org/reference/as_factor.html), by
appending original attributes except for "class" after converting to
factor to avoid ta loss in case of rich formatted and labelled data.

## Usage

``` r
as_factor(x, ...)

# S3 method for class 'factor'
as_factor(x, ...)

# S3 method for class 'logical'
as_factor(x, ...)

# S3 method for class 'numeric'
as_factor(x, ...)

# S3 method for class 'character'
as_factor(x, ...)

# S3 method for class 'haven_labelled'
as_factor(
  x,
  levels = c("default", "labels", "values", "both"),
  ordered = FALSE,
  ...
)

# S3 method for class 'labelled'
as_factor(
  x,
  levels = c("default", "labels", "values", "both"),
  ordered = FALSE,
  ...
)

# S3 method for class 'data.frame'
as_factor(x, ..., only_labelled = TRUE)
```

## Arguments

- x:

  Object to coerce to a factor.

- ...:

  Other arguments passed down to method.

- levels:

  How to create the levels of the generated factor:

  \* "default": uses labels where available, otherwise the values.
  Labels are sorted by value. \* "both": like "default", but pastes
  together the level and value \* "label": use only the labels;
  unlabelled values become \`NA\` \* "values": use only the values

- ordered:

  If \`TRUE\` create an ordered (ordinal) factor, if \`FALSE\` (the
  default) create a regular (nominal) factor.

- only_labelled:

  Only apply to labelled columns?

## Details

Please refer to parent functions for extended documentation. To avoid
redundancy calls and errors, functions are copy-pasted here

Empty variables with empty levels attribute are interpreted as logicals

## Examples

``` r
# will preserve all attributes
c(1, 4, 3, "A", 7, 8, 1) |> as_factor()
#> [1] 1 4 3 A 7 8 1
#> Levels: 1 4 3 A 7 8
structure(c(1, 2, 3, 2, 10, 9),
  labels = c(Unknown = 9, Refused = 10)
) |>
  as_factor() |>
  dput()
#> structure(c(1L, 2L, 3L, 2L, 5L, 4L), levels = c("1", "2", "3", 
#> "9", "10"), class = "factor", labels = c(Unknown = 9, Refused = 10
#> ))

structure(c(1, 2, 3, 2, 10, 9),
  labels = c(Unknown = 9, Refused = 10),
  class = "haven_labelled"
) |>
  as_factor() |> class()
#> [1] "factor"
structure(rep(NA,10),
  class = c("labelled")
) |>
  as_factor() |> summary()
#> FALSE  TRUE  NA's 
#>     0     0    10 

rep(NA,10) |> as_factor()
#>  [1] <NA> <NA> <NA> <NA> <NA> <NA> <NA> <NA> <NA> <NA>
#> Levels: FALSE TRUE
```
