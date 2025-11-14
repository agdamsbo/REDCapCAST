# Simple function to generate REDCap branching logic from character vector

Simple function to generate REDCap branching logic from character vector

## Usage

``` r
char2cond(
  data,
  minor.split = ",",
  major.split = ";",
  major.sep = " or ",
  .default = NA
)
```

## Arguments

- data:

  vector

- minor.split:

  minor split

- major.split:

  major split

- major.sep:

  argument separation. Default is " or ".

- .default:

  default value for missing. Default is NA.

## Value

vector

## Examples

``` r
#data <- dd_inst$betingelse
#c("Extubation_novent, 2; Pacu_delay, 1") |> char2cond()
```
