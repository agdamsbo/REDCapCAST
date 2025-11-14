# Doc table to data dictionary - EARLY, DOCS MISSING

Works well with \`project.aid::docx2list()\`. Allows defining a database
in a text document (see provided template) for an easier to use data
base creation. This approach allows easier collaboration when defining
the database. The generic case is a data frame with variable names as
values in a column. This is a format like the REDCap data dictionary,
but gives a few options for formatting.

## Usage

``` r
doc2dd(
  data,
  instrument.name,
  col.variables = 1,
  list.datetime.format = list(date_dmy = "_dat[eo]$", time_hh_mm_ss = "_ti[md]e?$"),
  col.description = NULL,
  col.condition = NULL,
  col.subheader = NULL,
  subheader.tag = "h2",
  condition.minor.sep = ",",
  condition.major.sep = ";",
  col.calculation = NULL,
  col.choices = NULL,
  choices.char.sep = "/",
  missing.default = NA
)
```

## Arguments

- data:

  tibble or data.frame with all variable names in one column

- instrument.name:

  character vector length one. Instrument name.

- col.variables:

  variable names column (default = 1), allows dplyr subsetting

- list.datetime.format:

  formatting for date/time detection. See \`case_match_regex_list()\`

- col.description:

  descriptions column, allows dplyr subsetting. If empty, variable names
  will be used.

- col.condition:

  conditions for branching column, allows dplyr subsetting. See
  \`char2cond()\`.

- col.subheader:

  sub-header column, allows dplyr subsetting. See
  \`format_subheader()\`.

- subheader.tag:

  formatting tag. Default is "h2"

- condition.minor.sep:

  condition split minor. See \`char2cond()\`. Default is ",".

- condition.major.sep:

  condition split major. See \`char2cond()\`. Default is ";".

- col.calculation:

  calculations column. Has to be written exact. Character vector.

- col.choices:

  choices column. See \`char2choice()\`.

- choices.char.sep:

  choices split. See \`char2choice()\`. Default is "/".

- missing.default:

  value for missing fields. Default is NA.

## Value

tibble or data.frame (same as data)

## Examples

``` r
# data <- dd_inst
# data |> doc2dd(instrument.name = "evt",
# col.description = 3,
# col.condition = 4,
# col.subheader = 2,
# col.calculation = 5,
# col.choices = 6)
```
