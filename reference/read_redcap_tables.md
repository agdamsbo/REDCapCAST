# Download REDCap data

Implementation of passed on to
[REDCap_split](https://agdamsbo.github.io/REDCapCAST/reference/REDCap_split.md)
with a focused data acquisition approach using passed on to
[redcap_read](https://ouhscbbmc.github.io/REDCapR/reference/redcap_read.html)
and only downloading specified fields, forms and/or events using the
built-in focused_metadata including some clean-up. Works with classical
and longitudinal projects with or without repeating instruments. Will
preserve metadata in the data.frames as labels.

## Usage

``` r
read_redcap_tables(
  uri,
  token,
  records = NULL,
  fields = NULL,
  events = NULL,
  forms = NULL,
  raw_or_label = c("raw", "label", "both"),
  split_forms = c("all", "repeating", "none"),
  ...
)
```

## Arguments

- uri:

  REDCap database API uri

- token:

  API token

- records:

  records to download

- fields:

  fields to download

- events:

  events to download

- forms:

  forms to download

- raw_or_label:

  raw or label tags. Can be "raw", "label" or "both".

  \* "raw": Standard
  [redcap_read](https://ouhscbbmc.github.io/REDCapR/reference/redcap_read.html)
  method to get raw values. \* "label": Standard
  [redcap_read](https://ouhscbbmc.github.io/REDCapR/reference/redcap_read.html)
  method to get label values. \* "both": Get raw values with REDCap
  labels applied as labels. Use
  [as_factor](https://agdamsbo.github.io/REDCapCAST/reference/as_factor.md)
  to format factors with original labels and use the \`gtsummary\`
  package functions like
  [tbl_summary](https://www.danieldsjoberg.com/gtsummary/reference/tbl_summary.html)
  to easily get beautiful tables with original labels from REDCap. Use
  [fct_drop](https://agdamsbo.github.io/REDCapCAST/reference/fct_drop.md)
  to drop empty levels.

- split_forms:

  Whether to split "repeating" or "all" forms, default is all. Give
  "none" to export native semi-long REDCap format

- ...:

  passed on to
  [redcap_read](https://ouhscbbmc.github.io/REDCapR/reference/redcap_read.html)

## Value

list of instruments

## Examples

``` r
# Examples will be provided later
```
