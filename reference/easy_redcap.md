# Secure API key storage and data acquisition in one

Secure API key storage and data acquisition in one

## Usage

``` r
easy_redcap(
  project.name,
  uri,
  raw_or_label = "both",
  data_format = c("wide", "list", "redcap", "long"),
  widen.data = NULL,
  ...
)
```

## Arguments

- project.name:

  The name of the current project (for key storage with
  [key_set](https://keyring.r-lib.org/reference/key_get.html), using the
  default keyring)

- uri:

  REDCap database API uri

- raw_or_label:

  argument passed on to
  [read_redcap_tables](https://agdamsbo.github.io/REDCapCAST/reference/read_redcap_tables.md).
  Default is "both" to get labelled data.

- data_format:

  Choose the data

- widen.data:

  argument to widen the exported data. \[DEPRECATED\], use
  \`data_format\`instead

- ...:

  arguments passed on to
  [read_redcap_tables](https://agdamsbo.github.io/REDCapCAST/reference/read_redcap_tables.md).

## Value

data.frame or list depending on widen.data

## Examples

``` r
if (FALSE) { # \dontrun{
easy_redcap("My_new_project", fields = c("record_id", "age", "hypertension"))
} # }
```
