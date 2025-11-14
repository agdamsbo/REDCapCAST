# Creates zip-file with necessary content to manually add instrument to database

Metadata can be added by editing the data dictionary of a project in the
initial design phase. If you want to later add new instruments, this
function can be used to create (an) instrument(s) to add to a project in
production.

## Usage

``` r
export_redcap_instrument(data, file, force = FALSE, record.id = "record_id")
```

## Arguments

- data:

  metadata for the relevant instrument. Could be from
  \`ds2dd_detailed()\`

- file:

  destination file name.

- force:

  force instrument creation and ignore different form names by just
  using the first.

- record.id:

  record id variable name. Default is 'record_id'.

## Value

exports zip-file

## Examples

``` r
# iris |>
#  ds2dd_detailed(
#    add.auto.id = TRUE,
#    form.name = sample(c("b", "c"), size = 6, replace = TRUE, prob = rep(.5, 2))
#  ) |>
#  purrr::pluck("meta") |>
#  (\(.x){
#  split(.x, .x$form_name)
#  })() |>
#  purrr::imap(function(.x, .i){
#  export_redcap_instrument(.x,file=here::here(paste0(.i,Sys.Date(),".zip")))
#  })

# iris |>
#  ds2dd_detailed(
#    add.auto.id = TRUE
#  ) |>
#  purrr::pluck("meta") |>
#  export_redcap_instrument(file=here::here(paste0("instrument",Sys.Date(),".zip")))
```
