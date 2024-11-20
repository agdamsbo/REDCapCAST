#' Creates zip-file with necessary content to manually add instrument to database
#'
#' @description
#' Metadata can be added by editing the data dictionary of a project in the
#' initial design phase. If you want to later add new instruments, this
#' function can be used to create (an) instrument(s) to add to a project in
#' production.
#'
#' @param data metadata for the relevant instrument.
#' Could be from `ds2dd_detailed()`
#' @param file destination file name.
#' @param force force instrument creation and ignore different form names by
#' just using the first.
#' @param record.id record id variable name. Default is 'record_id'.
#'
#' @return exports zip-file
#' @export
#'
#' @examples
#' #iris |>
#' #  ds2dd_detailed(
#' #    add.auto.id = TRUE,
#' #    form.name = sample(c("b", "c"), size = 6, replace = TRUE, prob = rep(.5, 2))
#' #  ) |>
#' #  purrr::pluck("meta") |>
#' #  (\(.x){
#' #  split(.x, .x$form_name)
#' #  })() |>
#' #  purrr::imap(function(.x, .i){
#' #  export_redcap_instrument(.x,file=here::here(paste0(.i,Sys.Date(),".zip")))
#' #  })
#'
#' #iris |>
#' #  ds2dd_detailed(
#' #    add.auto.id = TRUE
#' #  ) |>
#' #  purrr::pluck("meta") |>
#' #  export_redcap_instrument(file=here::here(paste0("instrument",Sys.Date(),".zip")))
export_redcap_instrument <- function(data,
                                     file,
                                     force=FALSE,
                                     record.id = "record_id") {
  # Ensure form name is the same
  if (force){
    data$form_name <- data$form_name[1]
  } else if (length(unique(data$form_name))!=1){
    stop("Please provide metadata for a single form only. See examples for
         ideas on exporting multiple instruments.")
  }

  if (!is.na(record.id) && record.id %in% data[["field_name"]]){
    data <- data[-match(record.id,data[["field_name"]]),]
  }

  temp_dir <- tempdir()
  utils::write.csv(data, paste0(temp_dir, "/instrument.csv"), row.names = FALSE, na = "")
  writeLines("REDCapCAST", paste0(temp_dir, "/origin.txt"))
  zip::zip(
    zipfile = file,
    files = c("origin.txt", "instrument.csv"),
    root = temp_dir
  )
}


#' DEPRICATED Create zips file with necessary content based on data set
#'
#' @description
#' Metadata can be added by editing the data dictionary of a project in the
#' initial design phase. If you want to later add new instruments, this
#' function can be used to create (an) instrument(s) to add to a project in
#' production.
#'
#' @param data metadata for the relevant instrument.
#' Could be from `ds2dd_detailed()`
#' @param dir destination dir for the instrument zip. Default is the current WD.
#' @param record.id flag to omit the first row of the data dictionary assuming
#' this is the record_id field which should not be included in the instrument.
#' Default is TRUE.
#'
#' @return list
#' @export
#'
#' @examples
#' data <- iris |>
#'   ds2dd_detailed(
#'     add.auto.id = TRUE,
#'     form.name = sample(c("b", "c"),
#'       size = 6,
#'       replace = TRUE, prob = rep(.5, 2)
#'     )
#'   ) |>
#'   purrr::pluck("meta")
#' # data |> create_instrument_meta()
#'
#' data <- iris |>
#'   ds2dd_detailed(add.auto.id = FALSE) |>
#'   purrr::pluck("data")
#' iris |>
#'   setNames(glue::glue("{sample(x = c('a','b'),size = length(ncol(iris)),
#' replace=TRUE,prob = rep(x=.5,2))}__{names(iris)}")) |>
#'   ds2dd_detailed(form.sep = "__")
#' # data |>
#' #   purrr::pluck("meta") |>
#' #   create_instrument_meta(record.id = FALSE)
create_instrument_meta <- function(data,
                                   dir = here::here(""),
                                   record.id = TRUE) {
  # browser()
  if (record.id) {
    data <- data[-1, ]
  }
  temp_dir <- tempdir()
  split(data, data$form_name) |> purrr::imap(function(.x, .i) {
    utils::write.csv(.x, paste0(temp_dir, "/instrument.csv"),
      row.names = FALSE, na = ""
    )
    writeLines("REDCapCAST", paste0(temp_dir, "/origin.txt"))
    zip::zip(paste0(dir, "/", .i, Sys.Date(), ".zip"),
      files = c("origin.txt", "instrument.csv"),
      root = temp_dir
    )
  })
}
