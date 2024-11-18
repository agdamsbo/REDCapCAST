#' Launch the included Shiny-app for database casting and upload
#'
#' @description
#' Wraps shiny::runApp()
#'
#' @param ... Arguments passed to shiny::runApp()
#'
#' @return shiny app
#' @export
#'
#' @examples
#' # shiny_cast()
#'
shiny_cast <- function(...) {
  appDir <- system.file("shiny-examples", "casting", package = "REDCapCAST")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `REDCapCAST`.", call. = FALSE)
  }

  shiny::runApp(appDir = appDir, ...)
}



#' DEPRECATED Helper to import files correctly
#'
#' @param filenames file names
#'
#' @return character vector
#' @export
#'
#' @examples
#' file_extension(list.files(here::here(""))[[2]])[[1]]
#' file_extension(c("file.cd..ks", "file"))
file_extension <- function(filenames) {
  sub(
    pattern = "^(.*\\.|[^.]+)(?=[^.]*)", replacement = "",
    filenames,
    perl = TRUE
  )
}

#' Flexible file import based on extension
#'
#' @param file file name
#' @param consider.na character vector of strings to consider as NAs
#'
#' @return tibble
#' @export
#'
#' @examples
#' read_input("https://raw.githubusercontent.com/agdamsbo/cognitive.index.lookup/main/data/sample.csv")
read_input <- function(file, consider.na = c("NA", '""', "")) {
  ext <- tools::file_ext(file)

  tryCatch(
    {
      if (ext == "csv") {
        df <- readr::read_csv(file = file, na = consider.na)
      } else if (ext %in% c("xls", "xlsx")) {
        df <- openxlsx2::read_xlsx(file = file, na.strings = consider.na)
      } else if (ext == "dta") {
        df <- haven::read_dta(file = file)
      } else if (ext == "ods") {
        df <- readODS::read_ods(file = file)
      } else {
        stop("Input file format has to be on of:
             '.csv', '.xls', '.xlsx', '.dta' or '.ods'")
      }
    },
    error = function(e) {
      # return a safeError if a parsing error occurs
      stop(shiny::safeError(e))
    }
  )

  df
}
