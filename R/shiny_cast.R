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
  ext <- tolower(tools::file_ext(file))

  tryCatch(
    {
      if (ext == "csv") {
        df <- readr::read_csv(file = file, na = consider.na)
      } else if (ext %in% c("xls", "xlsx")) {
        df <- openxlsx2::read_xlsx(file = file, na.strings = consider.na)
      } else if (ext == "dta") {
        df <- haven::read_dta(file = file)
      } else if (ext == "ods") {
        df <- readODS::read_ods(path = file)
      } else if (ext == "rds") {
        df <- readr::read_rds(file = file)
      }else {
        stop("Input file format has to be on of:
             '.csv', '.xls', '.xlsx', '.dta', '.ods' or '.rds'")
      }
    },
    error = function(e) {
      # return a safeError if a parsing error occurs
      stop(shiny::safeError(e))
    }
  )

  df
}

#' Overview of REDCapCAST data for shiny
#'
#' @param data list with class 'REDCapCAST'
#'
#' @return gt object
#' @export
cast_data_overview <- function(data){
  stopifnot("REDCapCAST" %in% class(data))
  data |>
    purrr::pluck("data") |>
    utils::head(20) |>
    # dplyr::tibble() |>
    gt::gt() |>
    gt::tab_style(
      style = gt::cell_text(weight = "bold"),
      locations = gt::cells_column_labels(dplyr::everything())
    ) |>
    gt::tab_header(
      title = "Imported data preview",
      subtitle = "The first 20 subjects of the supplied dataset for reference."
    )
}


#' Overview of REDCapCAST meta data for shiny
#'
#' @param data list with class 'REDCapCAST'
#'
#' @return gt object
#' @export
cast_meta_overview <- function(data){
  stopifnot("REDCapCAST" %in% class(data))
  data |>
    purrr::pluck("meta") |>
    # dplyr::tibble() |>
    dplyr::mutate(
      dplyr::across(
        dplyr::everything(),
        \(.x) {
          .x[is.na(.x)] <- ""
          return(.x)
        }
      )
    ) |>
    dplyr::select(1:8) |>
    gt::gt() |>
    gt::tab_style(
      style = gt::cell_text(weight = "bold"),
      locations = gt::cells_column_labels(dplyr::everything())
    ) |>
    gt::tab_header(
      title = "Generated metadata",
      subtitle = "Only the first 8 columns are modified using REDCapCAST. Download the metadata to see everything."
    ) |>
    gt::tab_style(
      style = gt::cell_borders(
        sides = c("left", "right"),
        color = "grey80",
        weight = gt::px(1)
      ),
      locations = gt::cells_body(
        columns = dplyr::everything()
      )
    )
}


#' Nav_bar defining function for shiny ui
#'
#' @return shiny object
#' @export
#'
nav_bar_page <- function(){
  bslib::page_navbar(
    title = "Easy REDCap database creation",
    sidebar = bslib::sidebar(
      width = 300,
      shiny::h5("Metadata casting"),
      shiny::fileInput(
        inputId = "ds",
        label = "Upload spreadsheet",
        multiple = FALSE,
        accept = c(
          ".csv",
          ".xls",
          ".xlsx",
          ".dta",
          ".rds",
          ".ods"
        )
      ),
      # shiny::actionButton(
      #   inputId = "load_data",
      #   label = "Load data",
      #   icon = shiny::icon("circle-down")
      # ),
      shiny::helpText("Have a look at the preview panels to validate the data dictionary and imported data."),
      # For some odd reason this only unfolds when the preview panel is shown..
      # This has been solved by adding an arbitrary button to load data - which was abandoned again
      shiny::conditionalPanel(
        condition = "output.uploaded=='yes'",
        shiny::radioButtons(
          inputId = "add_id",
          label = "Add ID, or use first column?",
          selected = "no",
          inline = TRUE,
          choices = list(
            "First column" = "no",
            "Add ID" = "yes",
            "No ID" = "none"
          )
        ),
        shiny::radioButtons(
          inputId = "specify_factors",
          label = "Specify categorical variables?",
          selected = "no",
          inline = TRUE,
          choices = list(
            "No" = "no",
            "Yes" = "yes"
          )
        ),
        shiny::conditionalPanel(
          condition = "input.specify_factors=='yes'",
          shiny::uiOutput("factor_vars")
        ),
        # condition = "input.load_data",
        #  shiny::helpText("Below you can download the dataset formatted for upload and the
        # corresponding data dictionary for a new data base, if you want to upload manually."),
        # Button
        shiny::downloadButton(outputId = "downloadData", label = "Download renamed data"),

        # Button
        shiny::downloadButton(outputId = "downloadMeta", label = "Download data dictionary"),

        # Button
        shiny::downloadButton(outputId = "downloadInstrument", label = "Download as instrument"),

        # Horizontal line ----
        shiny::tags$hr(),
        shiny::radioButtons(
          inputId = "upload_redcap",
          label = "Upload directly to REDCap server?",
          selected = "no",
          inline = TRUE,
          choices = list(
            "No" = "no",
            "Yes" = "yes"
          )
        ),
        shiny::conditionalPanel(
          condition = "input.upload_redcap=='yes'",
          shiny::h4("2) Data base upload"),
          shiny::helpText("This tool is usable for now. Detailed instructions are coming."),
          shiny::textInput(
            inputId = "uri",
            label = "URI",
            value = "https://redcap.your.institution/api/"
          ),
          shiny::textInput(
            inputId = "api",
            label = "API key",
            value = ""
          ),
          shiny::helpText("An API key is an access key to the REDCap database. Please", shiny::a("see here for directions", href = "https://www.iths.org/news/redcap-tip/redcap-api-101/"), " to obtain an API key for your project."),
          shiny::actionButton(
            inputId = "upload.meta",
            label = "Upload datadictionary", icon = shiny::icon("book-bookmark")
          ),
          shiny::helpText("Please note, that before uploading any real data, put your project
         into production mode."),
          shiny::actionButton(
            inputId = "upload.data",
            label = "Upload data", icon = shiny::icon("upload")
          )
        )
      ),
      shiny::br(),
      shiny::br(),
      shiny::br(),
      shiny::p(
        "License: ", shiny::a("GPL-3+", href = "https://agdamsbo.github.io/REDCapCAST/LICENSE.html")
      ),
      shiny::p(
        shiny::a("Package documentation", href = "https://agdamsbo.github.io/REDCapCAST")
      )
    ),
    bslib::nav_panel(
      title = "Intro",
      shiny::markdown(readLines("www/SHINYCAST.md")),
      shiny::br()
    ),
    # bslib::nav_spacer(),
    bslib::nav_panel(
      title = "Data preview",
      gt::gt_output(outputId = "data.tbl")
      # shiny::htmlOutput(outputId = "data.tbl", container = shiny::span)
    ),
    bslib::nav_panel(
      title = "Dictionary overview",
      gt::gt_output(outputId = "meta.tbl")
      # shiny::htmlOutput(outputId = "meta.tbl", container = shiny::span)
    ),
    bslib::nav_panel(
      title = "Upload",
      shiny::h3("Meta upload overview"),
      shiny::textOutput(outputId = "upload.meta.print"),
      shiny::h3("Data upload overview"),
      shiny::textOutput(outputId = "upload.data.print")
    )
  )
}
