library(bslib)
library(shiny)
library(openxlsx2)
library(haven)
library(readODS)
library(readr)
library(dplyr)
library(here)
library(devtools)
if (!requireNamespace("REDCapCAST")) {
  devtools::install_github("agdamsbo/REDCapCAST", quiet = TRUE, upgrade = "never")
}
library(REDCapCAST)


server <- function(input, output, session) {
  v <- shiny::reactiveValues(
    file = NULL
  )

  dat <- shiny::reactive({
    shiny::req(input$ds)

    out <- read_input(input$ds$datapath)

    # Saves labels to reapply later
    labels <- lapply(out, get_attr)

    out <- out |>
      ## Parses data with readr functions
      parse_data() |>
      ## Converts logical to factor, which overwrites attributes
      ##
      dplyr::mutate(dplyr::across(dplyr::where(is.logical), forcats::as_factor))

    if (!is.null(input$factor_vars)) {
      out <- out |>
        dplyr::mutate(
          dplyr::across(
            dplyr::all_of(input$factor_vars),
            forcats::as_factor
          )
        )
    }

    # Old attributes are appended
    out <- purrr::imap(out,\(.x,.i){
      set_attr(.x,labels[[.i]])
    }) |>
      dplyr::bind_cols()

    out
  })

  # getData <- reactive({
  #   if(is.null(input$ds$datapath)) return(NULL)
  # })
  # output$uploaded <- reactive({
  #   return(!is.null(getData()))
  # })

  dd <- shiny::reactive({
    shiny::req(input$ds)
    v$file <- "loaded"
    ds2dd_detailed(data = dat())
  })

  output$uploaded <- shiny::reactive({
    if (is.null(v$file)) {
      "no"
    } else {
      "yes"
    }
  })

  shiny::outputOptions(output, "uploaded", suspendWhenHidden = FALSE)

  output$factor_vars <- shiny::renderUI({
    shiny::req(input$ds)
    selectizeInput(
      inputId = "factor_vars",
      selected = colnames(dat())[sapply(dat(), is.factor)],
      label = "Covariables to format as categorical",
      choices = colnames(dat()),
      multiple = TRUE
    )
  })

  output$data.tbl <- gt::render_gt(
    dd() |>
      purrr::pluck("data") |>
      head(20) |>
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
  )

  output$meta.tbl <- gt::render_gt(
    dd() |>
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
  )

  # Downloadable csv of dataset ----
  output$downloadData <- shiny::downloadHandler(
    filename = "data_ready.csv",
    content = function(file) {
      write.csv(purrr::pluck(dd(), "data"), file, row.names = FALSE, na = "")
    }
  )

  # Downloadable csv of data dictionary ----
  output$downloadMeta <- shiny::downloadHandler(
    filename = paste0("REDCapCAST_DataDictionary_", Sys.Date(), ".csv"),
    content = function(file) {
      write.csv(purrr::pluck(dd(), "meta"), file, row.names = FALSE, na = "")
    }
  )

  # Downloadable .zip of instrument ----
  output$downloadInstrument <- shiny::downloadHandler(
    filename = paste0("REDCapCAST_instrument", Sys.Date(), ".zip"),
    content = function(file) {
      export_redcap_instrument(purrr::pluck(dd(), "meta"), file)
    }
  )

  output_staging <- shiny::reactiveValues()

  output_staging$meta <- output_staging$data <- NA

  shiny::observeEvent(input$upload.meta, {
    upload_meta()
  })

  shiny::observeEvent(input$upload.data, {
    upload_data()
  })

  upload_meta <- function() {
    shiny::req(input$uri)

    shiny::req(input$api)

    output_staging$meta <- REDCapR::redcap_metadata_write(
      ds = purrr::pluck(dd(), "meta"),
      redcap_uri = input$uri,
      token = input$api
    ) |> purrr::pluck("success")
  }

  upload_data <- function() {
    shiny::req(input$uri)

    shiny::req(input$api)

    output_staging$data <- REDCapR::redcap_write(
      ds = purrr::pluck(dd(), "data"),
      redcap_uri = input$uri,
      token = input$api
    ) |> purrr::pluck("success")
  }

  output$upload.meta.print <- renderText(output_staging$meta)

  output$upload.data.print <- renderText(output_staging$data)

  # session$onSessionEnded(function() {
  #   # cat("Session Ended\n")
  #   unlink("www",recursive = TRUE)
  # })
}
