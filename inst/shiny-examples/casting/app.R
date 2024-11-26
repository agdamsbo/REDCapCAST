library(bslib)
library(shiny)
library(openxlsx2)
library(haven)
library(readODS)
library(readr)
library(dplyr)
library(gt)
library(devtools)

if (!requireNamespace("REDCapCAST")) {
  install.packages("REDCapCAST")
}
library(REDCapCAST)

## Load merged files for shinyapps.io hosting
if (file.exists(here::here("functions.R"))) {
  source(here::here("functions.R"))
}

ui <-
  bslib::page(
    theme = bslib::bs_theme(preset = "united"),
    title = "REDCap database creator",
    nav_bar_page()
  )

server <- function(input, output, session) {
  v <- shiny::reactiveValues(
    file = NULL
  )

  ds <- shiny::reactive({
    shiny::req(input$ds)

    out <- read_input(input$ds$datapath)

    out <- out |>
      ## Parses data with readr functions
      parse_data() |>
      ## Converts logical to factor, preserving attributes with own function
      dplyr::mutate(dplyr::across(dplyr::where(is.logical), as_factor))

    out
  })

  dat <- shiny::reactive({
    out <- ds()

    if (!is.null(input$factor_vars)) {
      out <- out |>
        dplyr::mutate(
          dplyr::across(
            dplyr::all_of(input$factor_vars),
            as_factor
          )
        )
    }

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
    ds2dd_detailed(
      data = dat(),
      add.auto.id = input$add_id == "yes"
    )
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

  ## Specify ID if necessary
  # output$id_var <- shiny::renderUI({
  #   shiny::req(input$ds)
  #   selectizeInput(
  #     inputId = "id_var",
  #     selected = colnames(dat())[1],
  #     label = "ID variable",
  #     choices = colnames(dat())[-match(colnames(dat()),input$factor_vars)],
  #     multiple = FALSE
  #   )
  # })

  output$data.tbl <- gt::render_gt(
    dd() |>
      cast_data_overview()
  )

  output$meta.tbl <- gt::render_gt(
    dd() |>
      cast_meta_overview()
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
      export_redcap_instrument(purrr::pluck(dd(), "meta"),
        file = file,
        record.id = ifelse(input$add_id == "none", NA, names(dat())[1])
      )
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

shiny::shinyApp(ui = ui, server = server)
