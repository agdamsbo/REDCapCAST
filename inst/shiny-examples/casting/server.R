library(REDCapCAST)
library(bslib)
library(shiny)
library(openxlsx2)
library(haven)
library(readODS)
library(readr)
library(dplyr)
library(here)

server <- function(input, output, session) {

  v <- shiny::reactiveValues(
    file = NULL
  )

  dat <- shiny::reactive({
    shiny::req(input$ds)

    read_input(input$ds$datapath) |>
      parse_data()
  })

  # getData <- reactive({
  #   if(is.null(input$ds$datapath)) return(NULL)
  # })
  # output$uploaded <- reactive({
  #   return(!is.null(getData()))
  # })


  output$uploaded <- shiny::reactive({
    if (is.null(v$file)) {
      "no"
    } else {
      "yes"
    }
  })

  shiny::outputOptions(output, "uploaded", suspendWhenHidden = FALSE)

  dd <- shiny::reactive({
    shiny::req(input$ds)
    v$file <- "loaded"
    ds2dd_detailed(data = dat())
  })

  output$data.tbl <- gt::render_gt(
    dd() |>
      purrr::pluck("data") |>
      head(20) |>
      dplyr::tibble() |>
      gt::gt()
  )

  output$meta.tbl <- gt::render_gt(
    dd() |>
      purrr::pluck("meta") |>
      dplyr::tibble() |>
      gt::gt()
  )

  # Downloadable csv of dataset ----
  output$downloadData <- shiny::downloadHandler(
    filename = "data_ready.csv",
    content = function(file) {
      write.csv(purrr::pluck(dd(), "data"), file, row.names = FALSE,na = "")
    }
  )

  # Downloadable csv of data dictionary ----
  output$downloadMeta <- shiny::downloadHandler(
    filename = "datadictionary_ready.csv",
    content = function(file) {
      write.csv(purrr::pluck(dd(), "meta"), file, row.names = FALSE,na = "")
    }
  )

  # Downloadable .zip of instrument ----
  output$downloadInstrument <- shiny::downloadHandler(
    filename = paste0("REDCapCAST_instrument",Sys.Date(),".zip"),
    content = function(file) {
      export_redcap_instrument(purrr::pluck(dd(), "meta"), file)
    }
  )

  output_staging <- shiny::reactiveValues()

  output_staging$meta <- output_staging$data <- NA

  shiny::observeEvent(input$upload.meta,{  upload_meta()  })

  shiny::observeEvent(input$upload.data,{  upload_data()  })

  upload_meta <- function(){

    shiny::req(input$uri)

    shiny::req(input$api)

    output_staging$meta <- REDCapR::redcap_metadata_write(
      ds = purrr::pluck(dd(), "meta"),
      redcap_uri = input$uri,
      token = input$api
    )|> purrr::pluck("success")
  }

  upload_data <- function(){

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

}
