library(bslib)
library(shiny)
library(openxlsx2)
library(haven)
library(readODS)
library(readr)
library(dplyr)
library(gt)
library(devtools)

# if (!requireNamespace("REDCapCAST")) {
#   install.packages("REDCapCAST")
# }
# library(REDCapCAST)

## Load merged files for shinyapps.io hosting
if (file.exists(here::here("functions.R"))) {
  source(here::here("functions.R"))
}

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
      add.auto.id = input$add_id == "yes",
      metadata = c(
        "field_name", "form_name", "section_header", "field_type",
        "field_label", "select_choices_or_calculations", "field_note",
        "text_validation_type_or_show_slider_number", "text_validation_min",
        "text_validation_max", "identifier", "branching_logic", "required_field",
        "custom_alignment", "question_number", "matrix_group_name", "matrix_ranking",
        "field_annotation"
      )
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


ui <-
  bslib::page(
    theme = bslib::bs_theme(preset = "united"),
    title = "REDCap database creator",
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
  )


shiny::shinyApp(ui = ui, server = server)
