ui <-
  bslib::page(
    theme = bslib::bs_theme(preset = "united"),
    title = "REDCap database creator",
    bslib::page_navbar(
      title = "Easy REDCap database creation",
      sidebar = bslib::sidebar(
        width = 300,
        shiny::h5("1) Database meta data"),
        shiny::fileInput(
          inputId = "ds",
          label = "Upload spreadsheet",
          multiple = FALSE,
          accept = c(
            ".csv",
            ".xls",
            ".xlsx",
            ".dta",
            ".ods"
          )
        ),
        shiny::helpText("Have a look at the preview panels to show download options."),
        # For some odd reason this only unfolds when the preview panel is shown..
        shiny::conditionalPanel(
          condition = "output.uploaded=='yes'",
          shiny::helpText("Below you can download the dataset formatted for upload and the
         corresponding data dictionary for a new data base, if you want to upload manually."),
          # Button
          shiny::downloadButton("downloadData", "Download renamed data"),

          # Button
          shiny::downloadButton("downloadMeta", "Download data dictionary"),

          # Button
          shiny::downloadButton("downloadInstrument", "Download as instrument"),

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
