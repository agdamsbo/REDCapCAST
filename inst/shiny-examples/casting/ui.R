library(REDCapCAST)
ui <-
  bslib::page(
    theme = bslib::bs_theme(preset = "united"),
    title = "REDCap database creator",
    REDCapCAST::nav_bar_page()
  )
