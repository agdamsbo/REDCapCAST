# get_data_location <- function(x) {
#   system.file(
#     "testdata",
#     x,
#     package = "REDCapCAST"
#   )
# }

# setwd("tests/testthat")

get_data_location <- function(x){
  # here::here(file.path("tests","testthat","data", x))
  file.path("data", x)
}
