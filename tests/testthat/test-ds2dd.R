mtcars$id <- seq_len(nrow(mtcars))

metadata_names <- function(...) {
  c(
    "field_name", "form_name", "section_header", "field_type",
    "field_label", "select_choices_or_calculations", "field_note",
    "text_validation_type_or_show_slider_number", "text_validation_min",
    "text_validation_max", "identifier", "branching_logic", "required_field",
    "custom_alignment", "question_number", "matrix_group_name", "matrix_ranking",
    "field_annotation"
  )
}

test_that("ds2dd gives desired output", {
  expect_equal(ncol(ds2dd(mtcars, record.id = "id",metadata = metadata_names())), 18)
  expect_s3_class(ds2dd(mtcars, record.id = "id",metadata = metadata_names()), "data.frame")
  expect_s3_class(ds2dd(mtcars, record.id = 12,metadata = metadata_names()), "data.frame")
})


test_that("ds2dd gives output with list of length two", {
  expect_equal(length(ds2dd(
    mtcars,
    record.id = "id",
    include.column.names = TRUE,metadata = metadata_names()
  )), 2)
})


test_that("ds2dd gives correct errors", {
  expect_error(ds2dd(mtcars,metadata = metadata_names()))
  expect_error(ds2dd(mtcars, form.name = c("basis", "incl"),metadata = metadata_names()))
  expect_error(ds2dd(mtcars, field.type = c("text", "dropdown"),metadata = metadata_names()))
  expect_error(ds2dd(mtcars, field.label = c("Name", "Age"),metadata = metadata_names()))
})

test_that("ds2dd correctly renames", {
  expect_equal(ncol(ds2dd(mtcars, record.id = "id",metadata = metadata_names())), 18)
  expect_s3_class(ds2dd(mtcars, record.id = "id",metadata = metadata_names()), "data.frame")
})
