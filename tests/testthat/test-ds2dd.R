mtcars$id <- seq_len(nrow(mtcars))

test_that("ds2dd gives desired output", {
  expect_equal(ncol(ds2dd(mtcars, record.id = "id")), 18)
  expect_s3_class(ds2dd(mtcars, record.id = "id"), "data.frame")
  expect_s3_class(ds2dd(mtcars, record.id = 12), "data.frame")
})


test_that("ds2dd gives output with list of length two", {
  expect_equal(length(ds2dd(
    mtcars,
    record.id = "id",
    include.column.names = TRUE
  )), 2)
})


test_that("ds2dd gives correct errors", {
  expect_error(ds2dd(mtcars))
  expect_error(ds2dd(mtcars, form.name = c("basis", "incl")))
  expect_error(ds2dd(mtcars, field.type = c("text", "dropdown")))
  expect_error(ds2dd(mtcars, field.label = c("Name", "Age")))
})

test_that("ds2dd correctly renames", {
  expect_equal(ncol(ds2dd(mtcars, record.id = "id")), 18)
  expect_s3_class(ds2dd(mtcars, record.id = "id"), "data.frame")
})
