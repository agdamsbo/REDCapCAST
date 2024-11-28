# library(testthat)
test_that("fct2num works", {
  expect_equal(2 * 2, 4)

  expect_equal(
    c(1, 4, 3, "A", 7, 8, 1) |>
      as_factor() |> # named_levels()
      fct2num(),
    c(1, 2, 3, 4, 5, 6, 1)
  )

  expect_equal(
    structure(c(1, 2, 3, 2, 10, 9),
      labels = c(Unknown = 9, Refused = 10),
      class = "haven_labelled"
    ) |>
      as_factor() |>
      fct2num(),
    c(1, 2, 3, 2, 10, 9)
  )

  expect_equal(
    structure(c(1, 2, 3, 2, 10, 9),
      labels = c(Unknown = 9, Refused = 10),
      class = "labelled"
    ) |>
      as_factor() |>
      fct2num(),
    c(1, 2, 3, 2, 10, 9)
  )


  expect_equal(
    structure(c(1, 2, 3, 2, 10, 9),
      labels = c(Unknown = 9, Refused = 10)
    ) |>
      as_factor.labelled() |>
      fct2num(),
    c(1, 2, 3, 2, 10, 9)
  )

  expect_equal(
    structure(c(1, 2, 3, 2, 10, 9),
      labels = c(Unknown = 9, Refused = 10),
      class = "labelled"
    ) |>
      as_factor() |> dput(),
    structure(c(1L, 2L, 3L, 2L, 5L, 4L), levels = c(
      "1", "2", "3",
      "Unknown", "Refused"
    ), class = "factor", labels = c(
      Unknown = 9,
      Refused = 10
    ))
  )
})
