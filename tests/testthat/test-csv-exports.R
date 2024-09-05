# Set up the path and data -------------------------------------------------
metadata <- read.csv(
  get_data_location("ExampleProject_DataDictionary_2018-06-07.csv"),
  stringsAsFactors = TRUE
)

records <-
  read.csv(get_data_location("ExampleProject_DATA_2018-06-07_1129.csv"),
    stringsAsFactors = TRUE
  )

redcap_output_csv1 <- REDCap_split(records, metadata)

# Test that basic CSV export matches reference ------------------------------
test_that("CSV export matches reference", {
  # expect_known_hash(redcap_output_csv1, "cb5074a06e1abcf659d60be1016965d2")
  # dput(redcap_output_csv1)
  expect_identical(
    redcap_output_csv1,
    list(
      structure(list(
        row = structure(1:32, levels = c(
          "AMC Javelin",
          "Cadillac Fleetwood", "Camaro Z28", "Chrysler Imperial", "Datsun 710",
          "Dodge Challenger", "Duster 360", "Ferrari Dino", "Fiat 128",
          "Fiat X1-9", "Ford Pantera L", "Honda Civic", "Hornet 4 Drive",
          "Hornet Sportabout", "Lincoln Continental", "Lotus Europa", "Maserati Bora",
          "Mazda RX4", "Mazda RX4 Wag", "Merc 230", "Merc 240D", "Merc 280",
          "Merc 280C", "Merc 450SE", "Merc 450SL", "Merc 450SLC", "Pontiac Firebird",
          "Porsche 914-2", "Toyota Corolla", "Toyota Corona", "Valiant",
          "Volvo 142E"
        ), class = "factor"), mpg = c(
          15.2, 10.4, 13.3, 14.7,
          22.8, 15.5, 14.3, 19.7, 32.4, 27.3, 15.8, 30.4, 21.4, 18.7, 10.4,
          30.4, 15, 21, 21, 22.8, 24.4, 19.2, 17.8, 16.4, 17.3, 15.2, 19.2,
          26, 33.9, 21.5, 18.1, 21.4
        ), cyl = c(
          8L, 8L, 8L, 8L, 4L, 8L,
          8L, 6L, 4L, 4L, 8L, 4L, 6L, 8L, 8L, 4L, 8L, 6L, 6L, 4L, 4L, 6L,
          6L, 8L, 8L, 8L, 8L, 4L, 4L, 4L, 6L, 4L
        ), disp = c(
          304, 472, 350,
          440, 108, 318, 360, 145, 78.7, 79, 351, 75.7, 258, 360, 460,
          95.1, 301, 160, 160, 140.8, 146.7, 167.6, 167.6, 275.8, 275.8,
          275.8, 400, 120.3, 71.1, 120.1, 225, 121
        ), hp = c(
          150L, 205L,
          245L, 230L, 93L, 150L, 245L, 175L, 66L, 66L, 264L, 52L, 110L,
          175L, 215L, 113L, 335L, 110L, 110L, 95L, 62L, 123L, 123L, 180L,
          180L, 180L, 175L, 91L, 65L, 97L, 105L, 109L
        ), drat = c(
          3.15,
          2.93, 3.73, 3.23, 3.85, 2.76, 3.21, 3.62, 4.08, 4.08, 4.22, 4.93,
          3.08, 3.15, 3, 3.77, 3.54, 3.9, 3.9, 3.92, 3.69, 3.92, 3.92,
          3.07, 3.07, 3.07, 3.08, 4.43, 4.22, 3.7, 2.76, 4.11
        ), wt = c(
          3.435,
          5.25, 3.84, 5.345, 2.32, 3.52, 3.57, 2.77, 2.2, 1.935, 3.17,
          1.615, 3.215, 3.44, 5.424, 1.513, 3.57, 2.62, 2.875, 3.15, 3.19,
          3.44, 3.44, 4.07, 3.73, 3.78, 3.845, 2.14, 1.835, 2.465, 3.46,
          2.78
        ), qsec = c(
          17.3, 17.98, 15.41, 17.42, 18.61, 16.87, 15.84,
          15.5, 19.47, 18.9, 14.5, 18.52, 19.44, 17.02, 17.82, 16.9, 14.6,
          16.46, 17.02, 22.9, 20, 18.3, 18.9, 17.4, 17.6, 18, 17.05, 16.7,
          19.9, 20.01, 20.22, 18.6
        ), vs = c(
          0L, 0L, 0L, 0L, 1L, 0L, 0L,
          0L, 1L, 1L, 0L, 1L, 1L, 0L, 0L, 1L, 0L, 0L, 0L, 1L, 1L, 1L, 1L,
          0L, 0L, 0L, 0L, 0L, 1L, 1L, 1L, 1L
        ), am = c(
          0L, 0L, 0L, 0L, 1L,
          0L, 0L, 1L, 1L, 1L, 1L, 1L, 0L, 0L, 0L, 1L, 1L, 1L, 1L, 0L, 0L,
          0L, 0L, 0L, 0L, 0L, 0L, 1L, 1L, 0L, 0L, 1L
        ), gear = c(
          3L, 3L,
          3L, 3L, 4L, 3L, 3L, 5L, 4L, 4L, 5L, 4L, 3L, 3L, 3L, 5L, 5L, 4L,
          4L, 4L, 4L, 4L, 4L, 3L, 3L, 3L, 3L, 5L, 4L, 3L, 3L, 4L
        ), carb = c(
          2L,
          4L, 4L, 4L, 1L, 2L, 4L, 6L, 1L, 1L, 4L, 2L, 1L, 2L, 4L, 2L, 8L,
          4L, 4L, 2L, 2L, 4L, 4L, 3L, 3L, 3L, 2L, 2L, 1L, 1L, 1L, 2L
        ),
        color_available___red = c(
          1L, 0L, 0L, 0L, 0L, 0L, 0L, 0L,
          0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L,
          0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L
        ), color_available___green = c(
          1L,
          0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L,
          0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L,
          0L
        ), color_available___blue = c(
          1L, 0L, 0L, 0L, 0L, 0L, 0L,
          0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L,
          0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L
        ), color_available___black = c(
          0L,
          0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L,
          0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L,
          0L
        ), motor_trend_cars_complete = c(
          1L, 0L, 0L, 0L, 0L, 0L,
          0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L,
          0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L
        ), letter_group___a = c(
          1L,
          0L, 1L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L,
          0L, 0L, 1L, 1L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L,
          0L
        ), letter_group___b = c(
          1L, 0L, 0L, 1L, 1L, 0L, 1L, 0L,
          0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 1L, 1L, 0L, 0L, 0L,
          0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L
        ), letter_group___c = c(
          0L,
          0L, 1L, 1L, 1L, 0L, 1L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L,
          0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L,
          0L
        ), choice = structure(c(
          3L, 1L, 2L, 2L, 1L, 1L, 2L, 1L,
          1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 3L, 1L, 1L, 1L,
          1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L
        ), levels = c(
          "", "choice1",
          "choice2"
        ), class = "factor"), grouping_complete = c(
          2L,
          0L, 2L, 2L, 0L, 0L, 1L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L,
          0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L,
          0L
        )
      ), row.names = c(
        1L, 5L, 6L, 9L, 11L, 12L, 13L, 18L, 19L,
        20L, 21L, 22L, 23L, 24L, 25L, 26L, 27L, 28L, 29L, 30L, 34L, 35L,
        36L, 37L, 38L, 39L, 40L, 41L, 42L, 43L, 44L, 45L
      ), class = "data.frame"),
      sale = structure(list(
        row = structure(c(
          1L, 1L, 1L, 3L, 3L,
          4L, 7L, 7L, 7L, 7L, 20L, 20L, 20L
        ), levels = c(
          "AMC Javelin",
          "Cadillac Fleetwood", "Camaro Z28", "Chrysler Imperial",
          "Datsun 710", "Dodge Challenger", "Duster 360", "Ferrari Dino",
          "Fiat 128", "Fiat X1-9", "Ford Pantera L", "Honda Civic",
          "Hornet 4 Drive", "Hornet Sportabout", "Lincoln Continental",
          "Lotus Europa", "Maserati Bora", "Mazda RX4", "Mazda RX4 Wag",
          "Merc 230", "Merc 240D", "Merc 280", "Merc 280C", "Merc 450SE",
          "Merc 450SL", "Merc 450SLC", "Pontiac Firebird", "Porsche 914-2",
          "Toyota Corolla", "Toyota Corona", "Valiant", "Volvo 142E"
        ), class = "factor"), redcap_repeat_instrument = c(
          "sale",
          "sale", "sale", "sale", "sale", "sale", "sale", "sale", "sale",
          "sale", "sale", "sale", "sale"
        ), redcap_repeat_instance = c(
          1L,
          2L, 3L, 1L, 2L, 1L, 1L, 2L, 3L, 4L, 1L, 2L, 3L
        ), price = c(
          12000.5,
          13750.77, 15004.57, 7800, 8000, 7500, 8756.4, 6800.88, 8888.88,
          970, 7800.98, 7954, 6800.55
        ), color = c(
          1L, 3L, 2L, 2L, 3L,
          1L, 4L, 2L, 1L, 4L, 2L, 1L, 3L
        ), customer = structure(c(
          2L,
          12L, 7L, 4L, 14L, 5L, 10L, 8L, 3L, 6L, 13L, 9L, 11L
        ), levels = c(
          "",
          "Bob", "Erica", "Janice", "Jim", "Juan", "Kim", "Pablo",
          "Quentin", "Sarah", "Sharon", "Sue", "Ted", "Tim"
        ), class = "factor"),
        sale_complete = c(
          0L, 2L, 0L, 2L, 0L, 2L, 1L, 0L, 0L,
          0L, 0L, 0L, 2L
        )
      ), row.names = c(
        2L, 3L, 4L, 7L, 8L, 10L,
        14L, 15L, 16L, 17L, 31L, 32L, 33L
      ), class = "data.frame")
    )
  )
})

# Test that REDCap_split can handle a focused dataset

records_red <- records[
  !records$redcap_repeat_instrument == "sale",
  !names(records) %in%
    metadata$field_name[metadata$form_name == "sale"] &
    !names(records) == "sale_complete"
]
records_red$redcap_repeat_instrument <-
  as.character(records_red$redcap_repeat_instrument)

redcap_output_red <- REDCap_split(records_red, metadata)


test_that("REDCap_split handles subset dataset", {
  testthat::expect_length(redcap_output_red, 1)
})


# Test that R code enhanced CSV export matches reference --------------------
if (requireNamespace("Hmisc", quietly = TRUE)) {
  test_that("R code enhanced export matches reference", {
    redcap_output_csv2 <-
      REDCap_split(REDCap_process_csv(records), metadata)

    # expect_known_hash(redcap_output_csv2, "578dc054e59ec92a21e950042e08ee37")
    # dput(redcap_output_csv2)
    expect_identical(
      redcap_output_csv2,
      list(structure(list(
        row = structure(1:32, levels = c(
          "AMC Javelin",
          "Cadillac Fleetwood", "Camaro Z28", "Chrysler Imperial", "Datsun 710",
          "Dodge Challenger", "Duster 360", "Ferrari Dino", "Fiat 128",
          "Fiat X1-9", "Ford Pantera L", "Honda Civic", "Hornet 4 Drive",
          "Hornet Sportabout", "Lincoln Continental", "Lotus Europa", "Maserati Bora",
          "Mazda RX4", "Mazda RX4 Wag", "Merc 230", "Merc 240D", "Merc 280",
          "Merc 280C", "Merc 450SE", "Merc 450SL", "Merc 450SLC", "Pontiac Firebird",
          "Porsche 914-2", "Toyota Corolla", "Toyota Corona", "Valiant",
          "Volvo 142E"
        ), class = c("labelled", "factor"), label = "Name"),
        mpg = structure(c(
          15.2, 10.4, 13.3, 14.7, 22.8, 15.5, 14.3,
          19.7, 32.4, 27.3, 15.8, 30.4, 21.4, 18.7, 10.4, 30.4, 15,
          21, 21, 22.8, 24.4, 19.2, 17.8, 16.4, 17.3, 15.2, 19.2, 26,
          33.9, 21.5, 18.1, 21.4
        ), label = "Miles/(US) gallon", class = c(
          "labelled",
          "numeric"
        )), cyl = structure(c(
          8L, 8L, 8L, 8L, 4L, 8L, 8L,
          6L, 4L, 4L, 8L, 4L, 6L, 8L, 8L, 4L, 8L, 6L, 6L, 4L, 4L, 6L,
          6L, 8L, 8L, 8L, 8L, 4L, 4L, 4L, 6L, 4L
        ), label = "Number of cylinders", class = c(
          "labelled",
          "integer"
        )), disp = structure(c(
          304, 472, 350, 440, 108,
          318, 360, 145, 78.7, 79, 351, 75.7, 258, 360, 460, 95.1,
          301, 160, 160, 140.8, 146.7, 167.6, 167.6, 275.8, 275.8,
          275.8, 400, 120.3, 71.1, 120.1, 225, 121
        ), label = "Displacement", class = c(
          "labelled",
          "numeric"
        )), hp = structure(c(
          150L, 205L, 245L, 230L, 93L,
          150L, 245L, 175L, 66L, 66L, 264L, 52L, 110L, 175L, 215L,
          113L, 335L, 110L, 110L, 95L, 62L, 123L, 123L, 180L, 180L,
          180L, 175L, 91L, 65L, 97L, 105L, 109L
        ), label = "Gross horsepower", class = c(
          "labelled",
          "integer"
        )), drat = structure(c(
          3.15, 2.93, 3.73, 3.23, 3.85,
          2.76, 3.21, 3.62, 4.08, 4.08, 4.22, 4.93, 3.08, 3.15, 3,
          3.77, 3.54, 3.9, 3.9, 3.92, 3.69, 3.92, 3.92, 3.07, 3.07,
          3.07, 3.08, 4.43, 4.22, 3.7, 2.76, 4.11
        ), label = "Rear axle ratio", class = c(
          "labelled",
          "numeric"
        )), wt = structure(c(
          3.435, 5.25, 3.84, 5.345, 2.32,
          3.52, 3.57, 2.77, 2.2, 1.935, 3.17, 1.615, 3.215, 3.44, 5.424,
          1.513, 3.57, 2.62, 2.875, 3.15, 3.19, 3.44, 3.44, 4.07, 3.73,
          3.78, 3.845, 2.14, 1.835, 2.465, 3.46, 2.78
        ), label = "Weight", class = c(
          "labelled",
          "numeric"
        )), qsec = structure(c(
          17.3, 17.98, 15.41, 17.42,
          18.61, 16.87, 15.84, 15.5, 19.47, 18.9, 14.5, 18.52, 19.44,
          17.02, 17.82, 16.9, 14.6, 16.46, 17.02, 22.9, 20, 18.3, 18.9,
          17.4, 17.6, 18, 17.05, 16.7, 19.9, 20.01, 20.22, 18.6
        ), label = "1/4 mile time", class = c(
          "labelled",
          "numeric"
        )), vs = structure(c(
          0L, 0L, 0L, 0L, 1L, 0L, 0L,
          0L, 1L, 1L, 0L, 1L, 1L, 0L, 0L, 1L, 0L, 0L, 0L, 1L, 1L, 1L,
          1L, 0L, 0L, 0L, 0L, 0L, 1L, 1L, 1L, 1L
        ), label = "V engine?", class = c(
          "labelled",
          "integer"
        )), am = structure(c(
          0L, 0L, 0L, 0L, 1L, 0L, 0L,
          1L, 1L, 1L, 1L, 1L, 0L, 0L, 0L, 1L, 1L, 1L, 1L, 0L, 0L, 0L,
          0L, 0L, 0L, 0L, 0L, 1L, 1L, 0L, 0L, 1L
        ), label = "Transmission", class = c(
          "labelled",
          "integer"
        )), gear = structure(c(
          3L, 3L, 3L, 3L, 4L, 3L, 3L,
          5L, 4L, 4L, 5L, 4L, 3L, 3L, 3L, 5L, 5L, 4L, 4L, 4L, 4L, 4L,
          4L, 3L, 3L, 3L, 3L, 5L, 4L, 3L, 3L, 4L
        ), label = "Number of forward gears", class = c(
          "labelled",
          "integer"
        )), carb = structure(c(
          2L, 4L, 4L, 4L, 1L, 2L, 4L,
          6L, 1L, 1L, 4L, 2L, 1L, 2L, 4L, 2L, 8L, 4L, 4L, 2L, 2L, 4L,
          4L, 3L, 3L, 3L, 2L, 2L, 1L, 1L, 1L, 2L
        ), label = "Number of carburetors", class = c(
          "labelled",
          "integer"
        )), color_available___red = structure(c(
          1L, 0L,
          0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L,
          0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L
        ), label = "Colors Available (choice<-Red)", class = c(
          "labelled",
          "integer"
        )), color_available___green = structure(c(
          1L, 0L,
          0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L,
          0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L
        ), label = "Colors Available (choice<-Green)", class = c(
          "labelled",
          "integer"
        )), color_available___blue = structure(c(
          1L, 0L,
          0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L,
          0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L
        ), label = "Colors Available (choice<-Blue)", class = c(
          "labelled",
          "integer"
        )), color_available___black = structure(c(
          0L, 0L,
          0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L,
          0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L
        ), label = "Colors Available (choice<-Black)", class = c(
          "labelled",
          "integer"
        )), motor_trend_cars_complete = structure(c(
          1L,
          0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L,
          0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L,
          0L
        ), label = "Complete?", class = c("labelled", "integer")), letter_group___a = structure(c(
          1L, 0L, 1L, 0L, 0L, 0L,
          0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 1L, 1L, 0L,
          0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L
        ), label = "Which group? (choice<-A)", class = c(
          "labelled",
          "integer"
        )), letter_group___b = structure(c(
          1L, 0L, 0L, 1L,
          1L, 0L, 1L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 1L,
          1L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L
        ), label = "Which group? (choice<-B)", class = c(
          "labelled",
          "integer"
        )), letter_group___c = structure(c(
          0L, 0L, 1L, 1L,
          1L, 0L, 1L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L,
          0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L
        ), label = "Which group? (choice<-C)", class = c(
          "labelled",
          "integer"
        )), choice = structure(c(
          3L, 1L, 2L, 2L, 1L, 1L,
          2L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 3L, 1L,
          1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L
        ), levels = c(
          "",
          "choice1", "choice2"
        ), class = c("labelled", "factor"), label = "Choose one"),
        grouping_complete = structure(c(
          2L, 0L, 2L, 2L, 0L, 0L, 1L,
          0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L,
          0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L
        ), label = "Complete?", class = c(
          "labelled",
          "integer"
        )), cyl.factor = structure(c(
          6L, 6L, 6L, 6L, 2L,
          6L, 6L, 4L, 2L, 2L, 6L, 2L, 4L, 6L, 6L, 2L, 6L, 4L, 4L, 2L,
          2L, 4L, 4L, 6L, 6L, 6L, 6L, 2L, 2L, 2L, 4L, 2L
        ), levels = c(
          "3",
          "4", "5", "6", "7", "8"
        ), class = "factor"), vs.factor = structure(c(
          2L,
          2L, 2L, 2L, 1L, 2L, 2L, 2L, 1L, 1L, 2L, 1L, 1L, 2L, 2L, 1L,
          2L, 2L, 2L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 1L, 1L, 1L,
          1L
        ), levels = c("Yes", "No"), class = "factor"), am.factor = structure(c(
          1L,
          1L, 1L, 1L, 2L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 1L, 1L, 1L, 2L,
          2L, 2L, 2L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 1L, 1L,
          2L
        ), levels = c("Automatic", "Manual"), class = "factor"),
        gear.factor = structure(c(
          1L, 1L, 1L, 1L, 2L, 1L, 1L, 3L,
          2L, 2L, 3L, 2L, 1L, 1L, 1L, 3L, 3L, 2L, 2L, 2L, 2L, 2L, 2L,
          1L, 1L, 1L, 1L, 3L, 2L, 1L, 1L, 2L
        ), levels = c(
          "3", "4",
          "5"
        ), class = "factor"), carb.factor = structure(c(
          2L, 4L,
          4L, 4L, 1L, 2L, 4L, 6L, 1L, 1L, 4L, 2L, 1L, 2L, 4L, 2L, 8L,
          4L, 4L, 2L, 2L, 4L, 4L, 3L, 3L, 3L, 2L, 2L, 1L, 1L, 1L, 2L
        ), levels = c("1", "2", "3", "4", "5", "6", "7", "8"), class = "factor"),
        color_available___red.factor = structure(c(
          2L, 1L, 1L, 1L,
          1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L,
          1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L
        ), levels = c(
          "Unchecked",
          "Checked"
        ), class = "factor"), color_available___green.factor = structure(c(
          2L,
          1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L,
          1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L,
          1L
        ), levels = c("Unchecked", "Checked"), class = "factor"),
        color_available___blue.factor = structure(c(
          2L, 1L, 1L, 1L,
          1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L,
          1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L
        ), levels = c(
          "Unchecked",
          "Checked"
        ), class = "factor"), color_available___black.factor = structure(c(
          1L,
          1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L,
          1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L,
          1L
        ), levels = c("Unchecked", "Checked"), class = "factor"),
        motor_trend_cars_complete.factor = structure(c(
          2L, 1L, 1L,
          1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L,
          1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L
        ), levels = c(
          "Incomplete",
          "Unverified", "Complete"
        ), class = "factor"), letter_group___a.factor = structure(c(
          2L,
          1L, 2L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L,
          1L, 1L, 2L, 2L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L,
          1L
        ), levels = c("Unchecked", "Checked"), class = "factor"),
        letter_group___b.factor = structure(c(
          2L, 1L, 1L, 2L, 2L,
          1L, 2L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L,
          1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L
        ), levels = c(
          "Unchecked",
          "Checked"
        ), class = "factor"), letter_group___c.factor = structure(c(
          1L,
          1L, 2L, 2L, 2L, 1L, 2L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L,
          1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L,
          1L
        ), levels = c("Unchecked", "Checked"), class = "factor"),
        choice.factor = structure(c(
          2L, NA, 1L, 1L, NA, NA, 1L, NA,
          NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 2L, NA, NA, NA,
          NA, NA, NA, NA, NA, NA, NA, NA, NA
        ), levels = c(
          "Choice 1",
          "Choice 2"
        ), class = "factor"), grouping_complete.factor = structure(c(
          3L,
          1L, 3L, 3L, 1L, 1L, 2L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L,
          1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L,
          1L
        ), levels = c("Incomplete", "Unverified", "Complete"), class = "factor")
      ), row.names = c(
        1L,
        5L, 6L, 9L, 11L, 12L, 13L, 18L, 19L, 20L, 21L, 22L, 23L, 24L,
        25L, 26L, 27L, 28L, 29L, 30L, 34L, 35L, 36L, 37L, 38L, 39L, 40L,
        41L, 42L, 43L, 44L, 45L
      ), class = "data.frame"), sale = structure(list(
        row = structure(c(
          1L, 1L, 1L, 3L, 3L, 4L, 7L, 7L, 7L, 7L,
          20L, 20L, 20L
        ), levels = c(
          "AMC Javelin", "Cadillac Fleetwood",
          "Camaro Z28", "Chrysler Imperial", "Datsun 710", "Dodge Challenger",
          "Duster 360", "Ferrari Dino", "Fiat 128", "Fiat X1-9", "Ford Pantera L",
          "Honda Civic", "Hornet 4 Drive", "Hornet Sportabout", "Lincoln Continental",
          "Lotus Europa", "Maserati Bora", "Mazda RX4", "Mazda RX4 Wag",
          "Merc 230", "Merc 240D", "Merc 280", "Merc 280C", "Merc 450SE",
          "Merc 450SL", "Merc 450SLC", "Pontiac Firebird", "Porsche 914-2",
          "Toyota Corolla", "Toyota Corona", "Valiant", "Volvo 142E"
        ), class = c("labelled", "factor"), label = "Name"), redcap_repeat_instrument = c(
          "sale",
          "sale", "sale", "sale", "sale", "sale", "sale", "sale", "sale",
          "sale", "sale", "sale", "sale"
        ), redcap_repeat_instance = structure(c(
          1L,
          2L, 3L, 1L, 2L, 1L, 1L, 2L, 3L, 4L, 1L, 2L, 3L
        ), label = "Repeat Instance", class = c(
          "labelled",
          "integer"
        )), price = structure(c(
          12000.5, 13750.77, 15004.57,
          7800, 8000, 7500, 8756.4, 6800.88, 8888.88, 970, 7800.98,
          7954, 6800.55
        ), label = "Sale price", class = c(
          "labelled",
          "numeric"
        )), color = structure(c(
          1L, 3L, 2L, 2L, 3L, 1L,
          4L, 2L, 1L, 4L, 2L, 1L, 3L
        ), label = "Color", class = c(
          "labelled",
          "integer"
        )), customer = structure(c(
          2L, 12L, 7L, 4L, 14L,
          5L, 10L, 8L, 3L, 6L, 13L, 9L, 11L
        ), levels = c(
          "", "Bob",
          "Erica", "Janice", "Jim", "Juan", "Kim", "Pablo", "Quentin",
          "Sarah", "Sharon", "Sue", "Ted", "Tim"
        ), class = c(
          "labelled",
          "factor"
        ), label = "Customer Name"), sale_complete = structure(c(
          0L,
          2L, 0L, 2L, 0L, 2L, 1L, 0L, 0L, 0L, 0L, 0L, 2L
        ), label = "Complete?", class = c(
          "labelled",
          "integer"
        )), redcap_repeat_instrument.factor = structure(c(
          1L,
          1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L
        ), levels = "Sale", class = "factor"),
        color.factor = structure(c(
          1L, 3L, 2L, 2L, 3L, 1L, 4L, 2L,
          1L, 4L, 2L, 1L, 3L
        ), levels = c("red", "green", "blue", "black"), class = "factor"), sale_complete.factor = structure(c(
          1L,
          3L, 1L, 3L, 1L, 3L, 2L, 1L, 1L, 1L, 1L, 1L, 3L
        ), levels = c(
          "Incomplete",
          "Unverified", "Complete"
        ), class = "factor")
      ), row.names = c(
        2L,
        3L, 4L, 7L, 8L, 10L, 14L, 15L, 16L, 17L, 31L, 32L, 33L
      ), class = "data.frame"))
    )
  })
}


if (requireNamespace("readr", quietly = TRUE)) {
  metadata <-
    readr::read_csv(get_data_location(
      "ExampleProject_DataDictionary_2018-06-07.csv"
    ))

  records <-
    readr::read_csv(get_data_location(
      "ExampleProject_DATA_2018-06-07_1129.csv"
    ))

  redcap_output_readr <- REDCap_split(records, metadata)

  expect_matching_elements <- function(FUN) {
    FUN <- match.fun(FUN)
    expect_identical(
      lapply(redcap_output_readr, FUN),
      lapply(redcap_output_csv1, FUN)
    )
  }

  test_that("Result of data read in with `readr` will
            match result with `read.csv`", {
    # The list itself
    expect_identical(
      length(redcap_output_readr),
      length(redcap_output_csv1)
    )
    expect_identical(
      names(redcap_output_readr),
      names(redcap_output_csv1)
    )

    # Each element of the list
    expect_matching_elements(names)
    expect_matching_elements(dim)
  })
}
