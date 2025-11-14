# Split a data frame into separate tables for each form

Split a data frame into separate tables for each form

## Usage

``` r
split_non_repeating_forms(table, universal_fields, fields)
```

## Arguments

- table:

  A data frame

- universal_fields:

  A character vector of fields that should be included in every table

- fields:

  A two-column matrix containing the names of fields that should be
  included in each form

## Value

A list of data frames, one for each non-repeating form

## Examples

``` r
# Create a table
table <- data.frame(
  id = c(1, 2, 3, 4, 5),
  form_a_name = c("John", "Alice", "Bob", "Eve", "Mallory"),
  form_a_age = c(25, 30, 25, 15, 20),
  form_b_name = c("John", "Alice", "Bob", "Eve", "Mallory"),
  form_b_gender = c("M", "F", "M", "F", "F")
)

# Create the universal fields
universal_fields <- c("id")

# Create the fields
fields <- matrix(
  c(
    "form_a_name", "form_a",
    "form_a_age", "form_a",
    "form_b_name", "form_b",
    "form_b_gender", "form_b"
  ),
  ncol = 2, byrow = TRUE
)

# Split the table
split_non_repeating_forms(table, universal_fields, fields)
#> $form_a_age
#>   id
#> 1  1
#> 2  2
#> 3  3
#> 4  4
#> 5  5
#> 
```
