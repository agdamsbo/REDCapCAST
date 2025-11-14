# Extract data from stata file for data dictionary

Extract data from stata file for data dictionary

## Usage

``` r
ds2dd_detailed(
  data,
  add.auto.id = FALSE,
  date.format = "dmy",
  form.name = NULL,
  form.sep = NULL,
  form.prefix = TRUE,
  field.type = NULL,
  field.label = NULL,
  field.label.attr = "label",
  field.validation = NULL,
  metadata = names(REDCapCAST::redcapcast_meta),
  convert.logicals = FALSE
)
```

## Arguments

- data:

  data frame

- add.auto.id:

  flag to add id column

- date.format:

  date format, character string. ymd/dmy/mdy. dafault is dmy.

- form.name:

  manually specify form name(s). Vector of length 1 or ncol(data).
  Default is NULL and "data" is used.

- form.sep:

  If supplied dataset has form names as suffix or prefix to the
  column/variable names, the seperator can be specified. If supplied,
  the form.name is ignored. Default is NULL.

- form.prefix:

  Flag to set if form is prefix (TRUE) or suffix (FALSE) to the column
  names. Assumes all columns have pre- or suffix if specified.

- field.type:

  manually specify field type(s). Vector of length 1 or ncol(data).
  Default is NULL and "text" is used for everything but factors, which
  wil get "radio".

- field.label:

  manually specify field label(s). Vector of length 1 or ncol(data).
  Default is NULL and colnames(data) is used or attribute
  \`field.label.attr\` for haven_labelled data set (imported .dta file
  with \`haven::read_dta()\`).

- field.label.attr:

  attribute name for named labels for haven_labelled data set (imported
  .dta file with \`haven::read_dta()\`. Default is "label"

- field.validation:

  manually specify field validation(s). Vector of length 1 or
  ncol(data). Default is NULL and \`levels()\` are used for factors or
  attribute \`factor.labels.attr\` for haven_labelled data set (imported
  .dta file with \`haven::read_dta()\`).

- metadata:

  redcap metadata headings. Default is
  names(REDCapCAST::redcapcast_meta).

- convert.logicals:

  convert logicals to factor. Default is TRUE.

## Value

list of length 2

## Details

This function is a natural development of the ds2dd() function. It
assumes that the first column is the ID-column. No checks. Please, do
always inspect the data dictionary before upload.

Ensure, that the data set is formatted with as much information as
possible.

\`field.type\` can be supplied

## Examples

``` r
## Basic parsing with default options
requireNamespace("REDCapCAST")
redcapcast_data |>
  dplyr::select(-dplyr::starts_with("redcap_")) |>
  ds2dd_detailed()
#> $data
#> # A tibble: 25 × 24
#>    record_id cpr    inclusion  inclusion_time dob          age age_integer sex  
#>        <dbl> <chr>  <date>     <chr>          <date>     <dbl>       <dbl> <chr>
#>  1         1 12034… 2023-03-13 12:38:49       1940-03-12  83.0          83 fema…
#>  2         2 01023… 2023-03-01 10:38:57       1934-02-01  89.1          89 male 
#>  3         2 NA     NA         NA             NA          NA            NA NA   
#>  4         2 NA     NA         NA             NA          NA            NA NA   
#>  5         3 23015… 2022-03-08 12:01:07       1956-01-23  66.1          66 male 
#>  6         3 NA     NA         NA             NA          NA            NA NA   
#>  7         3 NA     NA         NA             NA          NA            NA NA   
#>  8         3 NA     NA         NA             NA          NA            NA NA   
#>  9         3 NA     NA         NA             NA          NA            NA NA   
#> 10         3 NA     NA         NA             NA          NA            NA NA   
#> # ℹ 15 more rows
#> # ℹ 16 more variables: cohabitation <chr>, hypertension <chr>, diabetes <chr>,
#> #   region <chr>, baseline_data_start_complete <chr>, mrs_assessed <chr>,
#> #   mrs_date <date>, mrs_score <dbl>, mrs_complete <chr>, con_mrs <dbl>,
#> #   con_calc <dbl>, consensus_complete <chr>, event_datetime <dttm>,
#> #   event_age <dbl>, event_type <chr>, new_event_complete <chr>
#> 
#> $meta
#> # A tibble: 24 × 18
#>    field_name     form_name section_header field_type field_label   
#>    <chr>          <chr>     <lgl>          <chr>      <chr>         
#>  1 record_id      data      NA             text       record_id     
#>  2 cpr            data      NA             text       cpr           
#>  3 inclusion      data      NA             text       inclusion     
#>  4 inclusion_time data      NA             text       inclusion_time
#>  5 dob            data      NA             text       dob           
#>  6 age            data      NA             text       age           
#>  7 age_integer    data      NA             text       age_integer   
#>  8 sex            data      NA             text       sex           
#>  9 cohabitation   data      NA             text       cohabitation  
#> 10 hypertension   data      NA             text       hypertension  
#> # ℹ 14 more rows
#> # ℹ 13 more variables: select_choices_or_calculations <lgl>, field_note <lgl>,
#> #   text_validation_type_or_show_slider_number <chr>,
#> #   text_validation_min <lgl>, text_validation_max <lgl>, identifier <lgl>,
#> #   branching_logic <lgl>, required_field <lgl>, custom_alignment <lgl>,
#> #   question_number <lgl>, matrix_group_name <lgl>, matrix_ranking <lgl>,
#> #   field_annotation <lgl>
#> 
#> attr(,"class")
#> [1] "REDCapCAST" "list"      

## Adding a record_id field
iris |> ds2dd_detailed(add.auto.id = TRUE)
#> $data
#> # A tibble: 150 × 6
#>    record_id sepallength sepalwidth petallength petalwidth species
#>        <int>       <dbl>      <dbl>       <dbl>      <dbl>   <dbl>
#>  1         1         5.1        3.5         1.4        0.2       1
#>  2         2         4.9        3           1.4        0.2       1
#>  3         3         4.7        3.2         1.3        0.2       1
#>  4         4         4.6        3.1         1.5        0.2       1
#>  5         5         5          3.6         1.4        0.2       1
#>  6         6         5.4        3.9         1.7        0.4       1
#>  7         7         4.6        3.4         1.4        0.3       1
#>  8         8         5          3.4         1.5        0.2       1
#>  9         9         4.4        2.9         1.4        0.2       1
#> 10        10         4.9        3.1         1.5        0.1       1
#> # ℹ 140 more rows
#> 
#> $meta
#> # A tibble: 6 × 18
#>   field_name  form_name section_header field_type field_label 
#>   <chr>       <chr>     <lgl>          <chr>      <chr>       
#> 1 record_id   data      NA             text       record_id   
#> 2 sepallength data      NA             text       Sepal.Length
#> 3 sepalwidth  data      NA             text       Sepal.Width 
#> 4 petallength data      NA             text       Petal.Length
#> 5 petalwidth  data      NA             text       Petal.Width 
#> 6 species     data      NA             radio      Species     
#> # ℹ 13 more variables: select_choices_or_calculations <chr>, field_note <lgl>,
#> #   text_validation_type_or_show_slider_number <chr>,
#> #   text_validation_min <lgl>, text_validation_max <lgl>, identifier <lgl>,
#> #   branching_logic <lgl>, required_field <lgl>, custom_alignment <lgl>,
#> #   question_number <lgl>, matrix_group_name <lgl>, matrix_ranking <lgl>,
#> #   field_annotation <lgl>
#> 
#> attr(,"class")
#> [1] "REDCapCAST" "list"      

## Passing form name information to function
iris |>
  ds2dd_detailed(
    add.auto.id = TRUE,
    form.name = sample(c("b", "c"), size = 6, replace = TRUE, prob = rep(.5, 2))
  ) |>
  purrr::pluck("meta")
#> # A tibble: 6 × 18
#>   field_name  form_name section_header field_type field_label 
#>   <chr>       <chr>     <lgl>          <chr>      <chr>       
#> 1 record_id   c         NA             text       record_id   
#> 2 sepallength b         NA             text       Sepal.Length
#> 3 sepalwidth  c         NA             text       Sepal.Width 
#> 4 petallength b         NA             text       Petal.Length
#> 5 petalwidth  c         NA             text       Petal.Width 
#> 6 species     b         NA             radio      Species     
#> # ℹ 13 more variables: select_choices_or_calculations <chr>, field_note <lgl>,
#> #   text_validation_type_or_show_slider_number <chr>,
#> #   text_validation_min <lgl>, text_validation_max <lgl>, identifier <lgl>,
#> #   branching_logic <lgl>, required_field <lgl>, custom_alignment <lgl>,
#> #   question_number <lgl>, matrix_group_name <lgl>, matrix_ranking <lgl>,
#> #   field_annotation <lgl>
mtcars |>
  dplyr::mutate(unknown = NA) |>
  numchar2fct() |>
  ds2dd_detailed(add.auto.id = TRUE)
#> $data
#> # A tibble: 32 × 13
#>    record_id   mpg   cyl  disp    hp  drat    wt  qsec    vs    am  gear  carb
#>        <int> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#>  1         1  21       2  160    110  3.9   2.62  16.5     1     2     2     4
#>  2         2  21       2  160    110  3.9   2.88  17.0     1     2     2     4
#>  3         3  22.8     1  108     93  3.85  2.32  18.6     2     2     2     1
#>  4         4  21.4     2  258    110  3.08  3.22  19.4     2     1     1     1
#>  5         5  18.7     3  360    175  3.15  3.44  17.0     1     1     1     2
#>  6         6  18.1     2  225    105  2.76  3.46  20.2     2     1     1     1
#>  7         7  14.3     3  360    245  3.21  3.57  15.8     1     1     1     4
#>  8         8  24.4     1  147.    62  3.69  3.19  20       2     1     2     2
#>  9         9  22.8     1  141.    95  3.92  3.15  22.9     2     1     2     2
#> 10        10  19.2     2  168.   123  3.92  3.44  18.3     2     1     2     4
#> # ℹ 22 more rows
#> # ℹ 1 more variable: unknown <dbl>
#> 
#> $meta
#> # A tibble: 13 × 18
#>    field_name form_name section_header field_type field_label
#>    <chr>      <chr>     <lgl>          <chr>      <chr>      
#>  1 record_id  data      NA             text       record_id  
#>  2 mpg        data      NA             text       mpg        
#>  3 cyl        data      NA             radio      cyl        
#>  4 disp       data      NA             text       disp       
#>  5 hp         data      NA             text       hp         
#>  6 drat       data      NA             text       drat       
#>  7 wt         data      NA             text       wt         
#>  8 qsec       data      NA             text       qsec       
#>  9 vs         data      NA             radio      vs         
#> 10 am         data      NA             radio      am         
#> 11 gear       data      NA             radio      gear       
#> 12 carb       data      NA             radio      carb       
#> 13 unknown    data      NA             truefalse  unknown    
#> # ℹ 13 more variables: select_choices_or_calculations <chr>, field_note <lgl>,
#> #   text_validation_type_or_show_slider_number <chr>,
#> #   text_validation_min <lgl>, text_validation_max <lgl>, identifier <lgl>,
#> #   branching_logic <lgl>, required_field <lgl>, custom_alignment <lgl>,
#> #   question_number <lgl>, matrix_group_name <lgl>, matrix_ranking <lgl>,
#> #   field_annotation <lgl>
#> 
#> attr(,"class")
#> [1] "REDCapCAST" "list"      

## Using column name suffix to carry form name
data <- iris |>
  ds2dd_detailed(add.auto.id = TRUE) |>
  purrr::pluck("data")
names(data) <- glue::glue("{sample(x = c('a','b'),size = length(names(data)),
replace=TRUE,prob = rep(x=.5,2))}__{names(data)}")
data |> ds2dd_detailed(form.sep = "__")
#> $data
#> # A tibble: 150 × 6
#>    record_id sepallength sepalwidth petallength petalwidth species
#>        <int>       <dbl>      <dbl>       <dbl>      <dbl>   <dbl>
#>  1         1         5.1        3.5         1.4        0.2       1
#>  2         2         4.9        3           1.4        0.2       1
#>  3         3         4.7        3.2         1.3        0.2       1
#>  4         4         4.6        3.1         1.5        0.2       1
#>  5         5         5          3.6         1.4        0.2       1
#>  6         6         5.4        3.9         1.7        0.4       1
#>  7         7         4.6        3.4         1.4        0.3       1
#>  8         8         5          3.4         1.5        0.2       1
#>  9         9         4.4        2.9         1.4        0.2       1
#> 10        10         4.9        3.1         1.5        0.1       1
#> # ℹ 140 more rows
#> 
#> $meta
#> # A tibble: 6 × 18
#>   field_name  form_name section_header field_type field_label
#>   <chr>       <chr>     <lgl>          <chr>      <chr>      
#> 1 record_id   b         NA             text       record_id  
#> 2 sepallength a         NA             text       sepallength
#> 3 sepalwidth  a         NA             text       sepalwidth 
#> 4 petallength a         NA             text       petallength
#> 5 petalwidth  a         NA             text       petalwidth 
#> 6 species     a         NA             text       species    
#> # ℹ 13 more variables: select_choices_or_calculations <lgl>, field_note <lgl>,
#> #   text_validation_type_or_show_slider_number <chr>,
#> #   text_validation_min <lgl>, text_validation_max <lgl>, identifier <lgl>,
#> #   branching_logic <lgl>, required_field <lgl>, custom_alignment <lgl>,
#> #   question_number <lgl>, matrix_group_name <lgl>, matrix_ranking <lgl>,
#> #   field_annotation <lgl>
#> 
#> attr(,"class")
#> [1] "REDCapCAST" "list"      
```
