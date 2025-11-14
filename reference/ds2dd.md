# (DEPRECATED) Data set to data dictionary function

Creates a very basic data dictionary skeleton. Please see
\`ds2dd_detailed()\` for a more advanced function.

## Usage

``` r
ds2dd(
  ds,
  record.id = "record_id",
  form.name = "basis",
  field.type = "text",
  field.label = NULL,
  include.column.names = FALSE,
  metadata = names(REDCapCAST::redcapcast_meta)
)
```

## Arguments

- ds:

  data set

- record.id:

  name or column number of id variable, moved to first row of data
  dictionary, character of integer. Default is "record_id".

- form.name:

  vector of form names, character string, length 1 or length equal to
  number of variables. Default is "basis".

- field.type:

  vector of field types, character string, length 1 or length equal to
  number of variables. Default is "text.

- field.label:

  vector of form names, character string, length 1 or length equal to
  number of variables. Default is NULL and is then identical to field
  names.

- include.column.names:

  Flag to give detailed output including new column names for original
  data set for upload.

- metadata:

  Metadata column names. Default is the included
  names(REDCapCAST::redcapcast_meta).

## Value

data.frame or list of data.frame and vector

## Details

Migrated from stRoke ds2dd(). Fits better with the functionality of
'REDCapCAST'.

## Examples

``` r
redcapcast_data$record_id <- seq_len(nrow(redcapcast_data))
ds2dd(redcapcast_data, include.column.names = TRUE)
#> $DataDictionary
#>                      field_name form_name section_header field_type
#> 1                     record_id     basis             NA       text
#> 2             redcap_event_name     basis             NA       text
#> 3      redcap_repeat_instrument     basis             NA       text
#> 4        redcap_repeat_instance     basis             NA       text
#> 5                           cpr     basis             NA       text
#> 6                     inclusion     basis             NA       text
#> 7                inclusion_time     basis             NA       text
#> 8                           dob     basis             NA       text
#> 9                           age     basis             NA       text
#> 10                  age_integer     basis             NA       text
#> 11                          sex     basis             NA       text
#> 12                 cohabitation     basis             NA       text
#> 13                 hypertension     basis             NA       text
#> 14                     diabetes     basis             NA       text
#> 15                       region     basis             NA       text
#> 16 baseline_data_start_complete     basis             NA       text
#> 17                 mrs_assessed     basis             NA       text
#> 18                     mrs_date     basis             NA       text
#> 19                    mrs_score     basis             NA       text
#> 20                 mrs_complete     basis             NA       text
#> 21                      con_mrs     basis             NA       text
#> 22                     con_calc     basis             NA       text
#> 23           consensus_complete     basis             NA       text
#> 24               event_datetime     basis             NA       text
#> 25                    event_age     basis             NA       text
#> 26                   event_type     basis             NA       text
#> 27           new_event_complete     basis             NA       text
#>                     field_label select_choices_or_calculations field_note
#> 1                     record_id                             NA         NA
#> 2             redcap_event_name                             NA         NA
#> 3      redcap_repeat_instrument                             NA         NA
#> 4        redcap_repeat_instance                             NA         NA
#> 5                           cpr                             NA         NA
#> 6                     inclusion                             NA         NA
#> 7                inclusion_time                             NA         NA
#> 8                           dob                             NA         NA
#> 9                           age                             NA         NA
#> 10                  age_integer                             NA         NA
#> 11                          sex                             NA         NA
#> 12                 cohabitation                             NA         NA
#> 13                 hypertension                             NA         NA
#> 14                     diabetes                             NA         NA
#> 15                       region                             NA         NA
#> 16 baseline_data_start_complete                             NA         NA
#> 17                 mrs_assessed                             NA         NA
#> 18                     mrs_date                             NA         NA
#> 19                    mrs_score                             NA         NA
#> 20                 mrs_complete                             NA         NA
#> 21                      con_mrs                             NA         NA
#> 22                     con_calc                             NA         NA
#> 23           consensus_complete                             NA         NA
#> 24               event_datetime                             NA         NA
#> 25                    event_age                             NA         NA
#> 26                   event_type                             NA         NA
#> 27           new_event_complete                             NA         NA
#>    text_validation_type_or_show_slider_number text_validation_min
#> 1                                          NA                  NA
#> 2                                          NA                  NA
#> 3                                          NA                  NA
#> 4                                          NA                  NA
#> 5                                          NA                  NA
#> 6                                          NA                  NA
#> 7                                          NA                  NA
#> 8                                          NA                  NA
#> 9                                          NA                  NA
#> 10                                         NA                  NA
#> 11                                         NA                  NA
#> 12                                         NA                  NA
#> 13                                         NA                  NA
#> 14                                         NA                  NA
#> 15                                         NA                  NA
#> 16                                         NA                  NA
#> 17                                         NA                  NA
#> 18                                         NA                  NA
#> 19                                         NA                  NA
#> 20                                         NA                  NA
#> 21                                         NA                  NA
#> 22                                         NA                  NA
#> 23                                         NA                  NA
#> 24                                         NA                  NA
#> 25                                         NA                  NA
#> 26                                         NA                  NA
#> 27                                         NA                  NA
#>    text_validation_max identifier branching_logic required_field
#> 1                   NA         NA              NA             NA
#> 2                   NA         NA              NA             NA
#> 3                   NA         NA              NA             NA
#> 4                   NA         NA              NA             NA
#> 5                   NA         NA              NA             NA
#> 6                   NA         NA              NA             NA
#> 7                   NA         NA              NA             NA
#> 8                   NA         NA              NA             NA
#> 9                   NA         NA              NA             NA
#> 10                  NA         NA              NA             NA
#> 11                  NA         NA              NA             NA
#> 12                  NA         NA              NA             NA
#> 13                  NA         NA              NA             NA
#> 14                  NA         NA              NA             NA
#> 15                  NA         NA              NA             NA
#> 16                  NA         NA              NA             NA
#> 17                  NA         NA              NA             NA
#> 18                  NA         NA              NA             NA
#> 19                  NA         NA              NA             NA
#> 20                  NA         NA              NA             NA
#> 21                  NA         NA              NA             NA
#> 22                  NA         NA              NA             NA
#> 23                  NA         NA              NA             NA
#> 24                  NA         NA              NA             NA
#> 25                  NA         NA              NA             NA
#> 26                  NA         NA              NA             NA
#> 27                  NA         NA              NA             NA
#>    custom_alignment question_number matrix_group_name matrix_ranking
#> 1                NA              NA                NA             NA
#> 2                NA              NA                NA             NA
#> 3                NA              NA                NA             NA
#> 4                NA              NA                NA             NA
#> 5                NA              NA                NA             NA
#> 6                NA              NA                NA             NA
#> 7                NA              NA                NA             NA
#> 8                NA              NA                NA             NA
#> 9                NA              NA                NA             NA
#> 10               NA              NA                NA             NA
#> 11               NA              NA                NA             NA
#> 12               NA              NA                NA             NA
#> 13               NA              NA                NA             NA
#> 14               NA              NA                NA             NA
#> 15               NA              NA                NA             NA
#> 16               NA              NA                NA             NA
#> 17               NA              NA                NA             NA
#> 18               NA              NA                NA             NA
#> 19               NA              NA                NA             NA
#> 20               NA              NA                NA             NA
#> 21               NA              NA                NA             NA
#> 22               NA              NA                NA             NA
#> 23               NA              NA                NA             NA
#> 24               NA              NA                NA             NA
#> 25               NA              NA                NA             NA
#> 26               NA              NA                NA             NA
#> 27               NA              NA                NA             NA
#>    field_annotation
#> 1                NA
#> 2                NA
#> 3                NA
#> 4                NA
#> 5                NA
#> 6                NA
#> 7                NA
#> 8                NA
#> 9                NA
#> 10               NA
#> 11               NA
#> 12               NA
#> 13               NA
#> 14               NA
#> 15               NA
#> 16               NA
#> 17               NA
#> 18               NA
#> 19               NA
#> 20               NA
#> 21               NA
#> 22               NA
#> 23               NA
#> 24               NA
#> 25               NA
#> 26               NA
#> 27               NA
#> 
#> $`Column names`
#>  [1] "record_id"                    "redcap_event_name"           
#>  [3] "redcap_repeat_instrument"     "redcap_repeat_instance"      
#>  [5] "cpr"                          "inclusion"                   
#>  [7] "inclusion_time"               "dob"                         
#>  [9] "age"                          "age_integer"                 
#> [11] "sex"                          "cohabitation"                
#> [13] "hypertension"                 "diabetes"                    
#> [15] "region"                       "baseline_data_start_complete"
#> [17] "mrs_assessed"                 "mrs_date"                    
#> [19] "mrs_score"                    "mrs_complete"                
#> [21] "con_mrs"                      "con_calc"                    
#> [23] "consensus_complete"           "event_datetime"              
#> [25] "event_age"                    "event_type"                  
#> [27] "new_event_complete"          
#> 
```
