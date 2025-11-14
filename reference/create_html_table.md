# Create two-column HTML table for data piping in REDCap instruments

Create two-column HTML table for data piping in REDCap instruments

## Usage

``` r
create_html_table(text, variable)
```

## Arguments

- text:

  descriptive text

- variable:

  variable to pipe

## Value

character vector

## Examples

``` r
create_html_table(text = "Patient ID", variable = c("[cpr]"))
#> <table style="border-collapse: collapse; width: 100%;" border="0"> <tbody><tr> <td style="width: 58%;"> <h5><span style="font-weight: normal;">Patient ID<br /></span></h5> </td> <td style="width: 42%; text-align: left;"> <h5><span style="font-weight: bold;">[cpr]</span></h5> </td> </tr></tbody> </table>
create_html_table(text = paste("assessor", 1:2, sep = "_"), variable = c("[cpr]"))
#> <table style="border-collapse: collapse; width: 100%;" border="0"> <tbody><tr> <td style="width: 58%;"> <h5><span style="font-weight: normal;">assessor_1<br /></span></h5> </td> <td style="width: 42%; text-align: left;"> <h5><span style="font-weight: bold;">[cpr]</span></h5> </td> </tr><tr> <td style="width: 58%;"> <h5><span style="font-weight: normal;">assessor_2<br /></span></h5> </td> <td style="width: 42%; text-align: left;"> <h5><span style="font-weight: bold;">[cpr]</span></h5> </td> </tr></tbody> </table>
# create_html_table(text = c("CPR nummer","Word"), variable = c("[cpr][1]", "[cpr][2]", "[test]"))
```
