# DEPRECATED Helper to import files correctly

DEPRECATED Helper to import files correctly

## Usage

``` r
file_extension(filenames)
```

## Arguments

- filenames:

  file names

## Value

character vector

## Examples

``` r
file_extension(list.files(here::here(""))[[2]])[[1]]
#> [1] ""
file_extension(c("file.cd..ks", "file"))
#> [1] "ks" ""  
```
