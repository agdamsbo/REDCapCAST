# clean_redcap_name

Stepwise removal on non-alphanumeric characters, trailing white space,
substitutes spaces for underscores and converts to lower case. Trying to
make up for different naming conventions.

## Usage

``` r
clean_redcap_name(x)
```

## Arguments

- x:

  vector or data frame for cleaning

## Value

vector or data frame, same format as input

## Examples

``` r
"Research!, ne:ws? and c;l-.ls" |> clean_redcap_name()
#> [1] "research_news_and_clls"
"8_new_TEST_" |> clean_redcap_name()
#> [1] "8_new_test"
```
