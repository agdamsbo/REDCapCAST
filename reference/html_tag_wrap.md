# Simple html tag wrapping for REDCap text formatting

Simple html tag wrapping for REDCap text formatting

## Usage

``` r
html_tag_wrap(data, tag = "h2", extra = NULL)
```

## Arguments

- data:

  character vector

- tag:

  character vector length 1

- extra:

  character vector

## Value

character vector

## Examples

``` r
html_tag_wrap("Titel", tag = "div", extra = 'class="rich-text-field-label"')
#> <div class="rich-text-field-label">Titel</div>
html_tag_wrap("Titel", tag = "h2")
#> <h2>Titel</h2>
```
