# Very simple function to remove rich text formatting from field label and save the first paragraph ('\<p\>...\</p\>').

Very simple function to remove rich text formatting from field label and
save the first paragraph ('\<p\>...\</p\>').

## Usage

``` r
clean_field_label(data)
```

## Arguments

- data:

  field label

## Value

character vector

## Examples

``` r
clean_field_label("<div class=\"rich-text-field-label\"><p>Fazekas score</p></div>")
#> [1] "Fazekas score"
```
