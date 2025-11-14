# Extended string splitting

Can be used as a substitute of the base function. Main claim to fame is
easing the split around the defined delimiter, see example.

## Usage

``` r
strsplitx(x, split, type = "classic", perl = FALSE, ...)
```

## Arguments

- x:

  data

- split:

  delimiter

- type:

  Split type. Can be c("classic", "before", "after", "around")

- perl:

  perl param from strsplit()

- ...:

  additional parameters are passed to base strsplit handling splits

## Value

list

## Examples

``` r
test <- c("12 months follow-up", "3 steps", "mRS 6 weeks",
"Counting to 231 now")
strsplitx(test, "[0-9]", type = "around")
#> [[1]]
#> [1] "1"                 "2"                 " months follow-up"
#> 
#> [[2]]
#> [1] "3"      " steps"
#> 
#> [[3]]
#> [1] "mRS "   "6"      " weeks"
#> 
#> [[4]]
#> [1] "Counting to " "2"            "3"            "1"            " now"        
#> 
```
