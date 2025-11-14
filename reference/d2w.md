# Convert single digits to words

Convert single digits to words

## Usage

``` r
d2w(x, lang = "en", neutrum = FALSE, everything = FALSE)
```

## Arguments

- x:

  data. Handle vectors, data.frames and lists

- lang:

  language. Danish (da) and English (en), Default is "en"

- neutrum:

  for numbers depending on counted word

- everything:

  flag to also split numbers \>9 to single digits

## Value

returns characters in same format as input

## Examples

``` r
d2w(c(2:8, 21))
#> [1] "two"   "three" "four"  "five"  "six"   "seven" "eight" "21"   
d2w(data.frame(2:7, 3:8, 1), lang = "da", neutrum = TRUE)
#>   X2.7 X3.8 X1
#> 1   to  tre et
#> 2  tre fire et
#> 3 fire  fem et
#> 4  fem seks et
#> 5 seks  syv et
#> 6  syv otte et

## If everything=T, also larger numbers are reduced.
## Elements in the list are same length as input
d2w(list(2:8, c(2, 6, 4, 23), 2), everything = TRUE)
#> [[1]]
#> [1] "two"   "three" "four"  "five"  "six"   "seven" "eight"
#> 
#> [[2]]
#> [1] "two"       "six"       "four"      "two three"
#> 
#> [[3]]
#> [1] "two"
#> 
```
