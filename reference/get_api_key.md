# Retrieve project API key if stored, if not, set and retrieve

Attempting to make secure API key storage so simple, that no other way
makes sense. Wrapping
[key_get](https://keyring.r-lib.org/reference/key_get.html) and
[key_set](https://keyring.r-lib.org/reference/key_get.html) using the
[key_list](https://keyring.r-lib.org/reference/key_get.html) to check if
key is in storage already.

## Usage

``` r
get_api_key(key.name, ...)
```

## Arguments

- key.name:

  character vector of key name

- ...:

  passed to [key_set](https://keyring.r-lib.org/reference/key_get.html)

## Value

character vector
