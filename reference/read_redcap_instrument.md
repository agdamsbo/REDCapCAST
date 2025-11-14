# Convenience function to download complete instrument, using token storage in keyring.

Convenience function to download complete instrument, using token
storage in keyring.

## Usage

``` r
read_redcap_instrument(
  key,
  uri,
  instrument,
  raw_or_label = "raw",
  id_name = "record_id",
  records = NULL
)
```

## Arguments

- key:

  key name in standard keyring for token retrieval.

- uri:

  REDCap database API uri

- instrument:

  instrument name

- raw_or_label:

  raw or label passed to \`REDCapR::redcap_read()\`

- id_name:

  id variable name. Default is "record_id".

- records:

  specify the records to download. Index numbers. Numeric vector.

## Value

data.frame
