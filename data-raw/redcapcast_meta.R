## code to prepare `redcapcast_meta` dataset goes here
redcapcast_meta <- REDCapR::redcap_metadata_read(
  redcap_uri = keyring::key_get("DB_URI"),
  token = keyring::key_get("cast_api")
)$data

usethis::use_data(redcapcast_meta, overwrite = TRUE)
