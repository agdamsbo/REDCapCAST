mtcars_redcap <- mtcars |>
  dplyr::mutate(
    record_id = seq_len(dplyr::n()),
    name = rownames(mtcars)
  ) |>
  dplyr::select(record_id, dplyr::everything())

mtcars_redcap |>
  write.csv(here::here("data/mtcars_redcap.csv"), row.names = FALSE)

usethis::use_data(mtcars_redcap, overwrite = TRUE)

gtsummary::trial|>
  dplyr::mutate(
    record_id = dplyr::row_number()
  ) |>
  dplyr::select(record_id, dplyr::everything())|>
  write.csv(here::here("drafting/trials_redcap.csv"), row.names = FALSE)

