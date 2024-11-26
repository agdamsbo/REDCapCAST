#' Retrieve project API key if stored, if not, set and retrieve
#'
#' @param key.name character vector of key name
#'
#' @return character vector
#' @importFrom keyring key_list key_get key_set
#' @export
get_api_key <- function(key.name) {
  if (key.name %in% keyring::key_list()$service) {
    keyring::key_get(service = key.name)
  } else {
    keyring::key_set(service = key.name, prompt = "Provide REDCap API key:")
    keyring::key_get(service = key.name)
  }
}


#' Secure API key storage and data acquisition in one
#'
#' @param project.name The name of the current project (for key storage with
#' `keyring::key_set()`, using the default keyring)
#' @param widen.data argument to widen the exported data
#' @param uri REDCap database API uri
#' @param ... arguments passed on to `REDCapCAST::read_redcap_tables()`
#'
#' @return data.frame or list depending on widen.data
#' @export
easy_redcap <- function(project.name, widen.data = TRUE, uri, ...) {
  key <- get_api_key(key.name = paste0(project.name, "_REDCAP_API"))

  out <- read_redcap_tables(
    uri = uri,
    token = key,
    ...
  )

  if (widen.data) {
    out <- out |> redcap_wider()
  }

  out
}


#' REDCap read function to preserve field labels and all factor levels
#'
#' @description
#' This works very much as `read_redcap_tables()` and might end up there
#'
#'
#' @param uri REDCap database API uri
#' @param token API token
#' @param records records to download
#' @param fields fields to download
#' @param events events to download
#' @param forms forms to download
#' @param split_forms Whether to split "repeating" or "all" forms, default is
#' "all".
#'
#' @return data.frame or list
#' @export
#'
read_redcap_labelled <- function(uri,
                                 token,
                                 records = NULL,
                                 fields = NULL,
                                 events = NULL,
                                 forms = NULL,
                                 split_forms = "all") {
  m <-
    REDCapR::redcap_metadata_read(redcap_uri = uri, token = token)[["data"]]

  # Tests
  if (!is.null(fields)) {
    fields_test <- fields %in% c(m$field_name, paste0(unique(m$form_name), "_complete"))

    if (any(!fields_test)) {
      print(paste0(
        "The following field names are invalid: ",
        paste(fields[!fields_test], collapse = ", "), "."
      ))
      stop("Not all supplied field names are valid")
    }
  }


  if (!is.null(forms)) {
    forms_test <- forms %in% unique(m$form_name)

    if (any(!forms_test)) {
      print(paste0(
        "The following form names are invalid: ",
        paste(forms[!forms_test], collapse = ", "), "."
      ))
      stop("Not all supplied form names are valid")
    }
  }

  if (!is.null(events)) {
    arm_event_inst <- REDCapR::redcap_event_instruments(
      redcap_uri = uri,
      token = token
    )

    event_test <- events %in% unique(arm_event_inst$data$unique_event_name)

    if (any(!event_test)) {
      print(paste0(
        "The following event names are invalid: ",
        paste(events[!event_test], collapse = ", "), "."
      ))
      stop("Not all supplied event names are valid")
    }
  }

  # Getting dataset
  d <- REDCapR::redcap_read(
    redcap_uri = uri,
    token = token,
    fields = fields,
    events = events,
    forms = forms,
    records = records,
    raw_or_label = "raw"
  )[["data"]]

  # Applying labels
  d <- purrr::imap(d, \(.x, .i){
    if (.i %in% m$field_name) {
      # Does not handle checkboxes
      out <- set_attr(.x,
        label = clean_field_label(m$field_label[m$field_name == .i]),
        attr = "label"
      )
      out
    } else {
      .x
    }
  }) |> dplyr::bind_cols()

  d <- purrr::imap(d, \(.x, .i){
    if (any(c("radio", "dropdown") %in% m$field_type[m$field_name == .i])) {
      format_redcap_factor(.x, m$select_choices_or_calculations[m$field_name == .i])
    } else {
      .x
    }
  }) |> dplyr::bind_cols()

  # Process repeat instrument naming
  # Removes any extra characters other than a-z, 0-9 and "_", to mimic raw
  # instrument names.
  if ("redcap_repeat_instrument" %in% names(d)) {
    d$redcap_repeat_instrument <- clean_redcap_name(d$redcap_repeat_instrument)
  }

  # Processing metadata to reflect focused dataset
  m <- focused_metadata(m, names(d))

  # Splitting
  out <- REDCap_split(d,
    m,
    forms = split_forms,
    primary_table_name = ""
  )

  sanitize_split(out)
}


#' Very simple function to remove rich text formatting from field label
#' and save the first paragraph ('<p>...</p>').
#'
#' @param data field label
#'
#' @return character vector
#' @export
#'
#' @examples
#' clean_field_label("<div class=\"rich-text-field-label\"><p>Fazekas score</p></div>")
clean_field_label <- function(data) {
  out <- data |>
    lapply(\(.x){
      unlist(strsplit(.x, "</"))[1]
    }) |>
    lapply(\(.x){
      splt <- unlist(strsplit(.x, ">"))
      splt[length(splt)]
    })
  Reduce(c, out)
}


format_redcap_factor <- function(data, meta) {
  lvls <- strsplit(meta, " | ", fixed = TRUE) |>
    unlist() |>
    lapply(\(.x){
      splt <- unlist(strsplit(.x, ", "))
      stats::setNames(splt[1], nm = paste(splt[-1], collapse = ", "))
    }) |>
    (\(.x){
      Reduce(c, .x)
    })()
  set_attr(data, label = lvls, attr = "labels") |>
    set_attr(data, label = "labelled", attr = "class") |>
    as_factor()
}
