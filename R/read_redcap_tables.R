#' Download REDCap data
#'
#' Implementation of REDCap_split with a focused data acquisition approach using
#' REDCapR::redcap_read and only downloading specified fields, forms and/or
#' events using the built-in focused_metadata including some clean-up.
#' Works with classical and longitudinal projects with or without repeating
#' instruments.
#' @param uri REDCap database API uri
#' @param token API token
#' @param records records to download
#' @param fields fields to download
#' @param events events to download
#' @param forms forms to download
#' @param raw_or_label raw or label tags
#' @param split_forms Whether to split "repeating" or "all" forms, default is
#' all.
#'
#' @return list of instruments
#' @importFrom REDCapR redcap_metadata_read redcap_read redcap_event_instruments
#' @include utils.r
#' @export
#'
#' @examples
#' # Examples will be provided later
read_redcap_tables <- function(uri,
                               token,
                               records = NULL,
                               fields = NULL,
                               events = NULL,
                               forms = NULL,
                               raw_or_label = "label",
                               split_forms = "all") {
  # Getting metadata
  m <-
    REDCapR::redcap_metadata_read(redcap_uri = uri, token = token)[["data"]]

  if (!is.null(fields)) {
    fields_test <- fields %in% c(m$field_name,paste0(unique(m$form_name),"_complete"))

    if (any(!fields_test)) {
      print(paste0("The following field names are invalid: ",
                   paste(fields[!fields_test], collapse = ", "), "."))
      stop("Not all supplied field names are valid")
    }
  }


  if (!is.null(forms)) {
    forms_test <- forms %in% unique(m$form_name)

    if (any(!forms_test)) {
      print(paste0("The following form names are invalid: ",
                   paste(forms[!forms_test], collapse = ", "), "."))
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
      print(paste0("The following event names are invalid: ",
                   paste(events[!event_test], collapse = ", "), "."))
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
    raw_or_label = raw_or_label
  )[["data"]]

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
