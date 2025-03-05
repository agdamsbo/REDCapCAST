#' Retrieve project API key if stored, if not, set and retrieve
#'
#' @description
#' Attempting to make secure API key storage so simple, that no other way makes
#' sense. Wrapping \link[keyring]{key_get} and \link[keyring]{key_set} using the
#' \link[keyring]{key_list} to check if key is in storage already.
#'
#'
#' @param key.name character vector of key name
#' @param ... passed to \link[keyring]{key_set}
#'
#' @return character vector
#' @importFrom keyring key_list key_get key_set
#' @export
get_api_key <- function(key.name, ...) {
  if (key.name %in% keyring::key_list()$service) {
    keyring::key_get(service = key.name)
  } else {
    keyring::key_set(service = key.name, ...)
    keyring::key_get(service = key.name)
  }
}


#' Secure API key storage and data acquisition in one
#'
#' @param project.name The name of the current project (for key storage with
#' \link[keyring]{key_set}, using the default keyring)
#' @param widen.data argument to widen the exported data. [DEPRECATED], use
#' `data_format`instead
#' @param uri REDCap database API uri
#' @param raw_or_label argument passed on to
#' \link[REDCapCAST]{read_redcap_tables}. Default is "both" to get labelled
#' data.
#' @param data_format Choose the data
#' @param ... arguments passed on to \link[REDCapCAST]{read_redcap_tables}.
#'
#' @return data.frame or list depending on widen.data
#' @export
#'
#' @examples
#' \dontrun{
#' easy_redcap("My_new_project", fields = c("record_id", "age", "hypertension"))
#' }
easy_redcap <- function(project.name,
                        uri,
                        raw_or_label = "both",
                        data_format = c("wide", "list", "redcap", "long"),
                        widen.data = NULL,
                        ...) {
  data_format <- match.arg(data_format)

  # Interpretation of "widen.data" is kept and will override "data_format"
  # for legacy sake
  if (isTRUE(widen.data)) {
    data_format <- "wide"
  }

  if (data_format %in% c("wide", "list")) {
    split_action <- "all"
  } else {
    split_action <- "none"
  }

  key <- get_api_key(
    key.name = paste0(project.name, "_REDCAP_API"),
    prompt = "Provide REDCap API key:"
  )

  redcap_data <- read_redcap_tables(
    uri = uri,
    token = key,
    raw_or_label = raw_or_label,
    split_forms = split_action,
    ...
  )

  # For now, long data format is just legacy REDCap
  # All options are written out for future improvements
  if (data_format == "wide") {
    out <- redcap_data |>
      redcap_wider() |>
      suffix2label()
  } else if (data_format == "list") {
    # The read_redcap_tables() output is a list of tables (forms)
    out <- redcap_data
  } else if (data_format == "long") {
    out <- redcap_data
  } else if (data_format == "redcap") {
    out <- redcap_data
  }

  out
}


