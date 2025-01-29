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
#' @param widen.data argument to widen the exported data
#' @param uri REDCap database API uri
#' @param raw_or_label argument passed on to
#' \link[REDCapCAST]{read_redcap_tables}. Default is "both" to get labelled
#' data.
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
                        widen.data = TRUE,
                        uri,
                        raw_or_label = "both",
                        ...) {
  key <- get_api_key(
    key.name = paste0(project.name, "_REDCAP_API"),
    prompt = "Provide REDCap API key:"
  )

  out <- read_redcap_tables(
    uri = uri,
    token = key,
    raw_or_label = raw_or_label,
    ...
  )

  if (widen.data) {
    out <- out |>
      redcap_wider() |>
      suffix2label()
  }

  out
}
