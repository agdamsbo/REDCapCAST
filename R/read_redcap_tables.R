#' Download REDCap data
#'
#' @description
#' Implementation of passed on to \link[REDCapCAST]{REDCap_split} with a focused
#' data acquisition approach using passed on to \link[REDCapR]{redcap_read} and
#' only downloading specified fields, forms and/or events using the built-in
#' focused_metadata including some clean-up.
#' Works with classical and longitudinal projects with or without repeating
#' instruments.
#' Will preserve metadata in the data.frames as labels.
#'
#' @param uri REDCap database API uri
#' @param token API token
#' @param records records to download
#' @param fields fields to download
#' @param events events to download
#' @param forms forms to download
#' @param raw_or_label raw or label tags. Can be "raw", "label" or "both".
#'
#'   * "raw": Standard \link[REDCapR]{redcap_read} method to get raw values.
#'   * "label": Standard \link[REDCapR]{redcap_read} method to get label values.
#'   * "both": Get raw values with REDCap labels applied as labels. Use
#'   \link[REDCapCAST]{as_factor} to format factors with original labels and use
#'   the  `gtsummary` package functions like \link[gtsummary]{tbl_summary} to
#'   easily get beautiful tables with original labels from REDCap. Use
#'   \link[REDCapCAST]{fct_drop} to drop empty levels.
#'
#' @param split_forms Whether to split "repeating" or "all" forms, default is
#' all.
#' @param ... passed on to \link[REDCapR]{redcap_read}
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
                               raw_or_label = c("raw","label","both"),
                               split_forms = "all",
                               ...) {

  raw_or_label <- match.arg(raw_or_label, c("raw","label","both"))

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

  if (raw_or_label=="both"){
    rorl <- "raw"
  } else {
    rorl <- raw_or_label
  }

  # Getting dataset
  d <- REDCapR::redcap_read(
    redcap_uri = uri,
    token = token,
    fields = fields,
    events = events,
    forms = forms,
    records = records,
    raw_or_label = rorl,
    ...
  )[["data"]]

  if (raw_or_label=="both"){
    d <- apply_field_label(data=d,meta=m)

    d <- apply_factor_labels(data=d,meta=m)
  }


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


#' Converts REDCap choices to factor levels and stores in labels attribute
#'
#' @description
#' Applying \link[REDCapCAST]{as_factor} to the data.frame or variable, will
#' coerce to a factor.
#'
#' @param data vector
#' @param meta vector of REDCap choices
#'
#' @return vector of class "labelled" with a "labels" attribute
#' @export
#'
#' @examples
#' format_redcap_factor(sample(1:3,20,TRUE),"1, First. | 2, second | 3, THIRD")
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
    set_attr(data, label = "labelled", attr = "class")
}



#' Apply REDCap filed labels to data frame
#'
#' @param data REDCap exported data set
#' @param meta REDCap data dictionary
#'
#' @return data.frame
#' @export
#'
apply_field_label <- function(data,meta){
  purrr::imap(data, \(.x, .i){
    if (.i %in% meta$field_name) {
      # Does not handle checkboxes
      out <- set_attr(.x,
                      label = clean_field_label(meta$field_label[meta$field_name == .i]),
                      attr = "label"
      )
      out
    } else {
      .x
    }
  }) |> dplyr::bind_cols()
}

#' Preserve all factor levels from REDCap data dictionary in data export
#'
#' @param data REDCap exported data set
#' @param meta REDCap data dictionary
#'
#' @return data.frame
#' @export
#'
apply_factor_labels <- function(data,meta=NULL){
  if (is.list(data) && !is.data.frame(data)){
    meta <- data$meta
    data <- data$data
  } else if (is.null(meta)) {
    stop("Please provide a data frame for meta")
  }
  purrr::imap(data, \(.x, .i){
    if (any(c("radio", "dropdown") %in% meta$field_type[meta$field_name == .i]) || is.factor(.x)) {
      format_redcap_factor(.x, meta$select_choices_or_calculations[meta$field_name == .i])
    } else {
      .x
    }
  }) |> dplyr::bind_cols()
}


