utils::globalVariables(c(
  "stats::setNames",
  "field_name",
  "field_type",
  "select_choices_or_calculations",
  "field_label"
))
#' Try at determining which are true time only variables
#'
#' @description
#' This is just a try at guessing data type based on data class and column names
#' hoping for a tiny bit of naming consistency. R does not include a time-only
#' data format natively, so the "hms" class from `readr` is used. This
#' has to be converted to character class before REDCap upload.
#'
#' @param data data set
#' @param validate flag to output validation data. Will output list.
#' @param sel.pos Positive selection regex string
#' @param sel.neg Negative selection regex string
#'
#' @return character vector or list depending on `validate` flag.
#' @export
#'
#' @examples
#' data <- redcapcast_data
#' data |> guess_time_only_filter()
#' data |>
#'   guess_time_only_filter(validate = TRUE) |>
#'   lapply(head)
guess_time_only_filter <- function(data,
                                   validate = FALSE,
                                   sel.pos = "[Tt]i[d(me)]",
                                   sel.neg = "[Dd]at[eo]") {
  datetime_nms <- data |>
    lapply(\(x) any(c("POSIXct", "hms") %in% class(x))) |>
    (\(x) names(data)[do.call(c, x)])()

  time_only_log <- datetime_nms |> (\(x) {
    ## Detects which are determined true Time only variables
    ## Inspection is necessary
    grepl(pattern = sel.pos, x = x) &
      !grepl(pattern = sel.neg, x = x)
  })()

  if (validate) {
    list(
      "is.POSIX" = data[datetime_nms],
      "is.datetime" = data[datetime_nms[!time_only_log]],
      "is.time_only" = data[datetime_nms[time_only_log]]
    )
  } else {
    datetime_nms[time_only_log]
  }
}

#' Correction based on time_only_filter function
#'
#'
#' @param data data set
#' @param ... arguments passed on to `guess_time_only_filter()`
#'
#' @return tibble
#' @importFrom readr parse_time
#'
#' @examples
#' data <- redcapcast_data
#' ## data |> time_only_correction()
time_only_correction <- function(data, ...) {
  nms <- guess_time_only_filter(data, ...)
  z <- nms |>
    lapply(\(y) {
      readr::parse_time(format(data[[y]], format = "%H:%M:%S"))
    }) |>
    suppressMessages(dplyr::bind_cols()) |>
    stats::setNames(nm = nms)
  data[nms] <- z
  data
}

#' Change "hms" to "character" for REDCap upload.
#'
#' @param data data set
#'
#' @return data.frame or tibble
#'
#' @examples
#' data <- redcapcast_data
#' ## data |> time_only_correction() |> hms2character()
hms2character <- function(data) {
  data |>
    lapply(function(x) {
      if ("hms" %in% class(x)) {
        as.character(x)
      } else {
        x
      }
    }) |>
    dplyr::bind_cols()
}

#' Extract data from stata file for data dictionary
#'
#' @details
#' This function is a natural development of the ds2dd() function. It assumes
#' that the first column is the ID-column. No checks.
#' Please, do always inspect the data dictionary before upload.
#'
#' Ensure, that the data set is formatted with as much information as possible.
#'
#' `field.type` can be supplied
#'
#' @param data data frame
#' @param date.format date format, character string. ymd/dmy/mdy. dafault is
#' dmy.
#' @param add.auto.id flag to add id column
#' @param form.name manually specify form name(s). Vector of length 1 or
#' ncol(data). Default is NULL and "data" is used.
#' @param form.sep If supplied dataset has form names as suffix or prefix to the
#' column/variable names, the seperator can be specified. If supplied, the
#' form.name is ignored. Default is NULL.
#' @param form.prefix Flag to set if form is prefix (TRUE) or suffix (FALSE) to
#' the column names. Assumes all columns have pre- or suffix if specified.
#' @param field.type manually specify field type(s). Vector of length 1 or
#' ncol(data). Default is NULL and "text" is used for everything but factors,
#' which wil get "radio".
#' @param field.label manually specify field label(s). Vector of length 1 or
#' ncol(data). Default is NULL and colnames(data) is used or attribute
#' `field.label.attr` for haven_labelled data set (imported .dta file with
#' `haven::read_dta()`).
#' @param field.label.attr attribute name for named labels for haven_labelled
#' data set (imported .dta file with `haven::read_dta()`. Default is "label"
#' @param field.validation manually specify field validation(s). Vector of
#' length 1 or ncol(data). Default is NULL and `levels()` are used for factors
#' or attribute `factor.labels.attr` for haven_labelled data set (imported .dta
#' file with `haven::read_dta()`).
#' @param metadata redcap metadata headings. Default is
#' REDCapCAST:::metadata_names.
#' @param convert.logicals convert logicals to factor. Default is TRUE.
#'
#' @return list of length 2
#' @export
#'
#' @examples
#' ## Basic parsing with default options
#' REDCapCAST::redcapcast_data |>
#'   dplyr::select(-dplyr::starts_with("redcap_")) |>
#'   ds2dd_detailed()
#'
#' ## Adding a record_id field
#' iris |> ds2dd_detailed(add.auto.id = TRUE)
#'
#' ## Passing form name information to function
#' iris |>
#'   ds2dd_detailed(
#'     add.auto.id = TRUE,
#'     form.name = sample(c("b", "c"), size = 6, replace = TRUE, prob = rep(.5, 2))
#'   ) |>
#'   purrr::pluck("meta")
#' mtcars |> ds2dd_detailed(add.auto.id = TRUE)
#'
#' ## Using column name suffix to carry form name
#' data <- iris |>
#'   ds2dd_detailed(add.auto.id = TRUE) |>
#'   purrr::pluck("data")
#' names(data) <- glue::glue("{sample(x = c('a','b'),size = length(names(data)),
#' replace=TRUE,prob = rep(x=.5,2))}__{names(data)}")
#' data |> ds2dd_detailed(form.sep = "__")
ds2dd_detailed <- function(data,
                           add.auto.id = FALSE,
                           date.format = "dmy",
                           form.name = NULL,
                           form.sep = NULL,
                           form.prefix = TRUE,
                           field.type = NULL,
                           field.label = NULL,
                           field.label.attr = "label",
                           field.validation = NULL,
                           metadata = names(REDCapCAST::redcapcast_meta),
                           convert.logicals = TRUE) {
  # Repair empty columns
  # These where sometimes classed as factors or
  # if (any(sapply(data,all_na))){
  #   data <- data |>
  #     ## Converts logical to factor, which overwrites attributes
  #     dplyr::mutate(dplyr::across(dplyr::where(all_na), as.character))
  # }

  if (convert.logicals) {
    data <- data |>
      ## Converts logical to factor, which overwrites attributes
      dplyr::mutate(dplyr::across(dplyr::where(is.logical), as_factor))
  }

  ## Handles the odd case of no id column present
  if (add.auto.id) {
    data <- dplyr::tibble(
      record_id = seq_len(nrow(data)),
      data
    )
  }

  ## ---------------------------------------
  ## Building the data dictionary
  ## ---------------------------------------

  ## skeleton

  dd <- data.frame(matrix(ncol = length(metadata), nrow = ncol(data))) |>
    stats::setNames(metadata) |>
    dplyr::tibble()

  ## form_name and field_name

  if (!is.null(form.sep)) {
    if (form.sep != "") {
      parts <- strsplit(names(data), split = form.sep)

      ## form.sep should be unique, but handles re-occuring pattern (by only considering first or last) and form.prefix defines if form is prefix or suffix
      ## The other split part is used as field names
      if (form.prefix) {
        dd$form_name <- clean_redcap_name(Reduce(c, lapply(parts, \(.x) .x[[1]])))
        dd$field_name <- Reduce(c, lapply(parts, \(.x) paste(.x[seq_len(length(.x))[-1]], collapse = form.sep)))
      } else {
        dd$form_name <- clean_redcap_name(Reduce(c, lapply(parts, \(.x) .x[[length(.x)]])))
        dd$field_name <- Reduce(c, lapply(parts, \(.x) paste(.x[seq_len(length(.x) - 1)], collapse = form.sep)))
      }
      ## To preserve original
      colnames(data) <- dd$field_name
      dd$field_name <- tolower(dd$field_name)
    } else {
      dd$form_name <- "data"
      dd$field_name <- gsub(" ", "_", tolower(colnames(data)))
    }
  } else {
    ## if no form name prefix, the colnames are used as field_names
    dd$field_name <- gsub(" ", "_", tolower(colnames(data)))

    if (is.null(form.name)) {
      dd$form_name <- "data"
    } else {
      if (length(form.name) == 1 || length(form.name) == nrow(dd)) {
        dd$form_name <- form.name
      } else {
        stop("Length of supplied 'form.name' has to be one (1) or ncol(data).")
      }
    }
  }

  ## field_label

  if (is.null(field.label)) {
    dd$field_label <- data |>
      sapply(function(x) {
        get_attr(x, attr = field.label.attr) |>
          compact_vec()
      })

    dd <-
      dd |>
      dplyr::mutate(
        field_label = dplyr::if_else(is.na(field_label),
          colnames(data),
          field_label
        )
      )
  } else {
    ## It really should be unique for each: same length as number of variables
    if (length(field.label) == 1 || length(field.label) == nrow(dd)) {
      dd$field_label <- field.label
    } else {
      stop("Length of supplied 'field.label' has to be one (1) or ncol(data).")
    }
  }

  data_classes <- do.call(c, lapply(data, \(.x)class(.x)[1]))

  ## field_type

  if (is.null(field.type)) {
    dd$field_type <- "text"

    dd <-
      dd |> dplyr::mutate(field_type = dplyr::if_else(data_classes == "factor",
        "radio", field_type
      ))
  } else {
    if (length(field.type) == 1 || length(field.type) == nrow(dd)) {
      dd$field_type <- field.type
    } else {
      stop("Length of supplied 'field.type' has to be one (1) or ncol(data).")
    }
  }

  ## validation
  if (is.null(field.validation)) {
    dd <-
      dd |> dplyr::mutate(
        text_validation_type_or_show_slider_number = dplyr::case_when(
          data_classes == "Date" ~ paste0("date_", date.format),
          data_classes ==
            "hms" ~ "time_hh_mm_ss",
          ## Self invented format after filtering
          data_classes ==
            "POSIXct" ~ paste0("datetime_", date.format),
          data_classes ==
            "numeric" ~ "number"
        )
      )
  } else {
    if (length(field.validation) == 1 || length(field.validation) == nrow(dd)) {
      dd$text_validation_type_or_show_slider_number <- field.validation
    } else {
      stop("Length of supplied 'field.validation'
           has to be one (1) or ncol(data).")
    }
  }

  ## choices

  factor_levels <- data |>
    sapply(function(x) {
      if (is.factor(x)) {
        ## Custom function to ensure factor order and keep original values
        ## Avoiding refactoring to keep as much information as possible
        sort(named_levels(x)) |>
          vec2choice()
      } else {
        NA
      }
    })

  dd <-
    dd |> dplyr::mutate(
      select_choices_or_calculations = dplyr::if_else(
        is.na(factor_levels),
        select_choices_or_calculations,
        factor_levels
      )
    )

  out <- list(
    data = data |>
      hms2character() |>
      stats::setNames(dd$field_name),
    meta = dd
  )

  class(out) <- c("REDCapCAST", class(out))
  out
}

#' Check if vector is all NA
#'
#' @param data vector of data.frame
#'
#' @return logical
#' @export
#'
#' @examples
#' rep(NA,4) |> all_na()
all_na <- function(data){
  all(is.na(data))
}

#' Guess time variables based on naming pattern
#'
#' @description
#' This is for repairing data with time variables with appended "1970-01-01"
#'
#'
#' @param data data.frame or tibble
#' @param validate.time Flag to validate guessed time columns
#' @param time.var.sel.pos Positive selection regex string passed to
#' `gues_time_only_filter()` as sel.pos.
#' @param time.var.sel.neg Negative selection regex string passed to
#' `gues_time_only_filter()` as sel.neg.
#'
#' @return data.frame or tibble
#' @export
#'
#' @examples
#' redcapcast_data |> guess_time_only(validate.time = TRUE)
guess_time_only <- function(data,
                            validate.time = FALSE,
                            time.var.sel.pos = "[Tt]i[d(me)]",
                            time.var.sel.neg = "[Dd]at[eo]") {
  if (validate.time) {
    return(data |> guess_time_only_filter(validate = TRUE))
  }

  ### Only keeps the first class, as time fields (POSIXct/POSIXt) has two
  ### classes
  data |> time_only_correction(
    sel.pos = time.var.sel.pos,
    sel.neg = time.var.sel.neg
  )
}




### Completion
#' Completion marking based on completed upload
#'
#' @param upload output list from `REDCapR::redcap_write()`
#' @param ls output list from `ds2dd_detailed()`
#'
#' @return list with `REDCapR::redcap_write()` results
mark_complete <- function(upload, ls) {
  data <- ls$data
  meta <- ls$meta
  forms <- unique(meta$form_name)
  cbind(
    data[[1]][data[[1]] %in% upload$affected_ids],
    data.frame(matrix(2,
      ncol = length(forms),
      nrow = upload$records_affected_count
    ))
  ) |>
    stats::setNames(c(names(data)[1], paste0(forms, "_complete")))
}


#' Helper to auto-parse un-formatted data with haven and readr
#'
#' @param data data.frame or tibble
#' @param guess_type logical to guess type with readr
#' @param col_types specify col_types using readr semantics. Ignored if guess_type is TRUE
#' @param locale option to specify locale. Defaults to readr::default_locale().
#' @param ignore.vars specify column names of columns to ignore when parsing
#' @param ... ignored
#'
#' @return data.frame or tibble
#' @export
#'
#' @examples
#' mtcars |>
#'   parse_data() |>
#'   str()
parse_data <- function(data,
                       guess_type = TRUE,
                       col_types = NULL,
                       locale = readr::default_locale(),
                       ignore.vars = "cpr",
                       ...) {
  if (any(ignore.vars %in% names(data))) {
    ignored <- data[ignore.vars]
  } else {
    ignored <- NULL
  }

  ## Parses haven data by applying labels as factors in case of any
  if (do.call(c, lapply(data, (\(x)inherits(x, "haven_labelled")))) |> any()) {
    data <- data |>
      as_factor()
  }

  ## Applying readr cols
  if (is.null(col_types) && guess_type) {
    if (do.call(c, lapply(data, is.character)) |> any()) {
      data <- data |> readr::type_convert(
        locale = locale,
        col_types = readr::cols(.default = readr::col_guess())
      )
    }
  } else {
    data <- data |> readr::type_convert(
      locale = locale,
      col_types = readr::cols(col_types)
    )
  }

  if (!is.null(ignored)) {
    data[ignore.vars] <- ignored
  }

  data
}

#' Convert vector to factor based on threshold of number of unique levels
#'
#' @description
#' This is a wrapper of forcats::as_factor, which sorts numeric vectors before
#' factoring, but levels character vectors in order of appearance.
#'
#'
#' @param data vector or data.frame column
#' @param unique.n threshold to convert class to factor
#'
#' @return vector
#' @export
#' @importFrom forcats as_factor
#'
#' @examples
#' sample(seq_len(4), 20, TRUE) |>
#'   var2fct(6) |>
#'   summary()
#' sample(letters, 20) |>
#'   var2fct(6) |>
#'   summary()
#' sample(letters[1:4], 20, TRUE) |> var2fct(6)
var2fct <- function(data, unique.n) {
  if (length(unique(data)) <= unique.n) {
    as_factor(data)
  } else {
    data
  }
}

#' Applying var2fct across data set
#'
#' @description
#' Individual thresholds for character and numeric columns
#'
#' @param data dataset. data.frame or tibble
#' @param numeric.threshold threshold for var2fct for numeric columns. Default
#' is 6.
#' @param character.throshold threshold for var2fct for character columns.
#' Default is 6.
#'
#' @return data.frame or tibble
#' @export
#'
#' @examples
#' mtcars |> str()
#' \dontrun{
#' mtcars |>
#'   numchar2fct(numeric.threshold = 6) |>
#'   str()
#' }
numchar2fct <- function(data, numeric.threshold = 6, character.throshold = 6) {
  data |>
    dplyr::mutate(
      dplyr::across(
        dplyr::where(is.numeric),
        \(.x){
          var2fct(data = .x, unique.n = numeric.threshold)
        }
      ),
      dplyr::across(
        dplyr::where(is.character),
        \(.x){
          var2fct(data = .x, unique.n = character.throshold)
        }
      )
    )
}


#' Named vector to REDCap choices (`wrapping compact_vec()`)
#'
#' @param data named vector
#'
#' @return character string
#' @export
#'
#' @examples
#' sample(seq_len(4), 20, TRUE) |>
#'   as_factor() |>
#'   named_levels() |>
#'   sort() |>
#'   vec2choice()
vec2choice <- function(data) {
  compact_vec(data,nm.sep = ", ",val.sep = " | ")
}

#' Compacting a vector of any length with or without names
#'
#' @param data vector, optionally named
#' @param nm.sep string separating name from value if any
#' @param val.sep string separating values
#'
#' @return character string
#' @export
#'
#' @examples
#' sample(seq_len(4), 20, TRUE) |>
#'   as_factor() |>
#'   named_levels() |>
#'   sort() |>
#'   compact_vec()
#' 1:6 |> compact_vec()
#' "test" |> compact_vec()
#' sample(letters[1:9], 20, TRUE) |> compact_vec()
compact_vec <- function(data,nm.sep=": ",val.sep="; ") {
  # browser()
  if (all(is.na(data))) {
    return(data)
  }

  if (length(names(data)) > 0) {
    paste(
      paste(data,
        names(data),
        sep = nm.sep
      ),
      collapse = val.sep
    )
  } else {
    paste(
      data,
      collapse = val.sep
    )
  }
}
