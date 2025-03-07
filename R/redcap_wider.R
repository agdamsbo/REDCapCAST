utils::globalVariables(c(
  "redcap_wider",
  "event.glue",
  "inst.glue"
))

#' Transforms list of REDCap data.frames to a single wide data.frame
#'
#' @description Converts a list of REDCap data.frames from long to wide format.
#' In essence it is a wrapper for the \link[tidyr]{pivot_wider} function applied
#' on a REDCap output (from \link[REDCapCAST]{read_redcap_tables}) or manually
#' split by \link[REDCapCAST]{REDCap_split}.
#'
#' @param data A list of data frames
#' @param event.glue A \link[glue]{glue} string for repeated events naming
#' @param inst.glue A \link[glue]{glue} string for repeated instruments naming
#'
#' @return data.frame in wide format
#' @export
#'
#' @importFrom tidyr pivot_wider
#' @importFrom tidyselect all_of
#' @importFrom purrr reduce
#'
#' @examples
#' # Longitudinal
#' list1 <- list(
#'   data.frame(
#'     record_id = c(1, 2, 1, 2),
#'     redcap_event_name = c("baseline", "baseline", "followup", "followup"),
#'     age = c(25, 26, 27, 28)
#'   ),
#'   data.frame(
#'     record_id = c(1, 2),
#'     redcap_event_name = c("baseline", "baseline"),
#'     gender = c("male", "female")
#'   )
#' )
#' redcap_wider(list1)
#' # Simpel with two instruments
#' list2 <- list(
#'   data.frame(
#'     record_id = c(1, 2),
#'     age = c(25, 26)
#'   ),
#'   data.frame(
#'     record_id = c(1, 2),
#'     gender = c("male", "female")
#'   )
#' )
#' redcap_wider(list2)
#' # Simple with single instrument
#' list3 <- list(data.frame(
#'   record_id = c(1, 2),
#'   age = c(25, 26)
#' ))
#' redcap_wider(list3)
#' # Longitudinal with repeatable instruments
#' list4 <- list(
#'   data.frame(
#'     record_id = c(1, 2, 1, 2),
#'     redcap_event_name = c("baseline", "baseline", "followup", "followup"),
#'     age = c(25, 26, 27, 28)
#'   ),
#'   data.frame(
#'     record_id = c(1, 1, 1, 1, 2, 2, 2, 2),
#'     redcap_event_name = c(
#'       "baseline", "baseline", "followup", "followup",
#'       "baseline", "baseline", "followup", "followup"
#'     ),
#'     redcap_repeat_instrument = "walk",
#'     redcap_repeat_instance = c(1, 2, 1, 2, 1, 2, 1, 2),
#'     dist = c(40, 32, 25, 33, 28, 24, 23, 36)
#'   ),
#'   data.frame(
#'     record_id = c(1, 2),
#'     redcap_event_name = c("baseline", "baseline"),
#'     gender = c("male", "female")
#'   )
#' )
#' redcap_wider(list4)
#'
#' list5 <- list(
#'   data.frame(
#'     record_id = c(1, 2, 1, 2),
#'     redcap_event_name = c("baseline", "baseline", "followup", "followup")
#'   ),
#'   data.frame(
#'     record_id = c(1, 1, 1, 1, 2, 2, 2, 2),
#'     redcap_event_name = c(
#'       "baseline", "baseline", "followup", "followup",
#'       "baseline", "baseline", "followup", "followup"
#'     ),
#'     redcap_repeat_instrument = "walk",
#'     redcap_repeat_instance = c(1, 2, 1, 2, 1, 2, 1, 2),
#'     dist = c(40, 32, 25, 33, 28, 24, 23, 36)
#'   ),
#'   data.frame(
#'     record_id = c(1, 2),
#'     redcap_event_name = c("baseline", "baseline"),
#'     gender = c("male", "female")
#'   )
#' )
#' redcap_wider(list5)
redcap_wider <-
  function(data,
           event.glue = "{.value}____{redcap_event_name}",
           inst.glue = "{.value}____{redcap_repeat_instance}") {


    if (!is_repeated_longitudinal(data)) {
      if (is.list(data)) {
        if (length(data) == 1) {
          out <- data[[1]]
        } else {
          out <- data |> purrr::reduce(dplyr::left_join)
        }
      } else if (is.data.frame(data)) {
        out <- data
      }
    } else {

      ## Cleaning instrument list to only include instruments holding other data
      ## than ID and generic columns
      ## This is to mitigate an issue when not exporting fields from the first
      ## instrument.
      ## Not taking this step would throw an error when pivoting.
      instrument_names <- lapply(data, names)

      id.name <- do.call(c, instrument_names)[[1]]

      generic_names <- c(
        id.name,
        "redcap_event_name",
        "redcap_repeat_instrument",
        "redcap_repeat_instance"
      )

      semi_empty <- lapply(instrument_names,\(.x){
        all(.x %in% generic_names)
      }) |> unlist()

      data <- data[!semi_empty]

      l <- lapply(data, function(i) {
        rep_inst <- "redcap_repeat_instrument" %in% names(i)

        if (rep_inst) {
          k <- lapply(split(i, f = i[[id.name]]), function(j) {
            cname <- colnames(j)
            vals <-
              cname[!cname %in% generic_names]
            s <- tidyr::pivot_wider(
              j,
              names_from = "redcap_repeat_instance",
              values_from = all_of(vals),
              names_glue = inst.glue
            )
            s[!colnames(s) %in% c("redcap_repeat_instrument")]
          })

          # Labels are removed and restored after bind_rows as class "labelled"
          # is not supported
          i <- remove_labelled(k) |>
            dplyr::bind_rows()

          all_labels <- save_labels(data)

          i <- restore_labels(i, all_labels)
        }

        event <- "redcap_event_name" %in% names(i)

        if (event) {
          event.n <- length(unique(i[["redcap_event_name"]])) > 1

          i[["redcap_event_name"]] <-
            gsub(" ", "_", tolower(i[["redcap_event_name"]]))

          if (event.n) {
            cname <- colnames(i)
            vals <- cname[!cname %in% c(id.name, "redcap_event_name")]

            s <- tidyr::pivot_wider(
              i,
              names_from = "redcap_event_name",
              values_from = all_of(vals),
              names_glue = event.glue
            )
            s[colnames(s) != "redcap_event_name"]
          } else {
            i[colnames(i) != "redcap_event_name"]
          }
        } else {
          i
        }
      })

      # out <- Reduce(f = dplyr::full_join, x = l)
      out <- purrr::reduce(.x = l, .f = dplyr::full_join)
    }

    out
  }

# Applies list of attributes to data.frame
restore_labels <- function(data, labels) {
  stopifnot(is.list(labels))
  stopifnot(is.data.frame(data))
  for (ndx in names(labels)) {
    data <- purrr::imap(data, \(.y, .j){
      if (startsWith(.j, ndx)) {
        set_attr(.y, labels[[ndx]])
      } else {
        .y
      }
    }) |> dplyr::bind_cols()
  }
  return(data)
}

# Extract unique variable attributes from list of data.frames
save_labels <- function(data) {
  stopifnot(is.list(data))
  out <- list()
  for (j in seq_along(data)) {
    out <- c(out, lapply(data[[j]], get_attr))
  }

  out[!duplicated(names(out))]
}

# Removes class attributes of class "labelled" or "haven_labelled"
remove_labelled <- function(data) {
  stopifnot(is.list(data))
  lapply(data, \(.x) {
    lapply(.x, \(.y) {
      if (REDCapCAST::is.labelled(.y)) {
        set_attr(.y, label = NULL, attr = "class")
      } else {
        .y
      }
    }) |>
      dplyr::bind_cols()
  })
}

#' Transfer variable name suffix to label in widened data
#'
#' @param data data.frame
#' @param suffix.sep string to split suffix(es). Passed to \link[base]{strsplit}
#' @param attr label attribute. Default is "label"
#' @param glue.str glue string for new label. Available variables are "label"
#' and "suffixes"
#'
#' @return data.frame
#' @export
#'
suffix2label <- function(data,
                         suffix.sep = "____",
                         attr = "label",
                         glue.str="{label} ({paste(suffixes,collapse=', ')})") {
  data |>
    purrr::imap(\(.d, .i){
      suffixes <- unlist(strsplit(.i, suffix.sep))[-1]
      if (length(suffixes) > 0) {
        label <- get_attr(.d, attr = attr)
        set_attr(.d,
          glue::glue(glue.str),
          attr = attr
        )
      } else {
        .d
      }
    }) |>
    dplyr::bind_cols()
}
