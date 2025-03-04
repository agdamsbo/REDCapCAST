#' Interpret specific binary values as logicals
#'
#' @param x vector or data.frame
#' @param values list of values to interpret as logicals. First value is
#' @param ... ignored
#' interpreted as TRUE.
#'
#' @returns vector
#' @export
#'
#' @examples
#' c(sample(c("TRUE", "FALSE"), 20, TRUE), NA) |>
#'   as_logical() |>
#'   class()
#' ds <- dplyr::tibble(
#'   B = factor(sample(c(1, 2), 20, TRUE)),
#'   A = factor(sample(c("TRUE", "FALSE"), 20, TRUE)),
#'   C = sample(c(3, 4), 20, TRUE),
#'   D = factor(sample(c("In", "Out"), 20, TRUE))
#' )
#' ds |>
#'   as_logical() |>
#'   sapply(class)
#' ds$A |> class()
#' sample(c("TRUE",NA), 20, TRUE) |>
#'   as_logical()
#' as_logical(0)
#' @name as_logical
as_logical <- function(x,
                       values = list(
                         c("TRUE", "FALSE"),
                         c("Yes", "No"),
                         c(1, 0),
                         c(1, 2)
                       ),
                       ...) {
  UseMethod("as_logical")
}

#' @rdname as_logical
#' @export
as_logical.data.frame <- function(x,
                                  values = list(
                                    c("TRUE", "FALSE"),
                                    c("Yes", "No"),
                                    c(1, 0),
                                    c(1, 2)
                                  ),
                                  ...) {
  as.data.frame(lapply(x, \(.x){
    as_logical.default(x = .x, values = values)
  }))
}

#' @rdname as_logical
#' @export
as_logical.default <- function(x,
                               values = list(
                                 c("TRUE", "FALSE"),
                                 c("Yes", "No"),
                                 c(1, 0),
                                 c(1, 2)
                               ),
                               ...) {
  label <- REDCapCAST::get_attr(x, "label")

  # browser()
  out <- c()
  if (any(
    c(
      "character",
      "factor",
      "numeric"
    ) %in% class(x)
  )){
   if (length(unique(x[!is.na(x)])) == 2) {
     if (is.factor(x)) {
       match_index <- which(sapply(values, \(.x){
         all(.x %in% levels(x))
       }))
     } else {
       match_index <- which(sapply(values, \(.x){
         all(.x %in% x)
       }))
     }
   } else if (length(unique(x[!is.na(x)])) == 1){
     if (is.factor(x)) {
       match_index <- which(sapply(values, \(.x){
         any(.x %in% levels(x))
       }))
     } else {
       match_index <- which(sapply(values, \(.x){
         any(.x %in% x)
       }))
     }
   } else {
     match_index <- c()
  }

    if (length(match_index) == 1) {
      out <- x == values[[match_index]][1]
    } else if (length(match_index) > 1) {
      # If matching several, the first match is used.
      out <- x == values[[match_index[1]]][1]
    }
  }

  if (length(out) == 0) {
    out <- x
  }

  if (!is.na(label)) {
    out <- REDCapCAST::set_attr(out, label = label, attr = "label")
  }
  out
}
