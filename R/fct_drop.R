#' Drop unused levels preserving label data
#'
#' This extends [forcats::fct_drop()] to natively work across a data.frame and
#' replaces [base::droplevels()].
#'
#' @param x Factor to drop unused levels
#' @param ... Other arguments passed down to method.
#' @export
#'
#' @importFrom forcats fct_drop
#' @export
#' @name fct_drop
fct_drop <- function(x, ...) {
  UseMethod("fct_drop")
}

#' @rdname fct_drop
#' @export
#'
#' @examples
#' mtcars |>
#'   numchar2fct() |>
#'   fct_drop()
fct_drop.data.frame <- function(x, ...) {
  purrr::map(x, \(.x){
    if (is.factor(.x)) {
      forcats::fct_drop(.x)
    } else {
      .x
    }
  }) |>
    dplyr::bind_cols()
}


#' @rdname fct_drop
#' @export
#'
#' @examples
#' mtcars |>
#'   numchar2fct() |>
#'   dplyr::mutate(vs = fct_drop(vs))
fct_drop.factor <- function(x, ...) {
  forcats::fct_drop(f = x, ...)
}
