#' Drop unused levels preserving label data
#'
#' This extends [forcats::fct_drop()] to natively work across a data.frame and
#' replace [base::droplevels()].
#'
#' @param x Factor to drop unused levels
#' @param ... Other arguments passed down to method.
#' @export
#'
#' @importFrom forcats fct_drop
#' @export
#' @name fct_drop
NULL

#' @rdname fct_drop
#' @export
fct_drop.data.frame <- function(x, ...) {
  purrr::map(x, \(.x){
    if (is.factor(.x)){
      forcats::fct_drop(.x)
    } else {
      .x
    }
  }) |>
    dplyr::bind_cols()
}



