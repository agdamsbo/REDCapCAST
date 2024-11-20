#' Convert labelled vectors to factors while preserving attributes
#'
#' This extends [forcats::as_factor()] as well as [haven::as_factor()], by appending
#' original attributes except for "class" after converting to factor to avoid
#' ta loss in case of rich formatted and labelled data.
#'
#' Please refer to parent functions for extended documentation.
#'
#' @param x Object to coerce to a factor.
#' @param ... Other arguments passed down to method.
#' @export
#' @examples
#' # will preserve all attributes but class
#' c(1, 4, 3, "A", 7, 8, 1) |> as_factor()
#' structure(c(1, 2, 3, 2, 10, 9),
#'   labels = c(Unknown = 9, Refused = 10)
#' ) |>
#'   as_factor()
#'
#' structure(c(1, 2, 3, 2, 10, 9),
#'   labels = c(Unknown = 9, Refused = 10),
#'   class = "haven_labelled"
#' ) |>
#'   as_factor()
#'
#' @importFrom forcats as_factor
#' @importFrom rlang check_dots_used
#' @export
#' @name as_factor
as_factor <- function(x, ...) {
  rlang::check_dots_used()
  UseMethod("as_factor")
}

#' @rdname as_factor
#' @export
as_factor.logical <- function(x, ...) {
  labels <- get_attr(x)
  x <- forcats::as_factor(x, ...)
  set_attr(x, labels[-match("class", names(labels))])
}

#' @rdname as_factor
#' @export
as_factor.numeric <- function(x, ...) {
  labels <- get_attr(x)
  x <- forcats::as_factor(x, ...)
  set_attr(x, labels[-match("class", names(labels))])
}

#' @rdname as_factor
#' @export
as_factor.character <- function(x, ...) {
  labels <- get_attr(x)
  x <- forcats::as_factor(x, ...)
  set_attr(x, labels[-match("class", names(labels))])
}

#' @rdname as_factor
#' @export
as_factor.haven_labelled <- function(x, ...) {
  labels <- get_attr(x)
  x <- haven::as_factor(x, ...)
  set_attr(x, labels[-match("class", names(labels))])
}

#' @export
#' @rdname as_factor
as_factor.labelled <- as_factor.haven_labelled



#' Get named vector of factor levels and values
#'
#' @param data factor
#' @param label character string of attribute with named vector of factor labels
#'
#' @return named vector
#' @export
#'
#' @examples
#' structure(c(1, 2, 3, 2, 10, 9),
#'   labels = c(Unknown = 9, Refused = 10),
#'   class = "haven_labelled"
#' ) |> as_factor() |> named_levels()
named_levels <- function(data, label = "labels",na.label=NULL, na.value=99) {
  stopifnot(is.factor(data))
  if (!is.null(na.label)){
    attrs <- attributes(data)
    lvls <- as.character(data)
    lvls[is.na(lvls)] <- na.label
    vals <- as.numeric(data)
    vals[is.na(vals)] <- na.value

    lbls <- data.frame(
      name = lvls,
      value = vals
    ) |> unique() |>
      (\(d){
        stats::setNames(d$value, d$name)
      })() |>
      sort()

    data <- do.call(structure,
            c(list(.Data=match(vals,lbls)),
              attrs[-match("levels", names(attrs))],
              list(levels=names(lbls),
                   labels=lbls)))
  }

  d <- data.frame(
    name = levels(data)[data],
    value = as.numeric(data)
  ) |>
    unique()

  ## Applying labels
  attr_l <- attr(x = data, which = label, exact = TRUE)
  if (length(attr_l) != 0) {
    d$value[match(names(attr_l), d$name)] <- unname(attr_l)
  }

  out <- stats::setNames(d$value, d$name)
  ## Sort if levels are numeric
  ## Else, they appear in order of appearance
  if (identical(
    levels(data),
    suppressWarnings(as.character(as.numeric(levels(data))))
  )) {
    out <- out |> sort()
  }
  out
}


#' Allows conversion of factor to numeric values preserving original levels
#'
#' @param data vector
#'
#' @return numeric vector
#' @export
#'
#' @examples
#' c(1, 4, 3, "A", 7, 8, 1) |>
#' as_factor() |> fct2num()
#'
#' structure(c(1, 2, 3, 2, 10, 9),
#'   labels = c(Unknown = 9, Refused = 10),
#'   class = "haven_labelled"
#' ) |>
#'   as_factor() |>
#'   fct2num()
#'
#' structure(c(1, 2, 3, 2, 10, 9),
#'   labels = c(Unknown = 9, Refused = 10)
#' ) |>
#'   as_factor() |>
#'   fct2num()
fct2num <- function(data) {
  stopifnot(is.factor(data))
  as.numeric(named_levels(data))[match(data, names(named_levels(data)))]
}

#' Extract attribute. Returns NA if none
#'
#' @param data vector
#' @param attr attribute name
#'
#' @return character vector
#' @export
#'
#' @examples
#' attr(mtcars$mpg, "label") <- "testing"
#' sapply(mtcars, get_attr)
#' lapply(mtcars, \(.x)get_attr(.x, NULL))
#' mtcars |>
#'   numchar2fct(numeric.threshold = 6) |>
#'   ds2dd_detailed()
get_attr <- function(data, attr = NULL) {
  if (is.null(attr)) {
    attributes(data)
  } else {
    a <- attr(data, attr, exact = TRUE)
    if (is.null(a)) {
      NA
    } else {
      a
    }
  }
}


#' Set attributes for named attribute. Appends if attr is NULL
#'
#' @param data vector
#' @param label label
#' @param attr attribute name
#'
#' @return vector with attribute
#' @export
#'
set_attr <- function(data, label, attr = NULL) {
  if (is.null(attr)) {
    ## Has to be list...
    stopifnot(is.list(label))
    ## ... with names
    stopifnot(length(label)==length(names(label)))
    attributes(data) <- c(attributes(data),label)
  } else {
    attr(data, attr) <- label
  }
  data
}

#' Finish incomplete haven attributes substituting missings with values
#'
#' @param data haven labelled variable
#'
#' @return named vector
#' @export
#'
#' @examples
#' ds <- structure(c(1, 2, 3, 2, 10, 9),
#'   labels = c(Unknown = 9, Refused = 10),
#'   class = "haven_labelled"
#' )
#' haven::is.labelled(ds)
#' attributes(ds)
#' ds |> haven_all_levels()
haven_all_levels <- function(data) {
  stopifnot(haven::is.labelled(data))
  if (length(attributes(data)$labels) == length(unique(data))) {
    out <- attributes(data)$labels
  } else {
    att <- attributes(data)$labels
    out <- c(unique(data[!data %in% att]), att) |>
      stats::setNames(c(unique(data[!data %in% att]), names(att)))
  }
  out
}

# readr::read_rds("/Users/au301842/PAaSO/labelled_test.rds") |> ds2dd_detailed()
#' sample(c(TRUE,FALSE,NA),20,TRUE) |> set_attr("hidden","status") |> trial_fct() |> named_levels(na.label = "Missing") |> sort()
# trial_fct <- function(x){
#   labels <- get_attr(x)
#   x <- factor(x, levels = c("FALSE", "TRUE"))
#   set_attr(x, labels[-match("class", names(labels))])
# }

