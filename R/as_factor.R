#' Convert labelled vectors to factors while preserving attributes
#'
#' This extends \link[forcats]{as_factor} as well as \link[haven]{as_factor}, by appending
#' original attributes except for "class" after converting to factor to avoid
#' ta loss in case of rich formatted and labelled data.
#'
#' Please refer to parent functions for extended documentation.
#' To avoid redundancy calls and errors, functions are copy-pasted here
#'
#' @param x Object to coerce to a factor.
#' @param ... Other arguments passed down to method.
#' @param only_labelled Only apply to labelled columns?
#' @export
#' @examples
#' # will preserve all attributes
#' c(1, 4, 3, "A", 7, 8, 1) |> as_factor()
#' structure(c(1, 2, 3, 2, 10, 9),
#'   labels = c(Unknown = 9, Refused = 10)
#' ) |>
#'   as_factor() |>
#'   dput()
#'
#' structure(c(1, 2, 3, 2, 10, 9),
#'   labels = c(Unknown = 9, Refused = 10),
#'   class = "haven_labelled"
#' ) |>
#'   as_factor()
#' @importFrom forcats as_factor
#' @export
#' @name as_factor
as_factor <- function(x, ...) {
  UseMethod("as_factor")
}

#' @rdname as_factor
#' @export
as_factor.factor <- function(x, ...) {
  x
}

#' @rdname as_factor
#' @export
as_factor.logical <- function(x, ...) {
  labels <- get_attr(x)
  x <- factor(x, levels = c("FALSE", "TRUE"))
  set_attr(x, labels, overwrite = FALSE)
}



#' @rdname as_factor
#' @export
as_factor.numeric <- function(x, ...) {
  labels <- get_attr(x)
  x <- factor(x)
  set_attr(x, labels, overwrite = FALSE)
}

#' @rdname as_factor
#' @export
as_factor.character <- function(x, ...) {
  labels <- get_attr(x)
  if (possibly_roman(x)) {
    x <- factor(x)
  } else {
    x <- structure(
      forcats::fct_inorder(x),
      label = attr(x, "label", exact = TRUE)
    )
  }
  set_attr(x, labels, overwrite = FALSE)
}

#' @param ordered If `TRUE` create an ordered (ordinal) factor, if
#'   `FALSE` (the default) create a regular (nominal) factor.
#' @param levels How to create the levels of the generated factor:
#'
#'   * "default": uses labels where available, otherwise the values.
#'     Labels are sorted by value.
#'   * "both": like "default", but pastes together the level and value
#'   * "label": use only the labels; unlabelled values become `NA`
#'   * "values": use only the values
#' @rdname as_factor
#' @export
as_factor.haven_labelled <- function(x, levels = c("default", "labels", "values", "both"),
                                     ordered = FALSE, ...) {
  labels_all <- get_attr(x)

  levels <- match.arg(levels)
  label <- attr(x, "label", exact = TRUE)
  labels <- attr(x, "labels")

  if (levels %in% c("default", "both")) {
    if (levels == "both") {
      names(labels) <- paste0("[", labels, "] ", names(labels))
    }

    # Replace each value with its label
    vals <- unique(vctrs::vec_data(x))
    levs <- replace_with(vals, unname(labels), names(labels))
    # Ensure all labels are preserved
    levs <- sort(c(stats::setNames(vals, levs), labels), na.last = TRUE)
    levs <- unique(names(levs))

    x <- replace_with(vctrs::vec_data(x), unname(labels), names(labels))

    x <- factor(x, levels = levs, ordered = ordered)
  } else if (levels == "labels") {
    levs <- unname(labels)
    labs <- names(labels)
    x <- replace_with(vctrs::vec_data(x), levs, labs)
    x <- factor(x, unique(labs), ordered = ordered)
  } else if (levels == "values") {
    if (all(x %in% labels)) {
      levels <- unname(labels)
    } else {
      levels <- sort(unique(vctrs::vec_data(x)))
    }
    x <- factor(vctrs::vec_data(x), levels, ordered = ordered)
  }

  x <- structure(x, label = label)

  set_attr(x, labels_all, overwrite = FALSE)
}

#' @export
#' @rdname as_factor
as_factor.labelled <- as_factor.haven_labelled

#' @rdname as_factor
#' @export
as_factor.data.frame <- function(x, ..., only_labelled = TRUE) {
  if (only_labelled) {
    labelled <- vapply(x, is.labelled, logical(1))
    x[labelled] <- lapply(x[labelled], as_factor, ...)
  } else {
    x[] <- lapply(x, as_factor, ...)
  }

  x
}

#' Tests for multiple label classes
#'
#' @param x data
#' @param classes classes to test
#'
#' @return logical
#' @export
#'
#' @examples
#' structure(c(1, 2, 3, 2, 10, 9),
#'   labels = c(Unknown = 9, Refused = 10),
#'   class = "haven_labelled"
#' ) |> is.labelled()
is.labelled <- function(x, classes = c("haven_labelled", "labelled")) {
  classes |>
    sapply(\(.class){
      inherits(x, .class)
    }) |>
    any()
}

replace_with <- function(x, from, to) {
  stopifnot(length(from) == length(to))

  out <- x
  # First replace regular values
  matches <- match(x, from, incomparables = NA)
  if (anyNA(matches)) {
    out[!is.na(matches)] <- to[matches[!is.na(matches)]]
  } else {
    out <- to[matches]
  }

  # Then tagged missing values
  tagged <- haven::is_tagged_na(x)
  if (!any(tagged)) {
    return(out)
  }

  matches <- match(haven::na_tag(x), haven::na_tag(from), incomparables = NA)

  # Could possibly be faster to use anyNA(matches)
  out[!is.na(matches)] <- to[matches[!is.na(matches)]]
  out
}


#' Get named vector of factor levels and values
#'
#' @param data factor
#' @param label character string of attribute with named vector of factor labels
#' @param na.label character string to refactor NA values. Default is NULL.
#' @param na.value new value for NA strings. Ignored if na.label is NULL.
#' Default is 99.
#' @param sort.numeric sort factor levels if levels are numeric. Default is TRUE
#'
#' @return named vector
#' @export
#'
#' @examples
#' structure(c(1, 2, 3, 2, 10, 9),
#'   labels = c(Unknown = 9, Refused = 10),
#'   class = "haven_labelled"
#' ) |>
#'   as_factor() |>
#'   named_levels()
#' structure(c(1, 2, 3, 2, 10, 9),
#'   labels = c(Unknown = 9, Refused = 10),
#'   class = "labelled"
#' ) |>
#'   as_factor() |>
#'   named_levels()
named_levels <- function(data, label = "labels", na.label = NULL, na.value = 99, sort.numeric=TRUE) {
  stopifnot(is.factor(data))
  if (!is.null(na.label)) {
    attrs <- attributes(data)
    lvls <- as.character(data)
    lvls[is.na(lvls)] <- na.label
    vals <- as.numeric(data)
    vals[is.na(vals)] <- na.value

    lbls <- data.frame(
      name = lvls,
      value = vals
    ) |>
      unique() |>
      (\(d){
        stats::setNames(d$value, d$name)
      })() |>
      sort()

    data <- do.call(
      structure,
      c(
        list(.Data = match(vals, lbls)),
        attrs[-match("levels", names(attrs))],
        list(
          levels = names(lbls),
          labels = lbls
        )
      )
    )
  }

  # Handle empty factors
  if (all_na(data)) {
    d <- data.frame(
      name = levels(data),
      value = seq_along(levels(data))
    )
  } else {
    d <- data.frame(
      name = levels(data)[data],
      value = as.numeric(data)
    ) |>
      unique() |>
      stats::na.omit()
  }

  ## Applying labels
  attr_l <- attr(x = data, which = label, exact = TRUE)
  if (length(attr_l) != 0) {
    if (all(names(attr_l) %in% d$name)) {
      d$value[match(names(attr_l), d$name)] <- unname(attr_l)
    } else if (all(d$name %in% names(attr_l)) && nrow(d) < length(attr_l)) {
      d <- data.frame(
        name = names(attr_l),
        value = unname(attr_l)
      )
    } else {
      d$name[match(attr_l, d$name)] <- names(attr_l)
      d$value[match(names(attr_l), d$name)] <- unname(attr_l)
    }
  }

  out <- stats::setNames(d$value, d$name)
  ## Sort if levels are numeric
  ## Else, they appear in order of appearance
  if (possibly_numeric(levels(data)) && sort.numeric) {
    out <- out |> sort()
  }
  out
}

#' Test if vector can be interpreted as roman numerals
#'
#' @param data character vector
#'
#' @return logical
#' @export
#'
#' @examples
#' sample(1:100, 10) |>
#'   as.roman() |>
#'   possibly_roman()
#' sample(c(TRUE, FALSE), 10, TRUE) |> possibly_roman()
#' rep(NA, 10) |> possibly_roman()
possibly_roman <- function(data) {
  if (all(is.na(data))) {
    return(FALSE)
  }
  identical(as.character(data),
            as.character(suppressWarnings(utils::as.roman(data))))
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
#'   as_factor() |>
#'   fct2num()
#'
#' structure(c(1, 2, 3, 2, 10, 9),
#'   labels = c(Unknown = 9, Refused = 10),
#'   class = "haven_labelled"
#' ) |>
#'   as_factor() |>
#'   fct2num()
#'
#' structure(c(1, 2, 3, 2, 10, 9),
#'   labels = c(Unknown = 9, Refused = 10),
#'   class = "labelled"
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

  if (is.character(named_levels(data))) {
    values <- as.numeric(named_levels(data))
  } else {
    values <- named_levels(data)
  }

  out <- values[match(data, names(named_levels(data)))]

  ## If no NA on numeric coercion, of original names, then return
  ## original numeric names, else values
  if (possibly_numeric(names(out))) {
    out <- as.numeric(names(out))
  }
  unname(out)
}

#' Tests if vector can be interpreted as numeric without introducing NAs by
#' coercion
#'
#' @param data vector
#'
#' @return logical
#' @export
#'
#' @examples
#' c("1","5") |> possibly_numeric()
#' c("1","5","e") |> possibly_numeric()
possibly_numeric <- function(data) {
  suppressWarnings(
  length(stats::na.omit(as.numeric(data))) ==
    length(data)
  )
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
#' do.call(c, sapply(mtcars, get_attr))
#' \dontrun{
#' mtcars |>
#'   numchar2fct(numeric.threshold = 6) |>
#'   ds2dd_detailed()
#' }
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
#' @param overwrite overwrite existing attributes. Default is FALSE.
#'
#' @return vector with attribute
#' @export
#'
set_attr <- function(data, label, attr = NULL, overwrite = FALSE) {
  # browser()
  if (is.null(attr)) {
    ## Has to be a named list
    ## Will not fail, but just return original data
    if (!is.list(label) | length(label) != length(names(label))) {
      return(data)
    }
    ## Only include named labels
    label <- label[!is.na(names(label))]

    if (!overwrite) {
      label <- label[!names(label) %in% names(attributes(data))]
    }
    attributes(data) <- c(attributes(data), label)
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
