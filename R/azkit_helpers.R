#' An alternative to stopifnot/assert_that etc
#'
#' This function makes it easy to use the `{purrr}` functions `every()`,
#' `some()` and `none()` to handle vector inputs of length >= 1, and supports
#' the seamless use of `glue` strings in the custom error message.
#' Not suitable for checking if `length(x) == 1` as it will check vectors
#' element-wise, so will potentially return TRUE even if `length(x) > 1`
#'
#' @param x The object to be checked
#' @param predicate The predicate function used to check elements of `x`
#' @param message A custom error message, as a string. Will be shown to the
#'  user if the predicate check does not succeed. Can include `glue` variables
#'  and `{cli}` semantic markup. Variable values will be searched for in the
#'  environment of the caller function (not in the environment of `check_vec()`
#'  ). This makes it easier to include informative values in the message.
#' @param which One of "every", "some", "none", as a string. Defines which
#'  `{purrr}` function to use when applying the predicate. "every", the default,
#'  means that `check_vec()` will succeed if every value of x satisfies the
#'  predicate. "none" can be used to generate an inverse predicate, or the
#'  situation where success means that none of the elements of x satisfies the
#'  predicate. "some" is unlikely to be useful often, but it is available.
#' @param pf Set as [parent.frame()] so variables in the caller environment can
#'  be used in the custom error message.
#' @seealso [check_scalar_type()]
#' @keywords internal
check_vec <- function(
  x,
  predicate,
  message,
  which = c("every", "some", "none"),
  pf = parent.frame()
) {
  w <- rlang::arg_match(which)
  test_call <- rlang::call2(w, .x = x, .p = predicate, .ns = "purrr")
  if (eval(test_call)) {
    x
  } else {
    cli::cli_abort(c(x = message), call = rlang::caller_call(), .envir = pf)
    invisible(NULL)
  }
}

#' @keywords internal
cv_error_msg <- \(text) paste0("{.fn check_vec}: ", text)


#' An alternative to stopifnot/assert_that etc
#' This function makes it easy to use the `is_scalar_*` functions from `{rlang}`
#'  to check the type of `x`, _and_ that `length(x) == 1`, and supports the
#'  seamless use of `glue` strings in the custom error message.
#' Possible values for the `type` parameter are: "character", "logical", "list",
#'  "integer", "double", "string", "bool", "bytes", "raw", "vector", "complex".
#  Supplying "string" or "bool" will additionally check that `x` is not missing.
#' @seealso [check_vec()]
#' @inheritParams check_vec
#' @param type A string defining the R object type that `x` is checked to be
#' @keywords internal
check_scalar_type <- function(
  x,
  type,
  message,
  pf = parent.frame()
) {
  opts <- c(
    "character",
    "logical",
    "integer",
    "double",
    "string",
    "bool",
    "list",
    "bytes",
    "raw",
    "vector",
    "complex"
  )
  t <- rlang::arg_match(type, opts)
  t <- if (t %in% c("string", "bool")) t else paste0("scalar_", t)
  test_call <- rlang::call2(paste0("is_", t), x = x, .ns = "rlang")
  if (eval(test_call)) {
    x
  } else {
    cli::cli_abort(c(x = message), call = rlang::caller_call(), .envir = pf)
    invisible(NULL)
  }
}

#' @keywords internal
cst_error_msg <- \(text) paste0("{.fn check_scalar_type}: ", text)


#' grepl a glued regex
#'
#' Use \{glue\} expressions in grepl (and put the arguments the right way round)
#' (https://glue.tidyverse.org/articles/wrappers.html)
#'
#' @param x A character vector to check
#' @param rx A string that after processing by [glue::glue_data()] will be used
#'  as a regex pattern in [grepl()]
#' @param ... Arguments passed onto [grepl()]
#' @param g The parent frame of gregg, which must be passed through to glue_data
#' @keywords internal
gregg <- \(x, rx, ..., g = parent.frame()) grepl(glue::glue_data(g, rx), x, ...)
