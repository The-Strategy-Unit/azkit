#' An alternative to stopifnot/assert_that etc
#'
#' This function makes it easy to use the `{purrr}` functions `every()`,
#' `some()` and `none()` to handle vector inputs of length >= 1, and supports
#' the seamless use of `glue` strings in the custom error message.
#' Not suitable for checking if `length(x) == 1` as it will check vectors
#' element-wise, so will potentially return TRUE even if `length(x) > 1`. For
#' this case, use `check_val()` instead.
#' @param x The object to be checked
#' @param predicate The predicate function used to check elements of `x`
#' @param message A custom error message, as a string. Will be shown to the
#'  user if the predicate check does not succeed. Can include `glue` variables
#'  and `{cli}` semantic markup. Variable values will be searched for in the
#'  environment of the caller function (not in the environment of
#'  `check_vec()`). This makes it easier to include informative values in the
#'  message.
#' @param which One of "every", "some", "none", as a string. Defines which
#'  `{purrr}` function to use when applying the predicate. "every", the default,
#'  means that `check_vec()` will succeed if every value of x satisfies the
#'  predicate. "none" can be used to generate an inverse predicate, or the
#'  situation where success means that none of the elements of x satisfies the
#'  predicate. "some" is unlikely to be useful often, but it is available.
#' @param pf Necessary for variables in the caller environment to be used in
#'  the custom error message.
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
