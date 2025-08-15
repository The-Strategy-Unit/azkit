#' An alternative to stopifnot/assert_that etc
#'
#' This function makes it easy to use the `{purrr}` functions `every()`,
#' `some()` and `none()` to handle vector inputs of length >= 1, and supports
#' the seamless use of `glue` strings in the custom error message.
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
