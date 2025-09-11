test_that("check_vec uses {cli} formatting and glue variables", {
  var <- "test"
  expect_identical(check_vec(letters, nzchar), letters)
  check_vec(letters, nzchar, "{.var {var}}", "none") |>
    expect_error(class = "rlang_error")
  check_vec(letters, nzchar, "{.var {var}}", "none") |>
    expect_error("`test`", class = "rlang_error")
})


test_that("I understand how rlang::abort works", {
  # Previously I was generating errors with `cli::cli_abort()` but maybe it
  # makes more sense to use `rlang::abort()` if it handles glue vars in its
  # messages OK
  var <- "test"
  message <- "{.var {var}} error"
  # will only succeed if devtools::load_all() has been run:
  # expect_error(rlang::abort(message), "`test` error")
  expect_error(rlang::abort(message), class = "rlang_error")
  rlang::local_use_cli(inline = TRUE)
  expect_error(rlang::abort(message), "`test` error", class = "rlang_error")

  check_var <- function(var, message) {
    if (var != "test") {
      rlang::abort(message, class = "azkit")
    } else {
      var
    }
  }
  expect_equal(check_var("test", "Error with {.var {var}}"), var)
  check_var("tst", "Error with {.var {var}}") |>
    expect_error("Error with `tst`", class = "rlang_error")
  check_var("tst", "Error with {.var {var}}") |>
    expect_error("Error with `tst`", class = "azkit")
})


test_that("check_scalar_type builds functions correctly", {
  typ <- "character"
  typ <- if (typ %in% c("string", "bool")) typ else paste0("scalar_", typ)
  expect_equal(typ, "scalar_character")
  x <- "a"
  test_call <- rlang::call2(paste0("is_", typ), x = x, .ns = "rlang")
  expect_identical(
    as.character(test_call),
    c("rlang::is_scalar_character", "a")
  )
  expect_true(eval(test_call))
  test_call <- rlang::call_modify(test_call, x = NA_character_)
  expect_true(eval(test_call))
  test_call <- rlang::call_modify(test_call, x = NA)
  expect_false(eval(test_call))
  test_call <- rlang::call_modify(test_call, x = 9)
  expect_false(eval(test_call))

  typ <- "string"
  typ <- if (typ %in% c("string", "bool")) typ else paste0("scalar_", typ)
  expect_equal(typ, "string")
  x <- "a"
  test_call <- rlang::call2(paste0("is_", typ), x = x, .ns = "rlang")
  expect_identical(
    as.character(test_call),
    c("rlang::is_string", "a")
  )
  expect_true(eval(test_call))
  test_call <- rlang::call_modify(test_call, x = NA_character_)
  expect_false(eval(test_call))
  test_call <- rlang::call_modify(test_call, x = 9)
  expect_false(eval(test_call))

  typ <- "integer"
  typ <- if (typ %in% c("string", "bool")) typ else paste0("scalar_", typ)
  expect_equal(typ, "scalar_integer")
  x <- 2L
  test_call <- rlang::call2(paste0("is_", typ), x = x, .ns = "rlang")
  expect_true(eval(test_call))
  test_call <- rlang::call_modify(test_call, x = NA_integer_)
  expect_true(eval(test_call))
  test_call <- rlang::call_modify(test_call, x = 9)
  expect_false(eval(test_call))
})
