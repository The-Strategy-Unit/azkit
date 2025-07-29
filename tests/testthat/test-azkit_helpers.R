test_that("check_val uses {cli} formatting and glue variables", {
  if (require("purrr")) {
    var <- "test"
    expect_identical(check_val(letters, nzchar), letters)
    expect_error(check_val(letters, nzchar, "{.var {var}}", "none"), "`test`")
  }
})
