test_that("check_vec uses {cli} formatting and glue variables", {
  if (require("purrr")) {
    var <- "test"
    expect_identical(check_vec(letters, nzchar), letters)
    expect_error(check_vec(letters, nzchar, "{.var {var}}", "none"), "`test`")
  }
})
