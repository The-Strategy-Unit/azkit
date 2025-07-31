test_that("simple failing test for missing env var", {
  withr::with_envvar(c(AZ_STORAGE_EP = ""), get_container("random")) |>
    expect_error("env var is not set", class = "rlang_error")
})
