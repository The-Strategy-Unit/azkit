test_that("possibly manages failure by returning NULL", {
  possibly_get_mtk <- \(...) purrr::possibly(AzureAuth::get_managed_token)(...)
  managed_resource <- "https://management.azure.com"
  expect_null(possibly_get_mtk(managed_resource))
})


test_that("generate_resource() behaves itself", {
  generate_resource(version = 3) |>
    expect_error()
  base_url <- "https://storage.azure.com"
  def_url <- paste0(base_url, "/.default")
  def1 <- c(def_url, "openid", "offline_access")
  generate_resource() |>
    expect_equal(def1)
  def2 <- c(def_url, "openid")
  generate_resource(refresh = FALSE) |>
    expect_equal(def2)
  generate_resource(authorise = FALSE) |>
    expect_equal(c("openid", "offline_access"))
  generate_resource(authorise = FALSE, refresh = FALSE) |>
    expect_equal("openid")
  generate_resource(version = 1) |>
    expect_equal(base_url)
  generate_resource(version = 1, refresh = FALSE) |>
    expect_equal(base_url)
  generate_resource(version = 1, authorise = FALSE) |>
    expect_equal("")
  generate_resource(version = 1, authorise = FALSE, refresh = FALSE) |>
    expect_equal("")
})
