test_that("possibly manages failure by returning NULL", {
  possibly_get_mtk <- \(...) purrr::possibly(AzureAuth::get_managed_token)(...)
  managed_resource <- "https://management.azure.com"
  expect_null(possibly_get_mtk(managed_resource))
})
