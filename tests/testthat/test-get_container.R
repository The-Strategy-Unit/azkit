test_that("simple failing test for missing env var", {
  withr::with_envvar(c(AZ_STORAGE_EP = ""), get_container("random")) |>
    expect_error("`AZ_STORAGE_EP` is not set", class = "rlang_error")
})

test_that("basic success", {
  endpoint_uri <- Sys.getenv("AZ_STORAGE_EP")
  # only run the test if this variable is set (ie locally, but not on GitHub)
  if (nzchar(endpoint_uri)) {
    token <- get_auth_token()
    expect_false(is.null(token))
    expect_s3_class(token, "AzureToken")

    # explore behaviour with blob_endpoint
    ep <- AzureStor::blob_endpoint(endpoint_uri, token = token) |>
      expect_no_error()
    expect_s3_class(ep, "blob_endpoint")
    cont <- expect_no_error(AzureStor::blob_container(ep, "supporting-data"))
    expect_s3_class(cont, "blob_container")
    expect_error(AzureStor::list_adls_files(cont))
    expect_no_error(AzureStor::list_blobs(cont))

    # compare behaviour with adls_endpoint instead (it's the same)
    ep <- AzureStor::adls_endpoint(endpoint_uri, token = token)
    expect_s3_class(ep, "adls_endpoint")
    fs <- AzureStor::adls_filesystem(ep, "results") |>
      expect_no_error()
    expect_s3_class(fs, "adls_filesystem")
    path <- "/archive/dev/synthetic"
    # Expected this to succeed with adls_endpoint but it doesn't
    expect_error(AzureStor::list_adls_files(fs, path))
    # list_blobs works just like it does with the blob container
    expect_no_error(AzureStor::list_blobs(fs, path))
  }
})
