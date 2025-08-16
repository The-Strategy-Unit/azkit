test_that("simple failing tests for missing env vars", {
  withr::with_envvar(c(AZ_CONTAINER = ""), get_container()) |>
    expect_error("`AZ_CONTAINER` is not set", class = "rlang_error")

  c(AZ_CONTAINER = "results", AZ_STORAGE_EP = "") |>
    withr::with_envvar(get_container()) |>
    expect_error("`AZ_STORAGE_EP` is not set", class = "rlang_error")
})

test_that("basic success", {
  endpoint_uri <- Sys.getenv("AZ_STORAGE_EP")
  # only run the test if this variable is set (ie locally, but not on GitHub)
  if (nzchar(endpoint_uri)) {
    token <- get_auth_token() # should return NULL if unsuccessful
    expect_false(is.null(token))
    expect_s3_class(token, "AzureToken")

    # explore behaviour with blob_endpoint
    # these tests are no good because they pass even if `endpoint_uri` is
    # not a real endpoint URI
    expect_no_error(AzureStor::blob_endpoint(endpoint_uri, token = token))
    ep <- AzureStor::blob_endpoint(endpoint_uri, token = token)
    expect_s3_class(ep, "blob_endpoint")
    expect_no_error(AzureStor::blob_container(ep, "supporting-data"))
    cont <- AzureStor::blob_container(ep, "supporting-data")
    expect_s3_class(cont, "blob_container")

    expect_error(AzureStor::list_adls_files(cont))
    expect_no_error(AzureStor::list_blobs(cont))

    # compare behaviour with adls_endpoint instead (it's the same)
    ep <- AzureStor::adls_endpoint(endpoint_uri, token = token)
    expect_s3_class(ep, "adls_endpoint")
    expect_no_error(AzureStor::adls_filesystem(ep, "results"))
    fs <- AzureStor::adls_filesystem(ep, "results")
    expect_s3_class(fs, "adls_filesystem")
    path <- "/archive/dev/synthetic"
    # Expected this to succeed with adls_endpoint but it doesn't
    expect_error(AzureStor::list_adls_files(fs, path))
    # list_blobs works just like it does with the blob container
    expect_no_error(AzureStor::list_blobs(fs, path))
  }
})
