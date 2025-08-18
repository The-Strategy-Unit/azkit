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
    inputs_data <- AzureStor::blob_container(ep, "inputs-data") |>
      expect_no_error()
    expect_s3_class(inputs_data, "blob_container")
    files <- AzureStor::list_blobs(inputs_data, "dev", recursive = FALSE) |>
      dplyr::filter(grepl("\\.parquet$", .data[["name"]])) |>
      dplyr::pull("name")
    name <- "wli"
    pqt_file <- grep(name, files, value = TRUE)
    expect_length(pqt_file, 1)
    expect_true(AzureStor::blob_exists(inputs_data, pqt_file))
    out <- AzureStor::download_blob(inputs_data, pqt_file, dest = NULL) |>
      arrow::read_parquet()
    expect_s3_class(out, "tbl_df")
    expect_length(out, 6) # ncol
  }
})

test_that("whole function works", {
  endpoint_uri <- Sys.getenv("AZ_STORAGE_EP")
  # only run the test if this variable is set (i.e. locally, but not on GitHub)
  if (nzchar(endpoint_uri)) {
    inputs_container <- expect_no_error(get_container("inputs-data"))
    expect_no_error(read_azure_parquet(inputs_container, "wli", path = "dev"))
    out1 <- read_azure_parquet(inputs_container, "wli", path = "dev")
    expect_s3_class(out1, "tbl_df")
    # check that it works with the file extension included
    out2 <- read_azure_parquet(inputs_container, "wli.parquet", path = "dev")
    expect_length(out2, 6) # ncol
    # this should error as there are >1 files matching "repat"
    read_azure_parquet(inputs_container, "repat", path = "dev") |>
      expect_error()
  }
})


test_that("check that sub-functions inherit options in R!", {
  yes <- \() getOption("do_it") %||% "nope"
  wrap <- \() yes()
  expect_identical(withr::with_options(list(do_it = "yes"), wrap()), "yes")
})
