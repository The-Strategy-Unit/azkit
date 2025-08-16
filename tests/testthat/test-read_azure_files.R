test_that("basic success", {
  endpoint_uri <- Sys.getenv("AZ_STORAGE_EP")
  # only run the test if this variable is set (ie locally, but not on GitHub)
  if (nzchar(endpoint_uri)) {
    token <- get_auth_token()
    expect_false(is.null(token))
    expect_s3_class(token, "AzureToken")

    # explore behaviour with blob_endpoint
    expect_no_error(AzureStor::blob_endpoint(endpoint_uri, token = token))
    ep <- AzureStor::blob_endpoint(endpoint_uri, token = token)
    expect_s3_class(ep, "blob_endpoint")
    expect_no_error(AzureStor::blob_container(ep, "inputs-data"))
    inputs_data <- AzureStor::blob_container(ep, "inputs-data")
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
    expect_no_error(inputs_container <- get_container("inputs-data"))
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


test_that("read_azure_json basically works", {
  endpoint_uri <- Sys.getenv("AZ_STORAGE_EP")
  # only run the test if this variable is set (i.e. locally, but not on GitHub)
  if (nzchar(endpoint_uri)) {
    expect_no_error(support_container <- get_container("supporting-data"))
    raw_out <- support_container |>
      download_azure_blob("/", "providers", "json", FALSE)
    expect_type(raw_out, "raw")
    # {yyjsonr} provides a function to read raw JSON (fast) - unlike {jsonlite}
    expect_no_error(yyjsonr::read_json_raw(raw_out))
    out <- yyjsonr::read_json_raw(raw_out)
    expect_type(out, "character")
    expect_length(out, 138)
  }
})


test_that("read_azure_csv basically works", {
  endpoint_uri <- Sys.getenv("AZ_STORAGE_EP")
  # only run the test if this variable is set (i.e. locally, but not on GitHub)
  if (nzchar(endpoint_uri)) {
    expect_no_error(support_container <- get_container("supporting-data"))
    support_container |>
      # should error as this stub will match more than 1 file
      download_azure_blob("/", "mitigator-lookup", "csv", FALSE) |>
      expect_error()
    support_container |>
      download_azure_blob("/", "mitigator-lookup.csv", "csv", FALSE) |>
      expect_no_error()
    raw_out <- support_container |>
      download_azure_blob("/", "mitigator-lookup.csv", "csv", FALSE)
    expect_type(raw_out, "raw")
    expect_no_error(readr::read_csv(raw_out))
    dat <- readr::read_csv(raw_out)
    expect_type(dat, "list")
    expect_s3_class(dat, "tbl_df")
  }
})


test_that("... parameters are passed through", {
  endpoint_uri <- Sys.getenv("AZ_STORAGE_EP")
  # only run the test if this variable is set (i.e. locally, but not on GitHub)
  if (nzchar(endpoint_uri)) {
    expect_no_error(support_container <- get_container("supporting-data"))
    col_types <- "ccc------"
    csv_out1 <- support_container |>
      download_azure_blob("/", "mitigator-lookup.csv", "csv", FALSE) |>
      readr::read_csv(col_types = col_types) |>
      expect_no_error()
    csv_out2 <- support_container |>
      read_azure_csv("mitigator-lookup.csv", col_types = col_types) |>
      expect_no_error()
    expect_identical(csv_out1, csv_out2)
    expect_length(csv_out1, 3) # ncol
  }
})


test_that("check that sub-functions inherit options in R!", {
  yes <- \() getOption("do_it") %||% "nope"
  wrap <- \() yes()
  expect_identical(withr::with_options(list(do_it = "yes"), wrap()), "yes")
})
