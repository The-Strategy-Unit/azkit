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

test_that("whole read_parquet function works", {
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


test_that("read_azure_json basically works", {
  endpoint_uri <- Sys.getenv("AZ_STORAGE_EP")
  # only run the test if this variable is set (i.e. locally, but not on GitHub)
  if (nzchar(endpoint_uri)) {
    expect_no_error(support_container <- get_container("supporting-data"))
    raw_out <- support_container |>
      download_azure_blob("providers", "json", FALSE)
    expect_type(raw_out, "raw")
    # {yyjsonr} provides a function to read raw JSON (fast) - unlike {jsonlite}
    expect_no_error(yyjsonr::read_json_raw(raw_out))
    out <- yyjsonr::read_json_raw(raw_out)
    expect_type(out, "character")
    expect_length(out, 138)
  }
})

test_that("dirname and basename logic works", {
  endpoint_uri <- Sys.getenv("AZ_STORAGE_EP")
  # only run the test if this variable is set (i.e. locally, but not on GitHub)
  if (nzchar(endpoint_uri)) {
    expect_no_error(support_container <- get_container("supporting-data"))
    file <- "neecom_table.rds"
    path <- ""
    expect_no_error(AzureStor::blob_exists(support_container, file))
    file2 <- paste0("/", file)
    file3 <- paste0("//", file)
    expect_no_error(AzureStor::blob_exists(support_container, file2))
    expect_no_error(AzureStor::blob_exists(support_container, file3))
    expect_equal(dirname(file), ".")
    file <- paste0(path, "/", file)
    expect_equal(dirname(file), "/")
    path <- "/"
    file <- paste0(path, "/", file)
    expect_equal(dirname(file), "/")

    new_path <- sub("^\\.$", "/", dirname(file))
    expect_equal(new_path, "/")
    path <- "QA"
    file <- "none.txt"
    file <- paste0(path, "/", file)
    new_path <- sub("^\\.$", "/", dirname(file))
    expect_equal(new_path, "QA")

    res <- get_container("results")
    path <- "prod/dev/national/national"
    file <- "test-2025"
    file_ext <- "json.gz"
    filepath <- sub("^/+", "", paste0(path, "/", file))
    expect_equal(filepath, "prod/dev/national/national/test-2025")

    path <- "/"
    filepath <- sub("^/+", "", paste0(path, "/", file))
    expect_equal(dirname(filepath), ".")

    path <- "prod/dev/national/national"
    filepath <- sub("^/+", "", paste0(path, "/", file))
    expect_equal(filepath, "prod/dev/national/national/test-2025")
    path <- sub("^\\.$", "/", dirname(filepath))

    filepath_out <- AzureStor::list_blobs(res, path, recursive = FALSE) |>
      dplyr::filter(
        !dplyr::if_any("isdir") &
          # Don't include `filepath` in the first regex here, because we want to
          # filter to `file_ext` explicitly, as well as also allow for `filepath`
          # to include its file extension if that suits the user's approach.
          dplyr::if_any("name", \(x) {
            gregg(x, "\\.{file_ext}$") & gregg(x, "^{filepath}")
          })
      ) |>
      dplyr::pull("name")
    expect_match(filepath_out, "prod/dev/national/national/test-2025.*json.gz")
  }
})

test_that("tdd of check_blob_exists", {
  endpoint_uri <- Sys.getenv("AZ_STORAGE_EP")
  # only run the test if this variable is set (i.e. locally, but not on GitHub)
  if (nzchar(endpoint_uri)) {
    expect_no_error(support_container <- get_container("supporting-data"))
    check_blob_exists <- function(container, file, path = "/") {
      stopifnot("path not found" = AzureStor::blob_dir_exists(container, path))
    }
    expect_no_error(check_blob_exists(support_container, "file"))

    check_blob_exists <- function(container, file, path = "/") {
      stopifnot("path not found" = AzureStor::blob_dir_exists(container, path))
      file_ext <- "json"
      AzureStor::list_blobs(container, path, recursive = FALSE) |>
        dplyr::filter(
          !dplyr::if_any("isdir") &
            dplyr::if_any("name", \(x) {
              # Don't include `file` in the regex here, because we want to filter to
              # `file_ext` explicitly, as well as also allow for `file` to include
              # its file extension if that suits the user's approach.
              grepl(glue::glue("\\.{file_ext}$"), x) & grepl(file, x)
            })
        ) |>
        dplyr::pull("name")
    }
    expect_no_error(check_blob_exists(support_container, "sites"))
    expect_length(check_blob_exists(support_container, "sites"), 2)

    check_blob_exists <- function(container, file, path = "/") {
      stopifnot("path not found" = AzureStor::blob_dir_exists(container, path))
      file_ext <- "json"
      filepath <- AzureStor::list_blobs(container, path, recursive = FALSE) |>
        dplyr::filter(
          !dplyr::if_any("isdir") &
            dplyr::if_any("name", \(x) {
              grepl(glue::glue("\\.{file_ext}$"), x) & grepl(file, x)
            })
        ) |>
        dplyr::pull("name")
      stop_msg1 <- glue::glue("no matching {file_ext} file found")
      stop_msg2 <- glue::glue("multiple matching {file_ext} files found")
      check_vec(filepath, rlang::is_character, stop_msg1) # check length > 0
      check_scalar_type(filepath, "character", stop_msg2) # check length == 1
    }
    expect_error(check_blob_exists(support_container, "unmatched"), "matching")
    expect_error(check_blob_exists(support_container, "sites"), "multiple")

    check_blob_exists <- function(container, file, path = "/") {
      stopifnot("path not found" = AzureStor::blob_dir_exists(container, path))
      file_ext <- "json"
      filepath <- AzureStor::list_blobs(container, path, recursive = FALSE) |>
        dplyr::filter(
          !dplyr::if_any("isdir") &
            dplyr::if_any("name", \(x) {
              grepl(glue::glue("\\.{file_ext}$"), x) & grepl(file, x)
            })
        ) |>
        dplyr::pull("name")
      stop_msg1 <- glue::glue("no matching {file_ext} file found")
      stop_msg2 <- glue::glue("multiple matching {file_ext} files found")
      check_vec(filepath, rlang::is_character, stop_msg1) # check length > 0
      check_scalar_type(filepath, "character", stop_msg2) # check length == 1
      filepath
    }
    expect_no_error(check_blob_exists(support_container, "providers"))
    expect_no_error(check_blob_exists(support_container, "providers.json"))
  }
})


# parquet and json read functions need `download_blob` first before reading in.
# However there are 'native' {AzureStor} functions for csv and rds files, so we
# should use those instead. Requiring a slightly different (simpler) workflow.

test_that("read_azure_csv basically works", {
  endpoint_uri <- Sys.getenv("AZ_STORAGE_EP")
  # only run the test if this variable is set (i.e. locally, but not on GitHub)
  if (nzchar(endpoint_uri)) {
    expect_no_error(support_container <- get_container("supporting-data"))
    support_container |>
      # should error as this stub will match more than 1 file
      download_azure_blob("mitigator-lookup", "csv", FALSE) |>
      expect_error()
    support_container |>
      download_azure_blob("mitigator-lookup.csv", "csv", FALSE) |>
      expect_no_error()
    raw_out <- support_container |>
      download_azure_blob("mitigator-lookup.csv", "csv", FALSE)
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
      download_azure_blob("mitigator-lookup.csv", "csv", FALSE) |>
      readr::read_csv(col_types = col_types) |>
      expect_no_error()
    csv_out2 <- support_container |>
      AzureStor::storage_read_csv(
        "mitigator-lookup.csv",
        col_types = col_types
      ) |>
      expect_no_error()
    expect_identical(csv_out1, csv_out2)
    expect_length(csv_out1, 3) # ncol
  }
})

test_that("read functions all work a bit at least", {
  endpoint_uri <- Sys.getenv("AZ_STORAGE_EP")
  # only run the test if this variable is set (i.e. locally, but not on GitHub)
  if (nzchar(endpoint_uri)) {
    res <- get_container(Sys.getenv("AZ_RESULTS_CONTAINER"))
    expect_no_error(read_azure_parquet(res, Sys.getenv("TEST_PARQUET_FILE")))
    supp <- get_container(Sys.getenv("AZ_SUPPORT_CONTAINER"))
    expect_no_error(read_azure_json(supp, Sys.getenv("TEST_JSON_FILE")))
    expect_no_error(read_azure_rds(supp, Sys.getenv("TEST_RDS_FILE")))
    expect_no_error(read_azure_csv(supp, Sys.getenv("TEST_CSV_FILE")))
  }
})


test_that("check that sub-functions inherit options in R!", {
  yes <- \() getOption("do_it") %||% "nope"
  wrap <- \() yes()
  expect_identical(withr::with_options(list(do_it = "yes"), wrap()), "yes")
})
