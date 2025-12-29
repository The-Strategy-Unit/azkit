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
    name <- "baseline"
    pqt_file <- grep(name, files, value = TRUE)
    expect_length(pqt_file, 1)
    expect_true(AzureStor::blob_exists(inputs_data, pqt_file))
    out <- AzureStor::download_blob(inputs_data, pqt_file, dest = NULL) |>
      arrow::read_parquet()
    expect_s3_class(out, "tbl_df")
    expect_length(out, 6) # ncol
  }
})


test_that("understand some new errors in check_blob_exists", {
  endpoint_uri <- Sys.getenv("AZ_STORAGE_EP")
  # only run the test if this variable is set (i.e. locally, but not on GitHub)
  if (nzchar(endpoint_uri)) {
    inp <- expect_no_error(get_container("inputs-data"))
    path <- "dev"
    file <- "baseline"
    ext <- "parquet"
    path <- if (path %in% c("", "/")) "" else path
    expect_equal(path, "dev")
    dir_name <- if (dirname(file) == ".") "" else dirname(file)
    expect_equal(dir_name, "")
    p2 <- file.path(path, dir_name)
    expect_equal(p2, "dev")
    file_name <- paste0(basename(file), ".", ext)
    expect_equal(file_name, "baseline.parquet")
    file_path <- sub("^/", "", sub("/+", "/", file.path(p2, file_name)))
    expect_equal(file_path, "dev/baseline.parquet")
    dir_list <- AzureStor::list_blobs(inp, p2, recursive = FALSE)
    file_name_out <- dir_list |>
      dplyr::filter(dplyr::if_any("name", \(x) x == file_path)) |>
      dplyr::pull("name")
    expect_equal(file_name_out, "dev/baseline.parquet")
    expect_no_error(check_blob_exists(inp, file, ext, FALSE, path))

    # check still works if full filepath is passed to file arg
    path <- ""
    file <- "dev/baseline.parquet"
    path <- if (path %in% c("", "/")) "" else path
    expect_equal(path, "")
    dir_name <- if (dirname(file) == ".") "" else dirname(file)
    expect_equal(dir_name, "dev")
    p2 <- glue::glue("{path}/{dir_name}")
    expect_equal(p2, "/dev")
    file_name <- basename(file)
    expect_equal(file_name, "baseline.parquet")
    file_path <- sub("^/", "", sub("/+", "/", glue::glue("{p2}/{file_name}")))
    expect_equal(file_path, "dev/baseline.parquet")
    dir_list <- AzureStor::list_blobs(inp, p2, recursive = FALSE)
    file_name_out <- dir_list |>
      dplyr::filter(dplyr::if_any("name", \(x) x == {{ file_path }})) |>
      dplyr::pull("name")
    expect_equal(file_name_out, file)
    expect_no_error(check_blob_exists(inp, file, ext, FALSE, path))
  }
})


test_that("whole read_parquet function works", {
  endpoint_uri <- Sys.getenv("AZ_STORAGE_EP")
  # only run the test if this variable is set (i.e. locally, but not on GitHub)
  if (nzchar(endpoint_uri)) {
    inputs_container <- expect_no_error(get_container("inputs-data"))
    expect_no_error(read_azure_parquet(
      inputs_container,
      "baseline",
      path = "dev"
    ))
    out1 <- read_azure_parquet(inputs_container, "baseline", path = "dev")
    expect_s3_class(out1, "tbl_df")
    # check that it works with the file extension included
    out2 <- read_azure_parquet(
      inputs_container,
      "baseline.parquet",
      path = "dev"
    )
    expect_length(out2, 6) # ncol

    res <- get_container(Sys.getenv("AZ_RESULTS_CONTAINER"))
    pqt_file <- Sys.getenv("TEST_PARQUET_FILE")
    path <- "/"
    file_ext <- "parquet"

    # experiment with changes to code:
    path <- if (path %in% c("", "/")) "" else path
    expect_equal(path, "")
    dir_name <- if (dirname(pqt_file) == ".") "" else dirname(pqt_file)
    dpath <- glue::glue("{path}/{dir_name}")
    file_name <- sub(glue::glue("\\.{file_ext}$"), "", basename(pqt_file))

    file_name <- if (gregg(basename(pqt_file), "\\.{file_ext}$")) {
      basename(pqt_file)
    } else {
      glue::glue("{basename(pqt_file)}.{file_ext}")
    }
    # remove duplicate slashes and any initial slashes
    fpath <- sub("^/", "", sub("/+", "/", glue::glue("{dpath}/{file_name}")))
    expect_equal(fpath, pqt_file)
    # now function should run without error
    expect_no_error(check_blob_exists(res, pqt_file, "parquet", FALSE, "/"))
    # we want this to error if the file_ext doesn't match the file
    expect_error(check_blob_exists(res, pqt_file, "rds", FALSE, "/"))
  }
})


test_that("read_azure_json basically works", {
  endpoint_uri <- Sys.getenv("AZ_STORAGE_EP")
  # only run the test if this variable is set (i.e. locally, but not on GitHub)
  if (nzchar(endpoint_uri)) {
    download_azure_blob <- function(container, file, file_ext, path = "") {
      check_blob_exists(container, file, file_ext, FALSE, path) |>
        AzureStor::download_blob(container, src = _, dest = NULL)
    }
    expect_no_error(support_container <- get_container("supporting-data"))
    raw_out <- support_container |>
      download_azure_blob("providers", "json")
    expect_type(raw_out, "raw")
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
    download_azure_blob <- function(container, file, file_ext, path = "") {
      check_blob_exists(container, file, file_ext, FALSE, path) |>
        AzureStor::download_blob(container, src = _, dest = NULL)
    }
    expect_no_error(support_container <- get_container("supporting-data"))
    support_container |>
      # should construct the file name OK
      download_azure_blob("mitigator-lookup", "csv") |>
      expect_no_error()
    support_container |>
      download_azure_blob("mitigator-lookup.csv", "csv") |>
      expect_no_error()
    raw_out <- support_container |>
      download_azure_blob("mitigator-lookup.csv", "csv")
    expect_type(raw_out, "raw")
    expect_no_error(readr::read_csv(raw_out, show_col_types = FALSE))
    dat <- readr::read_csv(raw_out, show_col_types = FALSE)
    expect_type(dat, "list")
    expect_s3_class(dat, "tbl_df")
  }
})


test_that("... parameters are passed through", {
  endpoint_uri <- Sys.getenv("AZ_STORAGE_EP")
  # only run the test if this variable is set (i.e. locally, but not on GitHub)
  if (nzchar(endpoint_uri)) {
    download_azure_blob <- function(container, file, file_ext, path = "") {
      check_blob_exists(container, file, file_ext, FALSE, path) |>
        AzureStor::download_blob(container, src = _, dest = NULL)
    }
    expect_no_error(support_container <- get_container("supporting-data"))
    col_types <- "ccc------"
    csv_out1 <- support_container |>
      download_azure_blob("mitigator-lookup.csv", "csv") |>
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
    read_azure_csv(supp, Sys.getenv("TEST_CSV_FILE"), show_col_types = FALSE) |>
      expect_no_error()
  }
})


test_that("check that sub-functions inherit options in R!", {
  yes <- \() getOption("do_it") %||% "nope"
  wrap <- \() yes()
  expect_identical(withr::with_options(list(do_it = "yes"), wrap()), "yes")
})
