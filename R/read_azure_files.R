#' Read a parquet file from Azure storage
#'
#' @param container An Azure container object, as returned by `get_container()`
#' @param file The name of the file to be read, as a string. NB this can be a
#'  partial match, and for example the file extension does not need to be
#'  included (though it can be). The function will error if multiple files are
#'  matched.
#' @param path The path to the directory where `file` is located, as a string.
#'  This must be the full path to the file location, as the function will not
#'  search into subdirectories recursively. Set to `"/"` (the root of the
#'  container) by default.
#' @param info Boolean. Whether to print user feedback about the file that is
#'  being read. Useful for checking the function is doing what is expected, but
#'  can be turned off with `FALSE`. Can be set persistently with the option
#'  "azkit.info". If `NULL` then it will default to the value of
#'  `rlang::is_interactive()` (ie `TRUE` for interactive sessions).
#' @param ... optional arguments to be passed through to `arrow::read_parquet()`
#' @returns A tibble
#' @examples \dontrun{
#'   # if a full filepath is available then path can be ignored
#'   read_azure_parquet(cont, "data/folder/path/1.parquet")
#'   # you can provide a filename without the '.parquet' extension
#'   # if you wish to use partial file name matching then it is probably easier
#'   # to provide a 'path'
#'   read_azure_parquet(cont, "case_details", "storage/parquet/2025/06/29")
#' }
#' @export
read_azure_parquet <- function(container, file, path = "/", info = NULL, ...) {
  check_blob_exists(container, file, "parquet", info, path) |>
    # using `dest = NULL` means read the data as a raw vector
    AzureStor::download_blob(container, src = _, dest = NULL) |>
    arrow::read_parquet(...)
}


#' Read a json file from Azure storage
#'
#' @inheritParams read_azure_parquet
#' @param ... optional arguments to be passed through to
#'  `yyjsonr::read_json_raw()`
#' @returns A list
#' @export
read_azure_json <- function(container, file, path = "/", info = NULL, ...) {
  check_blob_exists(container, file, "json", info, path) |>
    # using `dest = NULL` means read the data as a raw vector
    AzureStor::download_blob(container, src = _, dest = NULL) |>
    yyjsonr::read_json_raw(...)
}


#' Read an rds file from Azure storage
#'
#' @inheritParams read_azure_parquet
#' @returns Data object that was stored in the rds file
#' @export
read_azure_rds <- function(container, file, path = "/", info = NULL) {
  check_blob_exists(container, file, "rds", info, path) |>
    AzureStor::storage_load_rds(container, file = _)
}


#' Read a csv file from Azure storage
#'
#' @inheritParams read_azure_parquet
#' @param ... optional arguments to be passed through to `readr::read_delim()`
#' @returns A tibble
#' @export
read_azure_csv <- function(container, file, path = "/", info = NULL, ...) {
  check_blob_exists(container, file, "csv", info, path) |>
    AzureStor::storage_read_csv(container, file = _, ...)
}


#' Ensures that the filepath for the file to read exists
#'
#' @inheritParams read_azure_parquet
#' @param file_ext The standard file extension for the file type, e.g. "json"
#' @keywords internal
check_blob_exists <- function(container, file, file_ext, info, path = "") {
  stopifnot("no container found" = inherits(container, "blob_container"))
  stopifnot("path not found" = AzureStor::blob_dir_exists(container, path))
  filepath <- sub("^/+", "", paste0(path, "/", file))
  path <- sub("^\\.$", "/", dirname(filepath))
  filepath_out <- AzureStor::list_blobs(container, path, recursive = FALSE) |>
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
  stop_msg1 <- glue::glue("no matching {file_ext} file found")
  stop_msg2 <- glue::glue("multiple matching {file_ext} files found")
  check_vec(filepath_out, rlang::is_character, stop_msg1) # check length > 0
  check_scalar_type(filepath_out, "character", stop_msg2) # check length == 1

  info_option <- getOption("azkit.info")
  stopifnot(rlang::is_scalar_logical(info) || is.null(info))
  stopifnot(rlang::is_scalar_logical(info_option) || is.null(info_option))
  if (info %||% info_option %||% rlang::is_interactive()) {
    cli::cli_alert_info("File {.val {filepath_out}} will be read in")
  }
  filepath_out
}
