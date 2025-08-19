#' Read a parquet file from Azure storage
#'
#' @param container An Azure container object, as returned by `get_container()`
#' @param file The name of the file to be read, as a string. NB this can be a
#'  partial match, and for example the file extension does not need to be
#'  included (though it can be). The function will error if multiple files are
#'  matched.
#' @param path The path to the directory where `file` is located, as a string.
#'  This must be the full path to the file location, as the function will not
#'  search into subdirectories recursively. Set to `"\\"` (the root of the
#'  container) by default.
#' @param info Boolean. Whether to print user feedback about the file that is
#'  being read. Useful for checking the function is doing what is expected, but
#'  can be turned off with `FALSE`. Can be set persistently with the option
#'  "azkit.info". If `NULL` then it will default to the value of
#'  `rlang::is_interactive()` (ie `TRUE` for interactive sessions).
#' @param ... optional arguments to be passed through to `arrow::read_parquet()`
#' @returns A tibble
#' @export
read_azure_parquet <- function(container, file, path = "/", info = NULL, ...) {
  stopifnot("no container found" = inherits(container, "blob_container"))
  download_azure_blob(container, path, file, "parquet", info) |>
    arrow::read_parquet(...)
}


#' Read an rds file from Azure storage
#'
#' @inheritParams read_azure_parquet
#' @returns Data object that was stored in the rds file
#' @export
read_azure_rds <- function(container, file, path = "/", info = NULL) {
  stopifnot("no container found" = inherits(container, "blob_container"))
  download_azure_blob(container, path, file, "rds", info) |>
    readr::read_rds()
}


#' Read a csv file from Azure storage
#'
#' @inheritParams read_azure_parquet
#' @param ... optional arguments to be passed through to `readr::read_csv()`
#' @returns A tibble
#' @export
read_azure_csv <- function(container, file, path = "/", info = NULL, ...) {
  stopifnot("no container found" = inherits(container, "blob_container"))
  download_azure_blob(container, path, file, "csv", info) |>
    readr::read_csv(...)
}


#' Read a json file from Azure storage
#'
#' @inheritParams read_azure_parquet
#' @param ... optional arguments to be passed through to
#'  `yyjsonr::read_json_raw()`
#' @returns A list
#' @export
read_azure_json <- function(container, file, path = "/", info = NULL, ...) {
  stopifnot("no container found" = inherits(container, "blob_container"))
  download_azure_blob(container, path, file, "json", info) |>
    yyjsonr::read_json_raw(...)
}


#' Common routine for all `read_azure_*()` functions
#'
#' Downloads the blob with `dest = NULL`, which keeps the data in memory
#'
#' @inheritParams read_azure_parquet
#' @param file_ext The standard file extension for the file type, e.g. "rds"
#' @keywords internal
download_azure_blob <- function(container, path, file, file_ext, info) {
  stopifnot("path not found" = AzureStor::blob_dir_exists(container, path))
  filepath <-
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
  stop_msg1 <- glue::glue("no matching {file_ext} file found")
  stop_msg2 <- glue::glue("multiple matching {file_ext} files found")
  stopifnot(rlang::set_names(length(filepath) > 0, stop_msg1))
  stopifnot(rlang::set_names(length(filepath) == 1, stop_msg2))
  info_option <- getOption("azkit.info")
  stopifnot(rlang::is_scalar_logical(info_option) || is.null(info_option))
  if (info %||% info_option %||% rlang::is_interactive()) {
    cli::cli_alert_info("File {.val {filepath}} will be read in")
  }
  AzureStor::download_blob(container, filepath, dest = NULL)
}
