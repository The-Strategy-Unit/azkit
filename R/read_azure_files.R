#' Read a parquet file from Azure storage
#'
#' @param container An Azure container object, as returned by [get_container()]
#' @param file The name of the file to be read, as a string. NB The file
#'  extension does not need to be included (though it can be). The function
#'  will error if multiple files are somehow matched.
#' @param path The path to the directory where `file` is located, as a string.
#'  Only needed if `file` does not already contain its full path. If file is
#'  just a file name with no path, then provide the path to the directory here.
#'  This must be the full path to the file location, as the function will not
#'  search into subdirectories recursively. Set to `"/"` (the root of the
#'  container) by default.
#' @param info Boolean. Whether to print user feedback about the file that is
#'  being read. Useful for checking the function is doing what is expected, but
#'  can be turned off with `FALSE`. Can be set persistently with the option
#'  "azkit.info". If `NULL` then it will default to the value of
#'  [rlang::is_interactive()] (ie `TRUE` for interactive sessions).
#' @param ... optional arguments to be passed through to [arrow::read_parquet()]
#' @returns A tibble
#' @examples \dontrun{
#'   # if a full filepath is available then path can be ignored
#'   read_azure_parquet(cont, "data/folder/path/1.parquet")
#'   # you can provide a filename without the '.parquet' extension
#'   # if you wish to use this partial file name matching it is probably easier
#'   # to provide a 'path'
#'   read_azure_parquet(cont, "case_details", "storage/parquet/2025/06/29")
#' }
#' @export
read_azure_parquet <- function(container, file, path = "/", info = NULL, ...) {
  check_blob_exists(container, file, "parquet", info, path) |>
    # using `dest = NULL` means pass the data through as a raw vector
    AzureStor::download_blob(container, src = _, dest = NULL) |>
    arrow::read_parquet(...)
}


#' Read a json file from Azure storage
#'
#' @inheritParams read_azure_parquet
#' @param ... optional arguments to be passed through to
#'  [yyjsonr::read_json_raw()]
#' @returns A list
#' @export
read_azure_json <- function(container, file, path = "/", info = NULL, ...) {
  check_blob_exists(container, file, "json", info, path) |>
    # using `dest = NULL` means pass the data through as a raw vector
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
#' @param ... optional arguments to be passed through to [readr::read_delim()]
#' @returns A tibble
#' @export
read_azure_csv <- function(container, file, path = "/", info = NULL, ...) {
  check_blob_exists(container, file, "csv", info, path) |>
    AzureStor::storage_read_csv(container, file = _, ...)
}


#' Ensures that the filepath for the file to read exists
#'
#' @inheritParams read_azure_parquet
#' @param ext The standard file extension for the file type, e.g. "json"
#' @keywords internal
check_blob_exists <- function(container, file, ext, info, path) {
  stopifnot("no container found" = inherits(container, "blob_container"))
  path <- if (path %in% c("", "/")) "" else path
  stopifnot("path not found" = AzureStor::blob_dir_exists(container, path))
  dir_name <- if (dirname(file) == ".") "" else dirname(file)
  # Potentially the user could provide a partial file path in `path` and a
  # further sub-directory as part of `file`. This handles that eventuality,
  # though this usage pattern should be quite rare!
  dpath <- file.path(path, dir_name)
  fname <- basename(file)
  fname <- ifelse(gregg(fname, "\\.{ext}$"), fname, glue::glue("{fname}.{ext}"))
  # remove duplicate slashes and any initial slashes
  file_path <- sub("^/", "", gsub("/+", "/", file.path(dpath, fname)))

  filepath_out <- AzureStor::list_blobs(container, dpath, recursive = FALSE) |>
    dplyr::filter(dplyr::if_any("name", \(x) x == {{ file_path }})) |>
    dplyr::pull("name")

  msg1 <- cv_error_msg("no matching {ext} file found")
  msg2 <- cst_error_msg("multiple matching {ext} files found")
  check_vec(filepath_out, rlang::is_character, msg1) # check length > 0
  check_scalar_type(filepath_out, "character", msg2) # check length == 1

  info_option <- getOption("azkit.info")
  stopifnot(rlang::is_scalar_logical(info) || is.null(info))
  stopifnot(rlang::is_scalar_logical(info_option) || is.null(info_option))
  if (info %||% info_option %||% rlang::is_interactive()) {
    cli::cli_alert_info("File {.val {filepath_out}} will be read in")
  }
  filepath_out
}
