#' Read a parquet file from Azure storage
#'
#' @param container An Azure container object, as returned by [get_container]
#' @param file string The path to the file to be read.
#' @param ... optional arguments to be passed through to [arrow::read_parquet]
#' @returns A tibble
#' @examples \dontrun{
#'   read_azure_parquet(cont, "data/folder/path/1.parquet")
#' }
#' @export
read_azure_parquet <- function(container, file, ...) {
  # using `dest = NULL` means pass the data through as a raw vector
  AzureStor::download_blob(container, file, dest = NULL) |>
    arrow::read_parquet(...)
}


#' Read a json file from Azure storage
#'
#' @inheritParams read_azure_parquet
#' @param ... optional arguments to be passed through to
#'  [yyjsonr::read_json_raw]
#' @returns A list
#' @export
read_azure_json <- function(container, file, ...) {
  # using `dest = NULL` means pass the data through as a raw vector
  AzureStor::download_blob(container, file, dest = NULL) |>
    yyjsonr::read_json_raw(...)
}


#' Read a json.gz file from Azure storage
#'
#' @inheritParams read_azure_parquet
#' @param ... optional arguments to be passed through to
#'  [yyjsonr::read_json_file]
#' @returns A list
#' @export
read_azure_jsongz <- function(container, file, ...) {
  dl <- withr::local_tempfile(
    pattern = tools::file_path_sans_ext(basename(file), TRUE),
    fileext = "json.gz"
  )
  AzureStor::download_blob(container, file, dest = dl)
  yyjsonr::read_json_file(dl, ...)
}


#' Read an rds file from Azure storage
#'
#' @inheritParams read_azure_parquet
#' @param ... optional arguments to be passed through to
#'  [AzureStor::storage_load_rds]. For example, a compression type (one of
#'  c("unknown", "gzip", "bzip2", "xz", "zstd", "none")) can be provided using
#'  the argument `type`, which will be passed on to [memDecompress] via
#'  [AzureStor::storage_load_rds].
#   If nothing is provided here, the compression type will be set to "none".
#' @returns The data object that was stored in the rds file
#' @export
read_azure_rds <- function(container, file, ...) {
  # If the user doesn't specify a (de)compression type with `type` in `...`, we
  # will set a `type` of "none", as this seems to be the standard on SU Azure
  dots <- rlang::dots_list(..., type = "none", .homonyms = "first")
  rlang::inject(AzureStor::storage_load_rds(container, file, !!!dots))
}


#' Read a csv file from Azure storage
#'
#' @inheritParams read_azure_parquet
#' @param ... optional arguments to be passed through to [readr::read_delim]
#' @returns A tibble
#' @export
read_azure_csv <- function(container, file, ...) {
  AzureStor::storage_read_csv(container, file, ...)
}


#' Read any file from Azure storage
#'
#' @inheritParams read_azure_parquet
#' @param ... optional arguments to be passed through to
#'  [AzureStor::download_blob]
#' @returns A raw data stream
#' @export
read_azure_file <- function(container, file, ...) {
  # using `dest = NULL` means pass the data through as a raw vector
  AzureStor::download_blob(container, file, dest = NULL, ...)
}
