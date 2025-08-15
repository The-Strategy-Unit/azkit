#' Common routine for all `read_azure_*()` functions
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
  stopifnot(stop_msg1 = length(filepath) > 0)
  stopifnot(stop_msg2 = length(filepath) == 1)
  info_option <- getOption("azkit.info")
  stopifnot(rlang::is_scalar_logical(info_option) || is.null(info_option))
  if (info %||% info_option %||% rlang::is_interactive()) {
    cli::cli_alert_info("File {.val {filepath}} will be read in")
  }
  AzureStor::download_blob(container, filepath, dest = NULL)
}
