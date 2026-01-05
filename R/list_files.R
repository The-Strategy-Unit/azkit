#' List files in a container
#'
#' Recursively (or not, if desired) lists all files found in a container. Search
#' can be restricted to a particular 'subdirectory' of the container, and/or
#' to files with a specific extension. The function assumes that all file names
#' end with a ".ext" extension of some sort.
#'
#' The function does not support filtering by file name, only by file extension.
#'
#' The returned file list (character vector) contains the full paths to the
#' files, ready to be passed perhaps to a `read_azure_*` function, or further
#' filtered by you. If you just want the names of the files without the folder
#' path, use [basename()] to extract these.
#'
#' @inheritParams read_azure_parquet
#' @param path (optional) subdirectory of the container to list files within.
#'  `""` (the root folder of the container) by default
#' @param ext (optional) A string giving the extension of a particular file type
#'  you want to restrict the list to. No need to include the initial ".". The
#'  default, `""`, means no filtering by file extension will be applied. Can be
#'  a regular expression.
#' @param recursive A Boolean value: whether to list files recursively. `TRUE`
#'  by default
#'
#' @importFrom rlang .data
#' @returns A vector of file names, or an empty character vector if none found
#' @examples \dontrun{
#'   list_files(get_container("example"), ext = "csv")
#' }
#' @export
list_files <- function(container, path = "", ext = "", recursive = TRUE) {
  stopifnot(rlang::is_character(c(path, ext), 2))
  stopifnot(rlang::is_bool(recursive))
  pnf_msg <- ct_error_msg("Path {.val {path}} not found")
  check_that(path, \(x) AzureStor::blob_dir_exists(container, x), pnf_msg)

  tbl <- AzureStor::list_blobs(container, path, recursive = recursive)
  if (nrow(tbl) > 0) {
    ext_rx <- if (nzchar(ext)) sub("^\\.+", "", ext) else ".*" # nolint
    tbl <- tbl |>
      dplyr::filter(!.data[["isdir"]] & gregg(.data[["name"]], "\\.{ext_rx}$"))
  }

  # A zero-row tbl can result if `path` is initially empty, or via the filter
  # step above. We handle this the same way, no matter which route led here.
  if (nrow(tbl) == 0) {
    fix_path <- \(p) sub("^/+$", "", sub("^([^/])(.*)", "/\\1\\2", p)) # nolint
    ext <- if (nzchar(ext)) paste0(" ", ext)
    msg <- "No{ext} files found in {.val [{container$name}]:{fix_path(path)}}"
    if (rlang::is_interactive()) {
      cli::cli_alert_info(msg)
    }
    invisible(character(0))
  } else {
    tbl[["name"]]
  }
}
