#' List files in a container
#'
#' Lists all files (recursively, if desired) found in a container within a
#' given directory (`dir`). The search can be restricted to files with a
#' specific extension.
#'
#' The function does not support filtering by file name, only by file extension.
#'
#' The returned file list (character vector) contains the full paths to the
#' files, ready to be passed perhaps to a `read_azure_*` function, or filtered
#' further. If you just want the names of the files without the folder path,
#' use [basename] to extract these.
#'
#' @inheritParams read_azure_parquet
#' @param dir (optional) The directory of the container to list files within.
#'  `""` (the root directory of the container) by default
#' @param ext (optional) A string giving the extension of a particular file type
#'  to restrict the list to. No need to include the initial ".". The default,
#'  `""`, means no filtering by file extension will be applied.
#' @param recursive logical: whether to list files recursively. Default `FALSE`
#'
#' @importFrom rlang .data
#' @returns A vector of file names, or an empty character vector if none found
#' @examples \dontrun{
#'   list_files(get_container("example"), ext = "csv")
#' }
#' @export
list_files <- function(container, dir = "", ext = "", recursive = FALSE) {
  stopifnot(rlang::is_character(c(dir, ext), 2))
  stopifnot(rlang::is_bool(recursive))
  pnf_msg <- ct_error_msg("Path {.val {path}} not found")
  check_that(dir, \(x) AzureStor::blob_dir_exists(container, x), pnf_msg)

  ext_rx <- ifelse(nzchar(ext), gsub("^\\.+", "\\.", ext), ".*") # nolint
  tbl <- AzureStor::list_blobs(container, dir, recursive = recursive) |>
    dplyr::filter(!.data[["isdir"]] & gregg(.data[["name"]], "{ext_rx}$"))

  # A zero-row tbl can result if the directory is actually empty, or via
  # filtering out. We handle this the same way no matter which route led here.
  if (nrow(tbl) == 0) {
    fix_path <- \(p) sub("^/+$", "", sub("^([^/])(.*)", "/\\1\\2", p)) # nolint
    ext <- if (nzchar(ext)) paste0(" ", ext)
    msg <- "No{ext} files found in {.val [{container$name}]:{fix_path(path)}}"
    cli::cli_alert_info(msg)
    invisible(character(0))
  } else {
    tbl[["name"]]
  }
}
