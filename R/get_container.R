#' Get Azure storage container
#'
#' `r lifecycle::badge("experimental")`
#' @param container_name string: name of the container
#' @param ... arguments to be passed through to `get_auth_token()`
#' @export
get_container <- function(container_name, ...) {
  endpoint_uri <- Sys.getenv("AZ_STORAGE_EP")
  token <- get_auth_token(...)
  endpoint <- get_default_endpoint(token)
  container_names <- list_container_names(token)
#' Return an Azure "blob_endpoint"
#'
#' @param token An Azure authentication token
#' @returns An Azure blob endpoint (object of class "blob_endpoint")
#' @keywords internal
get_default_endpoint <- function(token) {
  cst_msg <- cst_error_msg("{.envvar AZ_STORAGE_EP} is not set")
  Sys.getenv("AZ_STORAGE_EP", NA) |>
    check_scalar_type("string", cst_msg) |>
    AzureStor::blob_endpoint(token = token)
}


#' Return a list of container names that are found at the default endpoint
#'
#' @inheritParams get_container
#' @inheritParams get_default_endpoint
#' @returns A character vector of all container names found
#' @export
list_container_names <- function(token = NULL, ...) {
  token <- token %||% get_auth_token(...)
  endpoint <- get_default_endpoint(token)
  names(AzureStor::list_blob_containers(endpoint))
}
