#' Get Azure storage container
#'
#' The environment variable "AZ_STORAGE_EP" should be set. This provides the URL
#'  for the default Azure storage endpoint.
#' Use [list_container_names] to get a list of available container names.
#'
#' @param container_name Name of the container as a string. `NULL` by default,
#'  which means the function will look instead for a container name stored in
#'  the environment variable "AZ_CONTAINER"
#' @param token An Azure authentication token. If left as `NULL`, a token
#'  returned by [get_auth_token] will be used
#' @param endpoint_url An Azure endpoint URL. If left as `NULL`, the default,
#'  the value of the environment variable "AZ_STORAGE_EP" will be used
#' @param ... arguments to be passed through to [get_auth_token], if a token is
#'  not already supplied
#' @returns An Azure blob container (list object of class "blob_container")
#' @export
get_container <- function(
  container_name = NULL,
  token = NULL,
  endpoint_url = NULL,
  ...
) {
  msg1 <- paste0(
    "{.var container_name} is empty. ",
    "Did you forget to set an environment variable?"
  )
  msg2 <- paste0(
    "{.var endpoint_url} is empty. ",
    "Did you forget to set an environment variable?"
  )
  container_name <- (container_name %||% check_envvar("AZ_CONTAINER")) |>
    check_nzchar(msg1)
  endpoint_url <- (endpoint_url %||% check_envvar("AZ_STORAGE_EP")) |>
    check_nzchar(msg2)
  token <- token %||% get_auth_token(...)

  get_azure_endpoint(token, endpoint_url) |>
    AzureStor::blob_container(container_name)
}


#' Return a list of container names that are found at the endpoint
#'
#' @inheritParams get_container
#' @returns A character vector of all container names found
#' @export
list_container_names <- function(token = NULL, endpoint_url = NULL, ...) {
  token <- token %||% get_auth_token(...)
  endpoint <- get_azure_endpoint(token, endpoint_url)
  container_list <- AzureStor::list_blob_containers(endpoint)
  stopifnot("no containers found" = length(container_list) >= 1L)
  names(container_list)
}


#' Return an Azure "blob_endpoint"
#'
#' This function will return the endpoint specified in the environment variable
#' "AZ_STORAGE_EP" by default
#'
#' @inheritParams get_container
#' @returns An Azure blob endpoint (object of class "blob_endpoint")
#' @keywords internal
get_azure_endpoint <- function(token = NULL, endpoint_url = NULL, ...) {
  token <- token %||% get_auth_token(...)
  endpoint_url <- endpoint_url %||% check_envvar("AZ_STORAGE_EP")
  AzureStor::blob_endpoint(endpoint_url, token = token)
}


#' Check that an environment variable exists
#'
#' The function prints a helpful error if the variable is not found, else
#'  it returns the value of `Sys.getenv(x)`
#'
#' @param x the *name* of the environment variable to be found and checked
#' @returns the value of the environment variable named in `x`
#' @export
check_envvar <- function(x) {
  cst_msg <- cst_error_msg("The environment variable {.envvar {x}} is not set")
  check_scalar_type(Sys.getenv(x, NA_character_), "string", cst_msg)
}
