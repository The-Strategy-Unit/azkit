#' Get Azure storage container
#'
#' `r lifecycle::badge("experimental")`
#' Use `list_container_names()` to see a list of available containers
#'
#' @param container_name Name of the container as a string. NULL by default,
#'  which means the function will look instead for a container name stored in
#'  the environment variable "AZ_CONTAINER"
#' @param ... arguments to be passed through to `get_auth_token()`
#' @returns An Azure blob container (list object of class "blob_container")
#' @export
get_container <- function(container_name = NULL, ...) {
  container_envvar_name <- "AZ_CONTAINER"
  cst_msg1 <- cst_error_msg("{.var container_name} must be a string")
  cst_msg2 <- cst_error_msg("{.envvar {container_envvar_name}} is not set")
  c_name <- (container_name %||% Sys.getenv(container_envvar_name, NA)) |>
    check_scalar_type("character", cst_msg1) |>
    check_scalar_type("string", cst_msg2)
  token <- get_auth_token(...)
  endpoint <- get_default_endpoint(token)
  container_names <- list_container_names(token)
  not_found_msg <- cv_error_msg("Container {.val {container_name}} not found")
  container_name |>
    check_vec(\(x) x %in% container_names, not_found_msg) |>
    AzureStor::blob_container(endpoint = endpoint)
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


#' Check that an environment variable exists
#'
#' The function prints a helpful error if the variable is not found, else
#' it returns the value of `Sys.getenv(x)`
#'
#' @param x the name of the environment variable to be found and checked
#' @returns the value of the environment variable `x`
#' @export
check_envvar <- function(x) {
  cst_msg <- cst_error_msg("{.envvar {x}} is not set")
  check_scalar_type(Sys.getenv(x, NA_character_), "string", cst_msg)
}


#' Return an Azure "blob_endpoint"
#'
#' @param token An Azure authentication token
#' @returns An Azure blob endpoint (object of class "blob_endpoint")
#' @keywords internal
get_default_endpoint <- function(token) {
  check_envvar("AZ_STORAGE_EP") |>
    AzureStor::blob_endpoint(token = token)
}
