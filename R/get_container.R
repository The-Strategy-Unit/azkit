#' Get Azure storage container
#'
#' Use [list_container_names()] to see a list of available containers
#'
#' @param container_name Name of the container as a string. `NULL` by default,
#'  which means the function will look instead for a container name stored in
#'  the environment variable "AZ_CONTAINER"
#' @param ... arguments to be passed through to [get_auth_token()]
#' @returns An Azure blob container (list object of class "blob_container")
#' @export
get_container <- function(container_name = NULL, ...) {
  cst_msg <- cst_error_msg("{.var container_name} must be a string")
  container_name <- (container_name %||% check_envvar("AZ_CONTAINER")) |>
    check_scalar_type("character", cst_msg)
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
  lcn <- "list_container_names" # nolint
  container_list <- AzureStor::list_blob_containers(endpoint) |>
    rlang::try_fetch(error = \(e) cli::cli_abort("Error in {.fn {lcn}}: {e}"))
  stopifnot("no containers found" = length(container_list) >= 1L)
  names(container_list)
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


#' Check that an environment variable exists
#'
#' The function prints a helpful error if the variable is not found, else
#'  it returns the value of `Sys.getenv(x)`
#'
#' @param x the *name* of the environment variable to be found and checked
#' @returns the value of the environment variable named in `x`
#' @export
check_envvar <- function(x) {
  cst_msg <- cst_error_msg("{.envvar {x}} is not set")
  check_scalar_type(Sys.getenv(x, NA_character_), "string", cst_msg)
}
