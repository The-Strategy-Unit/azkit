#' Get Azure storage container
#'
#' The environment variable "AZ_STORAGE_EP" must be set. This provides the URL
#'  for the default Azure storage endpoint.
#' Use [list_container_names] to get a list of available container names.
#'
#' @param container_name Name of the container as a string. `NULL` by default,
#'  which means the function will look instead for a container name stored in
#'  the environment variable "AZ_CONTAINER"
#' @param ... arguments to be passed through to [get_auth_token]
#' @returns An Azure blob container (list object of class "blob_container")
#' @export
get_container <- function(container_name = NULL, ...) {
  msg <- glue::glue(
    "{.var container_name} is empty. ",
    "Did you forget to set an environment variable?"
  )
  cont_nm <- check_nzchar(container_name, msg) %||% check_envvar("AZ_CONTAINER")
  token <- get_auth_token(...)
  endpoint <- get_default_endpoint(token)
  container_names <- list_container_names(token)
  not_found_msg <- cv_error_msg("Container {.val {cont_nm}} not found")
  cont_nm |>
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
  lcn <- "list_container_names"
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
