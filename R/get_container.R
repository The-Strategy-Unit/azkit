#' Get Azure storage container
#'
#' It may be helpful to set the environment variable "AZ_STORAGE_EP". This
#'  can contain your usual Azure storage endpoint URL should you not wish to
#'  pass it in explicitly to the function.
#' You may find it helpful to use [list_container_names] to get a list of
#'  available container names.
#'
#' @param container_name Name of the container as a string.
#' @param endpoint_url An Azure endpoint URL.
#' @param token An Azure authentication token, or a function that returns one.
#'  Uses [get_auth_token] by default.
#' @returns An Azure blob container (list object of class "blob_container")
#' @export
get_container <- function(
  container_name,
  endpoint_url = Sys.getenv("AZ_STORAGE_EP"),
  token = get_auth_token()
) {
  AzureStor::blob_endpoint(endpoint_url, token = token) |>
    AzureStor::blob_container(container_name)
}


#' Return a list of container names that are found at the endpoint
#'
#' @inheritParams get_container
#' @returns A character vector of all container names found
#' @export
list_container_names <- function(
  endpoint_url = Sys.getenv("AZ_STORAGE_EP"),
  token = get_auth_token()
) {
  container_list <- AzureStor::blob_endpoint(endpoint_url, token = token) |>
    AzureStor::list_blob_containers()
  if (length(container_list) == 0) {
    cli::cli_alert_info("No containers found")
    character(0)
  } else {
    names(container_list)
  }
}
