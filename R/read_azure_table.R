#' Read in data from an Azure table
#'
#' @param table_name name of the table to be read. If left as `NULL`,
#'  the default, the function will look instead for a value stored in the
#'  environment variable "AZ_TABLE_NAME"
#' @param table_endpoint URL of the Azure table endpoint. If left as `NULL`,
#'  the default, the function will look instead for a value stored in the
#'  environment variable "AZ_TABLE_EP"
#' @param ... parameters to be passed through to [get_auth_token]
#' @returns A tibble
#' @export
read_azure_table <- function(table_name = NULL, table_endpoint = NULL, ...) {
  table_name <- table_name %||% check_envvar("AZ_TABLE_NAME")
  table_ep <- table_endpoint %||% check_envvar("AZ_TABLE_EP")
  access_token <- get_auth_token(...) |>
    purrr::pluck("credentials", "access_token")
  headers <- list("2025-11-05", "application/json;odata=nometadata") |>
    purrr::set_names(c("x-ms-version", "Accept"))

  resp <- httr2::request(table_ep) |>
    httr2::req_url_path_append(table_name) |>
    httr2::req_auth_bearer_token(access_token) |>
    httr2::req_headers(!!!headers) |>
    httr2::req_perform() |>
    httr2::resp_check_status()

  resp |>
    httr2::resp_body_json() |>
    purrr::pluck("value") |>
    purrr::map(tibble::as_tibble) |>
    purrr::list_rbind()
}
