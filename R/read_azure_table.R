#' Read in data from an Azure table
#'
#' @param table_name Name of the table to be read.
#' @param table_endpoint An Azure table endpoint URL.
#' @inheritParams get_container
#' @returns A tibble
#' @export
read_azure_table <- function(
  table_name,
  table_endpoint = Sys.getenv("AZ_TABLE_EP"),
  token = get_auth_token()
) {
  access_token <- token |>
    purrr::pluck("credentials", "access_token")
  headers <- list("2025-11-05", "application/json;odata=nometadata") |>
    purrr::set_names(c("x-ms-version", "Accept"))

  resp <- httr2::request(table_endpoint) |>
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
