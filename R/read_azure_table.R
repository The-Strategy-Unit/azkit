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
  base_req <- httr2::request(table_endpoint) |>
    httr2::req_url_path_append(table_name) |>
    httr2::req_auth_bearer_token(token$credentials$access_token) |>
    httr2::req_headers(
      "x-ms-version" = "2025-11-05",
      "Accept" = "application/json;odata=nometadata"
    )

  responses <- httr2::req_perform_iterative(
    req = base_req,
    next_req = function(resp, req) {
      headers <- httr2::resp_headers(resp)

      pk <- headers[["x-ms-continuation-nextpartitionkey"]]
      rk <- headers[["x-ms-continuation-nextrowkey"]]

      # Stop when no continuation headers are present
      if (is.null(pk) && is.null(rk)) {
        return(NULL)
      }

      httr2::req_url_query(
        req,
        NextPartitionKey = pk,
        NextRowKey = rk
      )
    }
  )

  responses |>
    purrr::map(httr2::resp_body_json, simplifyVector = TRUE) |>
    purrr::map("value") |>
    purrr::list_flatten() |>
    purrr::map(tibble::as_tibble_row) |>
    purrr::list_rbind() |>
    tibble::as_tibble()
}
