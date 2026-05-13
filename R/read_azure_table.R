#' Read in data from an Azure table
#'
#' @param table_name Name of the table to be read.
#' @param table_endpoint An Azure table endpoint URL.
#' @inheritParams get_container
#' @param filter An OData filter string to filter the results.
#' @param select An OData select string to specify which properties to return.
#' @param top An integer specifying the maximum number of records to return.
#' @returns A tibble
#' @export
read_azure_table <- function(
  table_name,
  table_endpoint = Sys.getenv("AZ_TABLE_EP"),
  token = get_auth_token(),
  filter = NULL,
  select = NULL,
  top = NULL
) {
  base_req <- httr2::request(table_endpoint) |>
    httr2::req_url_path_append(table_name) |>
    httr2::req_auth_bearer_token(token$credentials$access_token) |>
    httr2::req_headers(
      "x-ms-version" = "2025-11-05",
      "Accept" = "application/json;odata=nometadata"
    )

  if (!is.null(filter)) {
    base_req <- httr2::req_url_query(base_req, `$filter` = filter)
  }
  if (!is.null(select)) {
    base_req <- httr2::req_url_query(base_req, `$select` = select)
  }
  if (!is.null(top)) {
    base_req <- httr2::req_url_query(base_req, `$top` = top)
  }

  responses <- httr2::req_perform_iterative(
    req = base_req,
    next_req = function(resp, req) {
      httr2::resp_check_status(resp)

      headers <- httr2::resp_headers(resp)

      pk <- headers[["x-ms-continuation-nextpartitionkey"]]
      rk <- headers[["x-ms-continuation-nextrowkey"]]

      # Stop when no continuation headers are present
      if ((is.null(pk) && is.null(rk)) || !is.null(top)) {
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
    purrr::list_rbind() |>
    tibble::as_tibble()
}
