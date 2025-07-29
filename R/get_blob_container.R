#' Get Azure storage blob container
#'
#' `r lifecycle::badge("experimental")`
#' @param container_name string: name of the container
#' @param ... arguments to be passed through to `get_azure_token()`
get_blob_container <- function(container_name, ...) {
  endpoint_uri <- Sys.getenv("AZ_STORAGE_EP")
  token <- get_azure_token(...)
  err_msg <- "{.fn check_val}: {.var AZ_STORAGE_EP} env var is not set"
  endpoint_uri |>
    check_val(nzchar, err_msg) |>
    AzureStor::blob_endpoint(token = token) |>
    AzureStor::storage_container(container_name)
}
