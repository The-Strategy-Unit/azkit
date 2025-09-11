#' Get Azure authentication token
#'
#' This function retrieves an Azure token for a specified resource.
#'
#' If the environment variables `AZ_TENANT_ID`, `AZ_CLIENT_ID` and
#'  `AZ_APP_SECRET` are all set, it will try to use these to return a token.
#'
#' Otherwise it will try to get a managed token from a managed resource such as
#'  Azure VM or Azure App Service.
#'
#' If neither of these approaches has returned a token, it will try to retrieve
#'  a user token using the provided parameters, requiring the user to have
#'  authenticated using their device.
#'
#' @param resource A string specifying the URL of the Azure resource for which
#'  the token is requested. Defaults to `"https://storage.azure.com"`.
#' @param tenant A string specifying the Azure tenant. Defaults to
#'  `"organizations"`. See [AzureAuth::get_azure_token] for other values.
#' @param client_id A string specifying the application ID (client ID). If
#'  `NULL`, (the default) the function attempts to obtain the client ID from the
#'  Azure Resource Manager token, or prompts the user to log in to obtain it.
#' @param auth_method A string specifying the authentication method. Defaults to
#'  `"authorization_code"`. See ?[AzureAuth::get_azure_token] for other values.
#' @param force_refresh Boolean: whether to use a stored token if available
#'  (`FALSE`, the default), or try to obtain a new one from Azure (`TRUE`).
#'  This may be useful if you wish to generate a new token with the same
#'  `resource` value as an existing token, but a different `tenant` or
#'  `auth_method`.
#' @param ... Optional arguments (`token_args` or `use_cache`) to be passed on
#'  to [AzureAuth::get_managed_token].
#'
#' @returns An Azure token object
#' @examples
#' \dontrun{
#' # Get a token for the default resource
#' token <- get_auth_token()
#'
#' # Get a token for a specific resource and tenant
#' token <- get_auth_token(
#'  resource = "https://graph.microsoft.com",
#'  tenant = "my-tenant-id"
#' )
#'
#' # Get a token using a specific app ID
#' token <- get_auth_token(client_id = "my-app-id")
#' }
#' @export
get_auth_token <- function(
  resource = "https://storage.azure.com",
  tenant = "organizations",
  client_id = NULL,
  auth_method = "authorization_code",
  force_refresh = FALSE,
  ...
) {
  possibly_get_token <- \(...) purrr::possibly(AzureAuth::get_azure_token)(...)
  possibly_get_mtk <- \(...) purrr::possibly(AzureAuth::get_managed_token)(...)

  # 1. use environment variables if all three are set
  tenant_id_env <- Sys.getenv("AZ_TENANT_ID")
  client_id_env <- Sys.getenv("AZ_CLIENT_ID")
  client_secret <- Sys.getenv("AZ_APP_SECRET")

  if (all(nzchar(c(tenant_id_env, client_id_env, client_secret)))) {
    token <- possibly_get_token(
      resource = resource,
      tenant = tenant_id_env,
      app = client_id_env,
      password = client_secret
    )
  } else {
    # 2. try to get a managed token (for example on Azure VM, App Service)
    token <- possibly_get_mtk(resource, ...)
  }

  # 3. if neither of those has worked, try to get an already stored user token
  if (is.null(token)) {
    # list tokens already locally stored
    local_tokens <- AzureAuth::list_azure_tokens()
    if (length(local_tokens) > 0) {
      resources <- purrr::map(local_tokens, "resource")
      # if there are token(s) matching the `resource` argument then return one
      token_index <- match(resource, resources)[1]
      token <- if (!is.na(token_index)) local_tokens[[token_index]] else NULL
    }
    # 4. If we still don't have a valid token, or if `force_refresh` is on,
    # then we try to get one via user reauthentication.
    if (is.null(token) || force_refresh) {
      client_id <- client_id %||% get_client_id()
      token <- possibly_get_token(
        resource = resource,
        tenant = tenant,
        app = client_id,
        auth_type = auth_method
      )
    }
  }

  # Give some helpful feedback if process above has not worked
  if (is.null(token) || length(token) == 0) {
    cli::cli_alert_info("No authentication token was obtained.")
    cli::cli_alert_info("Please check any variables you have supplied.")
    cli::cli_alert_info(
      "Alternatively, running {.fn AzureRMR::get_azure_login} or
      {.fn AzureRMR::list_azure_tokens} may shed some light on the problem."
    )
    invisible(NULL)
  } else {
    token
  }
}

#' Sub-routine for `get_auth_token()`
#'
#' Pulled out mainly to tidy up the main function code a bit
#' @keywords internal
#' @returns A string (the client ID)
get_client_id <- function() {
  pluck_client_id <- function() {
    suppressMessages(AzureRMR::get_azure_login()) |>
      purrr::pluck("token", "client", "client_id")
  }
  possibly_pluck_client_id <- \(...) purrr::possibly(pluck_client_id)(...)
  azure_cli_default_client_id <- "04b07795-8ddb-461a-bbee-02f9e1bf7b46"

  client_id <- possibly_pluck_client_id()
  # if that fails, prompt the user to log in, then try again...
  if (is.null(client_id)) {
    AzureRMR::create_azure_login()
    # ...using the default Azure CLI client ID as a final fallback
    client_id <- possibly_pluck_client_id() %||% azure_cli_default_client_id
  }
  client_id
}
