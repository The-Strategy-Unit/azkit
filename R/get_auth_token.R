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
#'  authenticated using their device. If `force_refresh` is set to `TRUE`, a
#'  fresh web authentication process should be launched. Otherwise it will
#'  attempt to use a cached token matching the given `resource` and `tenant`.
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
#'  to [AzureAuth::get_managed_token] or [AzureAuth::get_azure_token].
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
  tenant = "common",
  client_id = NULL,
  auth_method = "authorization_code",
  force_refresh = FALSE,
  ...
) {
  possibly_get_token <- \(...) purrr::possibly(AzureAuth::get_azure_token)(...)
  possibly_get_mtk <- \(...) purrr::possibly(AzureAuth::get_managed_token)(...)

  dots <- rlang::list2(...)
  # if the user specifies force_refresh = TRUE we turn off `use_cache`,
  # otherwise we leave `use_cache` as it is (or as `NULL`, its default value)
  use_cached <- !force_refresh && (dots[["use_cache"]] %||% TRUE)
  dots <- rlang::dots_list(!!!dots, use_cache = use_cached, .homonyms = "last")

  # 1. Use environment variables if all three are set
  token_resp <- rlang::inject(try_token_from_vars(get_azure_token, !!!dots))
  token <- token_resp[["result"]]
  token_error <- token_resp[["error"]]

  # 2. Try to get a managed token (for example on Azure VM, App Service)
  if (is.null(token)) {
    token <- rlang::inject(possibly_get_mtk(resource, !!!dots))
  }

  # 3. If neither of those has worked, try to get an already stored user token
  #    (unless `force_refresh` is on, in which case skip to option 4)
  if (is.null(token) && use_cached) {
    token <- match_cached_token(resource, tenant, aad_version)
  }

  # 4. If we still don't have a token, try to get a new one via reauthentication
  if (is.null(token)) {
    if (!force_refresh) {
      cli::cli_alert_info("No matching cached token found: fetching new token")
    }
    client_id <- client_id %||% get_client_id()
    token <- rlang::inject(
      possibly_get_token(
        resource = resource,
        tenant = tenant,
        app = client_id,
        auth_type = auth_method,
        !!!dots
      )
    )
  }

  # Give some helpful feedback if process above has not worked
  if (is.null(token) || length(token) == 0) {
    cli::cli_alert_info("No authentication token was obtained.")
    cli::cli_alert_info("Please check any variables you have supplied.")
    cli::cli_alert_info(
      "Alternatively, running {.fn AzureRMR::get_azure_login} or
      {.fn AzureRMR::list_azure_tokens} may shed some light on the problem."
    )
#' Get token via app and secret environment variables
#' Sub-routine for `get_auth_token()`
#' @keywords internal
#' @returns A list with elements `result` and `error`. If this method is
#'  successful, the `result` element will contain a token.
try_token_from_vars <- function(get_token_fun, ...) {
  tenant_id_env <- Sys.getenv("AZ_TENANT_ID")
  client_id_env <- Sys.getenv("AZ_CLIENT_ID")
  client_secret <- Sys.getenv("AZ_APP_SECRET")

  if (all(nzchar(c(tenant_id_env, client_id_env, client_secret)))) {
    rlang::inject(
      get_token_fun(
        tenant = tenant_id_env,
        app = client_id_env,
        password = client_secret,
        ...
      )
    )
  } else {
    list(result = NULL, error = NULL)
  }
}


#' Find an already cached token that matches desired parameters
#' Sub-routine for `get_auth_token()`
#' @keywords internal
#' @returns A token from local cache, or NULL if none matches
match_cached_token <- function(resource, tenant, aad_version) {
  # list tokens already locally cached
  local_tokens <- AzureAuth::list_azure_tokens()
  if (length(local_tokens) > 0) {
    resources <- purrr::map(local_tokens, "resource")
    scopes <- purrr::map(local_tokens, list("scope", 1))
    resources <- purrr::map2_chr(resources, scopes, `%||%`)
    tenants <- purrr::map_chr(local_tokens, "tenant")
    versions <- purrr::map_int(local_tokens, "version")
    resource_index <- gregg(resources, "^{resource[[1]]}")
    tenant_index <- tenants == tenant
    version_index <- versions == aad_version
    # return a token matching `resource`, `tenant` and `version`, if any
    token_index <- which(resource_index & tenant_index & version_index)[1]
    if (!is.na(token_index)) local_tokens[[token_index]] else NULL
  } else {
    NULL
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
