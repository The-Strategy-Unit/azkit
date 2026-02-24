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
#'  attempt to use a cached token matching the given `resource`, `tenant` and
#'  `aad_version`.
#'
#' @param resource For v1, a simple URL such as `"https://storage.azure.com/"`
#'  should be supplied.For v2, a vector specifying the URL of the Azure resource
#'  for which the token is requested as well as any desired scopes. See
#'  [AzureAuth::get_azure_token] for details. Use [generate_resource]
#'  to help provide an appropriate string or vector. The values default to
#'  `c("https://storage.azure.com/.default", "openid", "offline_access")`.
#'  If setting version to 2, ensure that the `aad_version` argument is also set
#'  to 2. Both are set to use AAD version 1 by default.
#' @param tenant A string specifying the Azure tenant. Defaults to
#'  `"common"`. See [AzureAuth::get_azure_token] for other values.
#' @param client_id A string specifying the application ID (client ID). If
#'  `NULL`, (the default) the function attempts to obtain the client ID from the
#'  Azure Resource Manager token, or prompts the user to log in to obtain it.
#' @param auth_method A string specifying the authentication method. Defaults to
#'  `"authorization_code"`. See [AzureAuth::get_azure_token] for other values.
#' @param aad_version Numeric. The AAD version, either 1 or 2 (1 by default)
#' @param force_refresh Boolean: whether to use a stored token if available
#'  (`FALSE`, the default), or try to obtain a new one from Azure (`TRUE`).
#'  This may be useful if you wish to generate a new token with the same
#'  `resource` value as an existing token, but a different `tenant` or
#'  `auth_method`. Note that you can also try using [refresh_token] which will
#'  cause an existing token to refresh itself, without obtaining a new token
#'  from Azure via online reauthentication
#' @param ... Optional arguments (`token_args` or `use_cache`) to be passed on
#'  to [AzureAuth::get_managed_token] or [AzureAuth::get_azure_token].
#'
#' @returns An Azure token object
#' @examples
#' \dontrun{
#' # Get a token for the default resource
#' token <- get_auth_token()
#'
#' # Force generation of a new token via online reauthentication
#' token <- get_auth_token(force_refresh = TRUE)
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
  resource = generate_resource(),
  tenant = "common",
  client_id = NULL,
  auth_method = "authorization_code",
  aad_version = 1,
  force_refresh = FALSE,
  ...
) {
  aad_msg <- "Invalid {.arg aad_version} variable supplied (must be 1 or 2)"
  aad_version <- check_that(aad_version, \(x) x %in% seq(2), aad_msg)

  safely_get_token <- \(...) purrr::safely(AzureAuth::get_azure_token)(...)
  get_azure_token <- purrr::partial(
    safely_get_token,
    resource = resource,
    version = aad_version
  )
  possibly_get_mtk <- \(...) purrr::possibly(AzureAuth::get_managed_token)(...)

  dots <- rlang::list2(...)
  # If the user specifies force_refresh = TRUE we turn off `use_cache`,
  # otherwise we leave `use_cache` as it is (or as `NULL`, its default value)
  use_cached <- !force_refresh && (dots[["use_cache"]] %||% TRUE)
  dots <- rlang::dots_list(!!!dots, use_cache = use_cached, .homonyms = "last")

  # We have 4 approaches to get a token, depending on the context
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
    token_resp <- rlang::inject(
      get_azure_token(
        tenant = tenant,
        app = client_id,
        auth_type = auth_method,
        !!!dots
      )
    )
    token <- token_resp[["result"]]
    token_error <- token_error %||% token_resp[["error"]]
  }

  # Give some helpful feedback if the steps above have not succeeded
  if (is.null(token) || length(token) == 0) {
    cli::cli_alert_info("No authentication token was obtained.")
    cli::cli_alert_info("Please check any variables you have supplied.")
    cli::cli_alert_info(
      "Alternatively, running {.fn AzureRMR::get_azure_login} or
      {.fn AzureRMR::list_azure_tokens} may shed some light on the problem."
    )
    error_msg <- "{.fn get_auth_token}: No authentication token was obtained."
    cli::cli_abort(as.character(token_error %||% error_msg))
  } else {
    if (aad_version == 2) {
      check_that(token, AzureAuth::is_azure_v2_token, "Invalid token returned")
    } else {
      check_that(token, AzureAuth::is_azure_v1_token, "Invalid token returned")
    }
  }
}


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


#' Generate appropriate values for the `resource` parameter in [get_auth_token]
#'
#' A helper function to generate appropriate values. Ensure that the `version`
#'  argument matches the `aad_version` argument to [get_auth_token].
#'  It's unlikely that you will ever want to set `authorise` to `FALSE` but it's
#'  here as an option since [AzureAuth::get_azure_token] supports it. Similarly,
#'  you are likely to want to keep `refresh` turned on (this argument has no
#'  effect on v1 tokens, it only applies to v2).
#'
#' @param version numeric. The AAD version, either 1 or 2 (1 by default)
#' @param url The URL of the Azure resource host
#' @param path For v2, the path designating the access scope
#' @param authorise Boolean, whether to return a token with authorisation scope,
#'  (TRUE, the default) or one that just provides authentication. You are
#'  unlikely to want to turn this off
#' @param refresh Boolean, applies to v2 tokens only, whether to return a token
#'  that has a refresh token also supplied.
#' @returns A scalar character, or (in most v2 situations) a character vector
#' @export
generate_resource <- function(
  version = 1,
  url = "https://storage.azure.com",
  path = "/.default",
  authorise = TRUE,
  refresh = TRUE
) {
  stopifnot("version must be 1 or 2" = version %in% seq(2))
  scopes <- if (refresh) c("openid", "offline_access") else "openid"
  if (authorise) {
    if (version == 2) {
      c(paste0(url, path), scopes)
    } else {
      url
    }
  } else {
    if (version == 2) {
      scopes
    } else {
      ""
    }
  }
}


#' Use a token's internal `refresh()` method to refresh it
#'
#' This method avoids the need to refresh by re-authenticating online. It seems
#'  like this only works with v1 tokens. v2 tokens always seem to refresh by
#'  re-authenticating with Azure online. But v2 tokens _ought_ to refresh
#'  automatically and not need manual refreshing. To instead generate a
#'  completely fresh token, pass `use_cache = FALSE` or `force_refresh = TRUE`
#'  to [get_auth_token].
#' @param token An Azure authentication token
#' @returns An Azure authentication token
#' @export
refresh_token <- \(token) token$refresh()
