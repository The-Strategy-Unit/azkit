# Get Azure authentication token

This function retrieves an Azure token for a specified resource.

This method avoids the need to refresh by re-authenticating online. It
seems that this only works with v1 tokens. (v2 tokens always seem to
refresh via online re-authentication, but they *ought* to refresh
automatically.) To instead generate a completely fresh token, set
`force_refresh = TRUE` in get_auth_token

## Usage

``` r
get_auth_token(
  resource = generate_resource(),
  tenant = "common",
  client_id = NULL,
  auth_method = "authorization_code",
  aad_version = 1,
  force_refresh = FALSE,
  ...
)

refresh_token(token)
```

## Arguments

- resource:

  For v1, a simple URL such as `"https://storage.azure.com/"` should be
  supplied. For v2, a vector specifying the URL of the Azure resource
  for which the token is requested as well as any desired scopes. See
  [AzureAuth::get_azure_token](https://rdrr.io/pkg/AzureAuth/man/get_azure_token.html)
  for details. Use
  [generate_resource](https://the-strategy-unit.github.io/azkit/reference/generate_resource.md)
  to help provide an appropriate string or vector. If setting version to
  2, ensure that the `aad_version` argument is also set to 2. Both are
  set to use AAD version 1 by default.

- tenant:

  A string specifying the Azure tenant. Defaults to `"common"`. See
  [AzureAuth::get_azure_token](https://rdrr.io/pkg/AzureAuth/man/get_azure_token.html)
  for other values.

- client_id:

  A string specifying the application ID (aka client ID). If `NULL`,
  (the default) the function attempts to obtain the client ID from the
  Azure Resource Manager token, or prompts the user to log in to obtain
  it.

- auth_method:

  A string specifying the authentication method. Defaults to
  `"authorization_code"`. To use a secret, pass `"client_credentials"`
  instead and provide the secret using the `password` argument in `...`.
  See
  [AzureAuth::get_azure_token](https://rdrr.io/pkg/AzureAuth/man/get_azure_token.html)
  for more information.

- aad_version:

  Numeric. The AAD version, either 1 or 2 (1 by default)

- force_refresh:

  logical. Whether to use a stored token if available (`FALSE`, the
  default), or try to obtain a new one from Azure (`TRUE`). This may be
  useful if you wish to generate a new token with the same `resource`
  value as an existing token, but a different `tenant` or `auth_method`.
  Note that you can also try refresh_token, which should cause an
  existing token to refresh itself, without obtaining a new token from
  Azure via online reauthentication

- ...:

  Optional arguments (eg `token_args` or `use_cache`) to be passed on to
  [AzureAuth::get_managed_token](https://rdrr.io/pkg/AzureAuth/man/get_azure_token.html)
  or
  [AzureAuth::get_azure_token](https://rdrr.io/pkg/AzureAuth/man/get_azure_token.html),
  for example to overwrite any opf their default values or to supply a
  `password`

- token:

  An Azure authentication token

## Value

An Azure token object

An Azure authentication token

## Details

It will try to get a managed token when used within a managed resource
such as Azure VM or Azure App Service.

If this method does not return a token, it will try to retrieve a user
token using the provided parameters, requiring the user to have
authenticated using their device. If `force_refresh` is set to `TRUE`, a
fresh web authentication process should be launched. Otherwise it will
attempt to use a cached token matching the given `resource`, `tenant`
and `aad_version`.

## Examples

``` r
if (FALSE) { # \dontrun{
# Get a token for the default resource
token <- get_auth_token()

# Force generation of a new token via online reauthentication
token <- get_auth_token(force_refresh = TRUE)

# Get a token for a specific resource and tenant
token <- get_auth_token(
  resource = "https://graph.microsoft.com",
  tenant = "my-tenant-id"
)

# Get a token using a specific app ID
token <- get_auth_token(client_id = "my-app-id")

# Use a secret
token <- get_auth_token(
 tenant = "my-tenant-id",
 client_id = "my-app-id",
 auth_method = "client_credentials",
 password = "123459878&%^"
)
} # }
```
