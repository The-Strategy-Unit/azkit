# Generate appropriate values for the `resource` parameter in [get_auth_token](https://the-strategy-unit.github.io/azkit/reference/get_auth_token.md)

A helper function to generate appropriate values. Ensure that the
`version` argument matches the `aad_version` argument to
[get_auth_token](https://the-strategy-unit.github.io/azkit/reference/get_auth_token.md).
It's unlikely that you will ever want to set `authorise` to `FALSE` but
it's here as an option since
[AzureAuth::get_azure_token](https://rdrr.io/pkg/AzureAuth/man/get_azure_token.html)
supports it. Similarly, you are likely to want to keep `refresh` turned
on (this argument has no effect on v1 tokens, it only applies to v2).

## Usage

``` r
generate_resource(
  version = 1,
  url = "https://storage.azure.com",
  path = "/.default",
  authorise = TRUE,
  refresh = TRUE
)
```

## Arguments

- version:

  numeric. The AAD version, either 1 or 2 (1 by default)

- url:

  The URL of the Azure resource host

- path:

  For v2, the path designating the access scope

- authorise:

  Boolean, whether to return a token with authorisation scope, (TRUE,
  the default) or one that just provides authentication. You are
  unlikely to want to turn this off

- refresh:

  Boolean, applies to v2 tokens only, whether to return a token that has
  a refresh token also supplied.

## Value

A scalar character, or (in most v2 situations) a character vector
