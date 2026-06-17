# Return a list of container names that are found at the endpoint

Return a list of container names that are found at the endpoint

## Usage

``` r
list_container_names(
  endpoint_url = Sys.getenv("AZ_STORAGE_EP"),
  token = get_auth_token()
)
```

## Arguments

- endpoint_url:

  An Azure endpoint URL.

- token:

  An Azure authentication token, or a function that returns one. Uses
  [get_auth_token](https://the-strategy-unit.github.io/azkit/reference/get_auth_token.md)
  by default.

## Value

A character vector of all container names found
