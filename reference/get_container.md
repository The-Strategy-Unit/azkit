# Get Azure storage container

It may be helpful to set the environment variable "AZ_STORAGE_EP". This
can contain your usual Azure storage endpoint URL should you not wish to
pass it in explicitly to the function. You may find it helpful to use
[list_container_names](https://the-strategy-unit.github.io/azkit/reference/list_container_names.md)
to get a list of available container names.

## Usage

``` r
get_container(
  container_name,
  endpoint_url = Sys.getenv("AZ_STORAGE_EP"),
  token = get_auth_token()
)
```

## Arguments

- container_name:

  Name of the container as a string.

- endpoint_url:

  An Azure endpoint URL.

- token:

  An Azure authentication token, or a function that returns one. Uses
  [get_auth_token](https://the-strategy-unit.github.io/azkit/reference/get_auth_token.md)
  by default.

## Value

An Azure blob container (list object of class "blob_container")
