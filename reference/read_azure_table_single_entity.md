# Read in data from an Azure table

Read in data from an Azure table

## Usage

``` r
read_azure_table_single_entity(
  table_name,
  partition_key,
  row_key,
  table_endpoint = Sys.getenv("AZ_TABLE_EP"),
  token = get_auth_token()
)
```

## Arguments

- table_name:

  Name of the table to be read.

- partition_key:

  The partition key of the entity to be read.

- row_key:

  The row key of the entity to be read.

- table_endpoint:

  An Azure table endpoint URL.

- token:

  An Azure authentication token, or a function that returns one. Uses
  [get_auth_token](https://the-strategy-unit.github.io/azkit/reference/get_auth_token.md)
  by default.

## Value

A tibble
