# Read in data from an Azure table

Read in data from an Azure table

## Usage

``` r
read_azure_table(
  table_name,
  table_endpoint = Sys.getenv("AZ_TABLE_EP"),
  token = get_auth_token(),
  filter = NULL,
  select = NULL,
  top = NULL
)
```

## Arguments

- table_name:

  Name of the table to be read.

- table_endpoint:

  An Azure table endpoint URL.

- token:

  An Azure authentication token, or a function that returns one. Uses
  [get_auth_token](https://the-strategy-unit.github.io/azkit/reference/get_auth_token.md)
  by default.

- filter:

  An OData filter string to filter the results.

- select:

  An OData select string to specify which properties to return.

- top:

  An integer specifying the maximum number of records to return.

## Value

A tibble

## Examples

``` r
if (FALSE) { # \dontrun{
# Read all rows from a table
read_azure_table(
  "my_table",
  table_endpoint = "https://myaccount.table.core.windows.net/"
)

# Filter rows using an OData filter string
read_azure_table(
  "my_table",
  table_endpoint = "https://myaccount.table.core.windows.net/",
  filter = "PartitionKey eq 'my-partition'"
)

# Filter, select specific properties, and limit the number of rows returned
read_azure_table(
  "my_table",
  table_endpoint = "https://myaccount.table.core.windows.net/",
  filter = "Status eq 'Active' and Score ge 10",
  select = "PartitionKey,RowKey,Status,Score",
  top = 100
)
} # }
```
