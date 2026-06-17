# Read a parquet file from Azure storage

Read a parquet file from Azure storage

## Usage

``` r
read_azure_parquet(container, file, ...)
```

## Arguments

- container:

  An Azure container object, as returned by
  [get_container](https://the-strategy-unit.github.io/azkit/reference/get_container.md)

- file:

  string The path to the file to be read.

- ...:

  optional arguments to be passed through to
  [arrow::read_parquet](https://arrow.apache.org/docs/r/reference/read_parquet.html)

## Value

A tibble

## Examples

``` r
if (FALSE) { # \dontrun{
  read_azure_parquet(cont, "data/folder/path/1.parquet")
} # }
```
