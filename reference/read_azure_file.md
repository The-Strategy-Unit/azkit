# Read any file from Azure storage

Read any file from Azure storage

## Usage

``` r
read_azure_file(container, file, ...)
```

## Arguments

- container:

  An Azure container object, as returned by
  [get_container](https://the-strategy-unit.github.io/azkit/reference/get_container.md)

- file:

  string The path to the file to be read.

- ...:

  optional arguments to be passed through to
  [AzureStor::download_blob](https://rdrr.io/pkg/AzureStor/man/blob.html)

## Value

A raw data stream
