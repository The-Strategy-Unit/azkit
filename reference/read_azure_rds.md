# Read an rds file from Azure storage

Read an rds file from Azure storage

## Usage

``` r
read_azure_rds(container, file, ...)
```

## Arguments

- container:

  An Azure container object, as returned by
  [get_container](https://the-strategy-unit.github.io/azkit/reference/get_container.md)

- file:

  string The path to the file to be read.

- ...:

  optional arguments to be passed through to
  [AzureStor::storage_load_rds](https://rdrr.io/pkg/AzureStor/man/storage_save.html).
  For example, a compression type (one of c("unknown", "gzip", "bzip2",
  "xz", "zstd", "none")) can be provided using the argument `type`,
  which will be passed on to
  [memDecompress](https://rdrr.io/r/base/memCompress.html) via
  [AzureStor::storage_load_rds](https://rdrr.io/pkg/AzureStor/man/storage_save.html).

## Value

The data object that was stored in the rds file
