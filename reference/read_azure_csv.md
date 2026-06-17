# Read a csv file from Azure storage

Read a csv file from Azure storage

## Usage

``` r
read_azure_csv(container, file, ...)
```

## Arguments

- container:

  An Azure container object, as returned by
  [get_container](https://the-strategy-unit.github.io/azkit/reference/get_container.md)

- file:

  string The path to the file to be read.

- ...:

  optional arguments to be passed through to
  [readr::read_delim](https://readr.tidyverse.org/reference/read_delim.html)

## Value

A tibble
