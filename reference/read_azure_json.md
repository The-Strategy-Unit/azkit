# Read a json file from Azure storage

Read a json file from Azure storage

## Usage

``` r
read_azure_json(container, file, ...)
```

## Arguments

- container:

  An Azure container object, as returned by
  [get_container](https://the-strategy-unit.github.io/azkit/reference/get_container.md)

- file:

  string The path to the file to be read.

- ...:

  optional arguments to be passed through to
  [yyjsonr::read_json_raw](https://coolbutuseless.github.io/package/yyjsonr/reference/read_json_raw.html)

## Value

A list
