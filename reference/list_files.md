# List files in a container

Lists all files (recursively, if desired) found in a container within a
given directory (`dir`). The search can be restricted to files with a
specific extension.

## Usage

``` r
list_files(container, dir = "", ext = "", recursive = FALSE)
```

## Arguments

- container:

  An Azure container object, as returned by
  [get_container](https://the-strategy-unit.github.io/azkit/reference/get_container.md)

- dir:

  (optional) The directory of the container to list files within. `""`
  (the root directory of the container) by default

- ext:

  (optional) A string giving the extension of a particular file type to
  restrict the list to. No need to include the initial ".". The default,
  `""`, means no filtering by file extension will be applied.

- recursive:

  logical: whether to list files recursively. Default `FALSE`

## Value

A vector of file names, or an empty character vector if none found

## Details

The function does not support filtering by file name, only by file
extension.

The returned file list (character vector) contains the full paths to the
files, ready to be passed perhaps to a `read_azure_*` function, or
filtered further. If you just want the names of the files without the
folder path, use [basename](https://rdrr.io/r/base/basename.html) to
extract these.

## Examples

``` r
if (FALSE) { # \dontrun{
  list_files(get_container("example"), ext = "csv")
} # }
```
