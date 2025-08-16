# `{azkit}` 🌊🔑📂📦![R](https://www.r-project.org/favicon-32x32.png)

<!-- badges: start -->
[![License: MIT][mit_svg]](https://opensource.org/licenses/MIT)
[![Project Status: WIP – Initial development is in progress, but there
has not yet been a stable release][repostatus_svg]][repostatus_info]
[![Lifecycle: experimental][lifecycle_svg]][lifecycle]
![GitHub R package version][gh_ver]
[![R CMD check status][cmd_svg]][cmd_yaml]

[mit_svg]: https://img.shields.io/badge/License-MIT-yellow.svg
[gh_ver]: https://img.shields.io/github/r-package/v/The-Strategy-Unit/azkit
[repostatus_info]: https://www.repostatus.org/#project-statuses
[repostatus_svg]: https://www.repostatus.org/badges/latest/wip.svg
[lifecycle]: https://lifecycle.r-lib.org/articles/stages.html#experimental
[lifecycle_svg]: https://img.shields.io/badge/lifecycle-experimental-orange.svg
[cmd_svg]: https://github.com/The-Strategy-Unit/azkit/actions/workflows/R-CMD-check.yaml/badge.svg
[cmd_yaml]: https://github.com/The-Strategy-Unit/azkit/actions/workflows/R-CMD-check.yaml
<!-- badges: end -->


R package to handle Azure authentication and basic tasks with blob storage.

## Status

The package is in development.
Please leave an issue or raise a pull request if you have ideas for its
improvement.

## Installation

You can install the development version of `{azkit}` with:

``` r
# install.packages("pak")
pak::pak("The-Strategy-Unit/azkit")
```

## Usage

A primary function in `{azkit}` enables access to an Azure blob container:

```r
data_container <- azkit::get_container()

```
Authentication is handled "under the hood" by the `get_container()` function,
but if you need to, you can explicitly return an authentication token for
inspection or testing:

```r
my_token <- azkit::get_auth_token()

```

The container returned will be set by the name stored in the `AZ_CONTAINER`
environment variable, if any, by default, but you can override this by supplying
a container name to the function:

```r
custom_container <- azkit::get_container("custom")
```

Return a list of all available containers in your default Azure storage with:

```r
list_container_names()
```

Once you have access to a container, you can use one of a set of data reading
functions to bring data into R from `.parquet`, `.rds`, `.json` or `.csv` files:

```r
pqt_data <- azkit::read_azure_parquet(data_container, "v_important_data")

```

The functions will try to match a file of the required type using the `file`
name supplied. In the case above, "v_important_data" would match a file named
"v_important_data.parquet", no need to supply the file extension.

By default the `read_*` functions will look in the root folder of the container.
To specify a subfolder, supply this to the `path` argument.
The functions will _not_ search recursively into further subfolders, so the path
needs to be full and accurate.

If there is more than 1 file matching the string supplied to `file` argument,
the functions will throw an error.
Specifying the exact filename will avoid this of course - but shorter `file`
arguments may be convenient in some situations.

Currently these functions only read in a single file at a time.

Setting the `info` argument to `TRUE` will enable the functions to give some
confirmatory feedback on what file is being read in.
You can also pass through arguments to for example `readr::read_csv()`:

```r
csv_data <- data_container |>
  azkit::read_azure_csv("vital_data.csv", path = "data", col_types = "ccci")

```

## Environment variables

To access Azure Storage you will want to set some environment variables.
The neatest way to do this is to include a [`.Renviron` file][posit_env] in
your project folder.

⚠️These values are sensitive and should not be exposed to anyone outside The
Strategy Unit.
Make sure you include `.Renviron` in [the `.gitignore` file][github] for
your project.

Your `.Renviron` file should contain the variables below.
Ask a member of [the Data Science team][suds] for the necessary values.

```
# essential
AZ_STORAGE_EP=
# useful but not absolutely essential:
AZ_CONTAINER=

# optional, for certain authentication scenarios:
AZ_TENANT_ID=
AZ_CLIENT_ID=
AZ_APP_SECRET=
```

These may vary depending on the specific container you’re connecting to.

For one project you might want to set the default container (`AZ_CONTAINER`) to
one value, but for a different project you might be mainly working with a
different container so it would make sense to set the values within the
`.Renviron` file for each project, rather than globally for your account.

## Getting help

Please use the [Issues][issues] feature on GitHub to report any bugs, ideas
or problems, including with the package documentation.

Alternatively, to ask any questions about the package you may contact
[Fran Barton](mailto:francis.barton@nhs.net).

[posit_env]: https://docs.posit.co/ide/user/ide/guide/environments/r/managing-r.html#renviron
[github]: https://docs.github.com/en/get-started/getting-started-with-git/ignoring-files
[suds]: https://the-strategy-unit.github.io/data_science/about.html
[issues]: https://github.com/The-Strategy-Unit/azkit/issues
