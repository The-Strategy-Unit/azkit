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

_To be added._

## Environment variables

To access Azure Storage you need to add some variables to a
[`.Renviron` file][posit_env] in your project.

⚠️These values are sensitive and should not be exposed to anyone outside The
Strategy Unit.
Make sure you include `.Renviron` in [the `.gitignore` file][github] for
your project.

Your `.Renviron` file should contain the variables below.
Ask a member of [the Data Science team][suds] for the necessary values.

```
AZ_STORAGE_EP=
AZ_STORAGE_CONTAINER=
```

These may vary depending on the specific container you’re connecting to.

## Getting help

Please use the [Issues][issues] feature on GitHub to report any bugs, ideas
or problems, including with the package documentation.

Alternatively, to ask any questions about the package you may contact
[Fran Barton](mailto:francis.barton@nhs.net).

[posit_env]: https://docs.posit.co/ide/user/ide/guide/environments/r/managing-r.html#renviron
[github]: https://docs.github.com/en/get-started/getting-started-with-git/ignoring-files
[suds]: https://the-strategy-unit.github.io/data_science/about.html
[issues]: https://github.com/The-Strategy-Unit/azkit/issues
