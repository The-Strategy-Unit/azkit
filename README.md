# `azkit` 🌊🔑📂📦![R](https://www.r-project.org/favicon-32x32.png) <img src="man/figures/logo.png" align="right" height="138" alt="azkit badge" />

<!-- badges: start -->
[![License: MIT][mit_svg]](https://opensource.org/licenses/MIT)
[![Project Status: Active – The project has reached a stable, usable state and is being actively developed][repostatus_svg]][repostatus_info]
![GitHub R package version][gh_ver]
[![R CMD check status][cmd_svg]][cmd_yaml]

[mit_svg]: https://img.shields.io/badge/License-MIT-yellow.svg
[gh_ver]: https://img.shields.io/github/r-package/v/The-Strategy-Unit/azkit
[repostatus_info]: https://www.repostatus.org/#project-statuses
[repostatus_svg]: https://www.repostatus.org/badges/latest/active.svg
[cmd_svg]: https://github.com/The-Strategy-Unit/azkit/actions/workflows/R-CMD-check.yaml/badge.svg?event=release
[cmd_yaml]: https://github.com/The-Strategy-Unit/azkit/actions/workflows/R-CMD-check.yaml
<!-- badges: end -->

An R package to handle Azure authentication and some basic tasks accessing
blob and table storage and reading in data from files.

## Status

The package is in development.
Please [create an issue][issues] if you have ideas for its improvement.

## Installation

You can install the development version of `{azkit}` with:

``` r
# install.packages("pak")
pak::pak("The-Strategy-Unit/azkit")
```

## Usage

Functions are grouped with consistent prefixes:

- `check_`
- `get_`
- `list_`
- `read_`

There are some single prefixed functions:

- `cst_`
- `ct_`
- `cv_`
- `generate_`
- `imds_`

## Environment variables

To facilitate access to Azure Storage you will need to set up credentials to 
authenticate access and you may want to set these in the environment. Details
for the different places to save this can be found in 
[Getting Started](./azkit.html).

> ⚠️ Important  
> Ensure you follow the guidelines set by your IT security and/or team about
how and where you save Azure credentials.

## Getting help

Please use the [Issues][issues] feature on GitHub to report any bugs, ideas
or problems.

Alternatively, to ask any questions about the package you may contact
[Fran Barton](mailto:francis.barton@nhs.net).


[issues]: https://github.com/The-Strategy-Unit/azkit/issues
