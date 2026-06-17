# `azkit` 🌊🔑📂📦![R](https://www.r-project.org/favicon-32x32.png)

An R package to handle Azure authentication and some basic tasks
accessing blob and table storage and reading in data from files.

## Status

The package is in development. Please [create an
issue](https://github.com/The-Strategy-Unit/azkit/issues) if you have
ideas for its improvement.

## Installation

You can install the development version of
[azkit](https://the-strategy-unit.github.io/azkit/) with:

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

To facilitate access to Azure Storage you will need to set up
credentials to authenticate access and you may want to set these in the
environment. Details for the different places to save this can be found
in [Getting
Started](https://the-strategy-unit.github.io/azkit/azkit.md).

> ⚠️ Important  
> Ensure you follow the guidelines set by your IT security and/or team
> about how and where you save Azure credentials.

## Getting help

Please use the
[Issues](https://github.com/The-Strategy-Unit/azkit/issues) feature on
GitHub to report any bugs, ideas or problems.

Alternatively, to ask any questions about the package you may contact
[Fran Barton](mailto:francis.barton@nhs.net).
