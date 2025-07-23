# `{azkit}` 🌊🔑📂📦![R](https://www.r-project.org/favicon-32x32.png)

<!-- badges: start -->
[![Project Status: WIP – Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/wip.svg)][repostatus]
<!-- badges: end -->

[repostatus]: https://www.repostatus.org/#wip

R package to handle Azure authentication and basic tasks with blob storage.

The package is in development.
Please leave an issue or raise a pull request if you have ideas for its
improvement.

## Usage

To be added.

## Secrets

To access Azure Storage you need to add some variables to a
[`.Renviron` file][posit] in your project.

⚠️These values are sensitive and should not be exposed to anyone outside The
Strategy Unit.
Make sure you have '.Renviron' listed in [the `.gitignore` file][github] for
your project.

Your `.Renviron` file should contain the variables below.
Ask a member of [the Data Science team][suds] for the necessary values.

```
AZ_STORAGE_EP=
AZ_STORAGE_CONTAINER=
```

These may vary depending on the specific container you’re connecting to.

## Getting help

Please use the Issues feature here on GitHub to report any bugs or problems.

Alternatively, to raise any questions about the package contact
[Fran Barton](mailto:francis.barton@nhs.net).

[posit]: https://docs.posit.co/ide/user/ide/guide/environments/r/managing-r.html#renviron
[github]: https://docs.github.com/en/get-started/getting-started-with-git/ignoring-files
[suds]: https://the-strategy-unit.github.io/data_science/about.html
