# Getting started

[azkit](https://the-strategy-unit.github.io/azkit/) is an R package to
help you:

- handle authentication with Azure
- perform basic tasks accessing blob (unstructured data) and table
  storage (structured data)
- read in data from some common file types.

The purpose of this page is to show you a few ways you might use {azkit}
functions to achieve certain tasks. It’s not the only way you can set
things up and use the functions, it’s just an opinionated example
workflow - you may choose to do things differently 😀.

## Setting up security credentials

> 💡 Tip  
> This only needs to be done once but if you choose to do this for a
> project rather than globally (user) then every project will need
> credentials set up.

Azure requires credentials set up locally on your computer in order to
the data it contains. There are a number of places this information can
be stored and their use depends on your team’s use and the security
level required.

### `.Renviron` files

There are two `.Renviron` files, one can be stored in your project
folder and one can be set globally so that any project can access the
credential. Both files need to be created and set up and can either be
done manually or, more conveniently, through using the `usethis`
package:

> ⚠️ Important  
> Don’t forget to ensure that the `.gitignore` has `.Renviron` listed.
> This is particularly important if the file is saved in the project.

``` r

# creates or opens an existing file in the project
usethis::edit_r_environ(scope = "project")
# Add a file to .gitignore if you haven't already done so
usethis::use_git_ignore(".Renviron")

# creates or opens an existing file in the global
usethis::edit_r_environ(scope = "user")

# just running the following defaults to "user"
usethis::edit_r_environ()
```

When opening a `.Renviron` file for the first time, whether user or
project, it will be blank and the following needs to be added:

    AZ_STORAGE_EP=

and after the `=` sign add the URL of your Azure endpoint (with no
quotes). Any changes to the files will require restart in R and if you
use the usethis function it will remind you in a message that appears in
the Console.

> 💡 Tip  
> If you use both `.Renviron` files the project will be read before the
> user. Ensure that the project `.Renviron` is not completely blank
> though as this will default to blank even if the details are in the
> user file.

## Using the `keyring` package for credentials

The package {keyring} can be a good way to separate credentials and are
also available globally so any project can access them.

To set the credential:

``` r

library(keyring)
keyring::key_set("AZ_STORAGE_EP")
```

A pop up box will appear where you can copy the url (without quotes). To
retrieve the credential you will need the following code:

``` r

keyring::key_get("AZ_STORAGE_EP")
```

To view what keyrings you have:

``` r

keyring::key_list()
```

## Find what containers there available to view

``` r

library(azkit)
```

To see the containers you have access to:

``` r

# This will work if you have the AZ_STORAGE_EP url stored in an `.Renviron` file
azkit::list_container_names()

# The default is the same as writing regardless of which `.Renviron` file is used
azkit::list_container_names(Sys.getenv("AZ_STORAGE_EP"))

# If you are using keyring you will need to type out the code:
azkit::list_container_names(endpoint_url = keyring::key_get("AZ_STORAGE_EP"))
```

## Accessing a container

Two functions are used together to access files from a particular
container.

> 💡 Tip The following will refer to `"supporting-data"` and this is a
> placeholder so will need to be changed to a container you have access
> to.

The function
[`list_files()`](https://the-strategy-unit.github.io/azkit/reference/list_files.md)
only returns files according to their type, rather than individual files
which may be more familiar with which list *all* files:

``` r

# library(here)

# This is just an example of a base R function that returns all the files
# list.files(here::here())
```

[`list_files()`](https://the-strategy-unit.github.io/azkit/reference/list_files.md)
requires
[`get_container()`](https://the-strategy-unit.github.io/azkit/reference/get_container.md)
to point to the exact container and provide the relevant credentials:

``` r

# Retrieving the container information will be used in subsequent code
sd_container <- get_container(
  container_name = "supporting-data",
  endpoint_url = Sys.getenv("AZ_STORAGE_EP")
)
```

``` r

# "supporting_data" is a name that you will have to change according to what the
# containers you have access to and what they are called
azkit::list_files(
  container = sd_container,
  ext = "csv"
)
```

If the container has sub folders the
[`list_files()`](https://the-strategy-unit.github.io/azkit/reference/list_files.md)
can also return the files in those when the `recursive =` parameter is
set to TRUE (the default is FALSE):

``` r

azkit::list_files(
  container = sd_container,
  ext = "csv",
  recursive = TRUE
)
```

> 💡 Tip  
> If you’d prefer to view Azure in a GUI (Graphical User Interface) and
> have permissions use the Microsoft program Azure Storage Explorer to
> explore containers or access via a browser by logging into
> <https://portal.azure.com/#home>

## Reading a file

For all the functions to get particular files, they need to be used in
conjunction with the
[`get_container()`](https://the-strategy-unit.github.io/azkit/reference/get_container.md)
function that provides details of the container and credentials.

For the simplest access downloading a csv:

``` r

# Note this code is "nested" so the get_container() function is inside
# the read_azure_csv() function

azkit::read_azure_csv(
  container = sd_container,
  file = "mitigator-lookup.csv"
)
```

You can also use other arguments that will be applied to the specific
file reader through the argument `...`. For example,
[`readr::read_delim()`](https://readr.tidyverse.org/reference/read_delim.html)
is used by the function `read_azure_csv` and because of this it’s
possible to use other arguments that the
[`readr::read_delim()`](https://readr.tidyverse.org/reference/read_delim.html)
could use:

``` r

# Note this code is "piped" so the get_container() function is before the
# the read_azure_csv() function

azkit::get_container(
  container_name = "supporting-data"
) |>
  azkit::read_azure_csv(
    file = "mitigator-lookup.csv",
    # select specific columns and can be used with other dplyr functions
    col_select = dplyr::starts_with("active")
  )
```

Functions for other file types all follow the same pattern using
[`get_container()`](https://the-strategy-unit.github.io/azkit/reference/get_container.md)
and detailing the file wanted:

``` r

azkit::read_azure_rds()
azkit::read_azure_json()
azkit::read_azure_jsongz()
azkit::read_azure_parquet()
```

These all will download in formats that you will be familiar with but if
the data is required in the `raw binary` form that is stored in Azure
and then parsed/translated by some other means then the following
function will retrieve any data type:

``` r

# This is not a human readable form!

azkit::read_azure_file(
  container = sd_container,
  file = "mitigator-lookup.csv"
)
```

This makes transferring data very fast and can be translated locally. In
this example from a .csv it would look like:

``` r

raw_data <- azkit::read_azure_file(
  container = sd_container,
  file = "mitigator-lookup.csv"
)

csv_data <- read.csv(text = rawToChar(raw_data))

# view top few rows of what is now a data frame
head(csv_data)
```

## Reading multiple files

The functions don’t read in multiple files but can be combined with
other code to do this using loops or the equivalent using the purrr
package:

``` r

list_of_csvs <- azkit::list_files(
  container = sd_container,
  ext = "csv"
)

list_of_csvs |>
  # get the name from the file name but drop the extension detail
  purrr::set_names(basename(tools::file_path_sans_ext(list_of_csvs))) |>
  purrr::map(\(x) {
    read_azure_csv(
      container = sd_container,
      x
    )
  }) |>
  list2env(.GlobalEnv)
```
