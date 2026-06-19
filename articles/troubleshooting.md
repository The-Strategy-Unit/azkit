# Troubleshooting

## Troubleshooting

Azure authentication is probably the main area where you might
experience difficulty. To debug, try running:

``` r

library(azkit)

token <- azkit::get_auth_token()
```

It should look like:

    Azure Active Directory v1.0 token for resource https://url.com
      Tenant:
      App ID:
      Authentication method:
      Token valid from: to:
      MD5 hash of inputs: 

If this errors, it’s worth testing the authentication is set up
correctly by using another package:

``` r

# install.packages("AzureRMR")

# This will open the browser and prompt for a Microsoft Account to be used to connect
AzureRMR::create_azure_login()

AzureRMR::get_azure_login()
```

A successful authentication will result in the following message:

    Loading Azure Resource Manager login for default tenant

If you had successfully used azkit to get a token and then run the code
above you may now have multiple tokens which you can view:

``` r

AzureRMR::list_azure_tokens()
```

To refresh a token:

``` r

azkit::refresh_token(token)
```

## Errors reading files

If you get errors when reading in files, first check that you are
passing in the full and correct file path relative to the root directory
of the container.

Next try reading in the raw data with
[`read_azure_file()`](https://the-strategy-unit.github.io/azkit/reference/read_azure_file.md)
which returns the binary form of data (which isn’t human readable). If
this is successful then you will be able to pass the data to a handler
function that’s relevant to the data. Details can be found in [Getting
Started](https://the-strategy-unit.github.io/azkit/articles/azkit.md).
