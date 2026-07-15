# Troubleshooting

Azure authentication is probably the main area where you might
experience difficulty. To debug initially, try running:

``` r

token <- azkit::get_auth_token()
```

It should look something like:

``` sh
Azure Active Directory v1.0 token for resource https://url.com
  Tenant:
  App ID:
  Authentication method:
  Token valid from: to:
  MD5 hash of inputs:
```

If this errors, it’s worth testing the authentication is set up
correctly by using another package:

``` r

# pak::pak("AzureRMR")

# This will open the browser and prompt for a Microsoft account to be used to
# connect
AzureRMR::create_azure_login()
AzureRMR::get_azure_login()
```

A successful authentication will result in the following message:

``` sh
Loading Azure Resource Manager login for default tenant
```

> ℹ️ **Note**
>
> Users at The Strategy Unit: when initially authenticating with Azure,
> you will be sent to the web. Since you may be logged into one of
> multiple Microsoft tenants, it is important that you authenticate with
> the same account (MLCSU) that is used for access to Azure storage. If
> you authenticate when logged into your nhs.net Microsoft account in
> your browser, your token will not be useable for access to Azure.

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

You may wish to use the Azure Portal website or the Azure Storage
Explorer app to check that you have a valid container name and file
path.

The following function should also return `TRUE` if the file exists:

``` r

AzureStor::blob_exists(container_name, file)
```
