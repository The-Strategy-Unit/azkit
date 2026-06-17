# Check if Azure Instance Metadata Service (IMDS) is Available

This function checks if the Azure Instance Metadata Service (IMDS) is
available by attempting to make a request to the IMDS endpoint. The
result is cached in an environment variable for future use, saving the
need for repeated checks.

## Usage

``` r
imds_available()
```

## Details

You can also set the `IMDS_AVAILABLE` environment variable manually to
"TRUE" or "FALSE" to override the automatic check, which can be useful
for testing or in environments where the check may not work correctly.
