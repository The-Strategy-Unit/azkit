# Check if a supplied non-NULL value is a string with \>0 characters

Will error if x is equal to `""`, or if it is otherwise missing or
invalid. With the exception that if x is NULL, then NULL will be passed
through.

## Usage

``` r
check_nzchar(x, message, pf = parent.frame())
```

## Arguments

- x:

  The object to be checked

- message:

  A custom error message, as a string. Will be shown to the user if the
  check does not pass. Can include `glue` variables and `{cli}` semantic
  markup. Variable values will be searched for in the environment of the
  caller function (not in the environment of `check_nzchar()`). This
  makes it easier to include informative values in the message.

- pf:

  Set as [`parent.frame()`](https://rdrr.io/r/base/sys.parent.html) so
  variables in the caller environment can be used in the custom error
  message.
