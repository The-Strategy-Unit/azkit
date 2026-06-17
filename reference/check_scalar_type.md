# An alternative to stopifnot/assert_that etc

This function makes it easy to use the `is_scalar_*` functions from
`{rlang}` to check the type of `x`, *and* that `length(x) == 1`, and
supports the seamless use of `glue` strings in the custom error message.
Possible values for the `type` parameter are: "character", "logical",
"list", "integer", "double", "string", "bool", "bytes", "raw", "vector",
"complex".

## Usage

``` r
check_scalar_type(x, type, message, pf = parent.frame())
```

## Arguments

- x:

  The object to be checked

- type:

  A string defining the R object type that `x` is checked to be

- message:

  A custom error message, as a string. Will be shown to the user if the
  predicate check does not succeed. Can include `glue`d variables and
  `{cli}` semantic markup.

- pf:

  Set as [`parent.frame()`](https://rdrr.io/r/base/sys.parent.html) so
  variables in the caller environment can be used in the custom error
  message.

## See also

[check_that](https://the-strategy-unit.github.io/azkit/reference/check_that.md)
