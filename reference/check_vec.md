# An alternative to stopifnot/assert_that etc

This function makes it easy to use the `{purrr}` functions `every()`,
`some()` and `none()` to handle vector inputs of length \>= 1, and
supports the seamless use of `glue` strings in the custom error message.
Not suitable for checking if `length(x) == 1` as it will check vectors
element-wise, so will potentially return `TRUE` even if `length(x) > 1`

## Usage

``` r
check_vec(
  x,
  predicate,
  message,
  which = c("every", "some", "none"),
  pf = parent.frame()
)
```

## Arguments

- x:

  The object to be checked

- predicate:

  The predicate function used to check elements of `x`

- message:

  A custom error message, as a string. Will be shown to the user if the
  predicate check does not succeed. Can include `glue`d variables and
  `{cli}` semantic markup. Variable values will be searched for in the
  environment of the caller function (not in the environment of
  `check_vec()` ). This makes it easier to include informative values in
  the message.

- which:

  One of "every", "some", "none", as a string. Defines which `{purrr}`
  function to use when applying the predicate. "every", the default,
  means that `check_vec()` will succeed if every value of x satisfies
  the predicate. "none" can be used to generate an inverse predicate, or
  the situation where success means that none of the elements of x
  satisfies the predicate. "some" is unlikely to be useful often, but it
  is available.

- pf:

  Set as [`parent.frame()`](https://rdrr.io/r/base/sys.parent.html) so
  variables in the caller environment can be used in the custom error
  message.

## See also

[`check_scalar_type()`](https://the-strategy-unit.github.io/azkit/reference/check_scalar_type.md)
