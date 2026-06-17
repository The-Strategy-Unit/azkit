# An alternative to stopifnot/assert_that etc

If the predicate function is true of `x` then `x` is returned.
Otherwise, an error is thrown with a custom `message`.

## Usage

``` r
check_that(x, predicate, message, pf = parent.frame())
```

## Arguments

- x:

  The object to be checked

- predicate:

  The predicate function used to check `x`

- message:

  A custom error message, as a string. Will be shown to the user if the
  predicate check does not succeed. Can include `glue`d variables and
  `{cli}` semantic markup.

- pf:

  Set as [`parent.frame()`](https://rdrr.io/r/base/sys.parent.html) so
  variables in the caller environment can be used in the custom error
  message.

## See also

[check_vec](https://the-strategy-unit.github.io/azkit/reference/check_vec.md)
