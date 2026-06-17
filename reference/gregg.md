# grepl a glued regex

Use {glue} expressions in grepl (and put the arguments the right way
round) (https://glue.tidyverse.org/articles/wrappers.html)

## Usage

``` r
gregg(x, rx, ..., g = parent.frame())
```

## Arguments

- x:

  A character vector to check

- rx:

  A string that after processing by
  [`glue::glue_data()`](https://glue.tidyverse.org/reference/glue.html)
  will be used as a regex pattern in
  [`grepl()`](https://rdrr.io/r/base/grep.html)

- ...:

  Arguments passed onto [`grepl()`](https://rdrr.io/r/base/grep.html)

- g:

  The parent frame of gregg, which must be passed through to glue_data
