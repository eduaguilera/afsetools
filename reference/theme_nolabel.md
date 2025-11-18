# ggplot2 Theme Without Labels

A theme similar to theme_new() but with strip text removed, useful for
multi-panel plots.

## Usage

``` r
theme_nolabel(base_size = 7, base_family = "")
```

## Arguments

- base_size:

  Base font size. Default is 7.

- base_family:

  Base font family. Default is "".

## Value

A ggplot2 theme object

## Examples

``` r
if (FALSE) { # \dontrun{
library(ggplot2)
ggplot(mtcars, aes(wt, mpg)) +
  geom_point() +
  theme_nolabel()
} # }
```
