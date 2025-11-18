# ggplot2 Themes for Scientific Plots

Custom ggplot2 themes for creating publication-ready scientific figures.
Custom ggplot2 Theme

## Usage

``` r
theme_new(base_size = 7, base_family = "")
```

## Arguments

- base_size:

  Base font size. Default is 7.

- base_family:

  Base font family. Default is "".

## Value

A ggplot2 theme object

## Details

A clean theme for scientific plots with white background and minimal
grid lines.

## Examples

``` r
if (FALSE) { # \dontrun{
library(ggplot2)
ggplot(mtcars, aes(wt, mpg)) +
  geom_point() +
  theme_new()
} # }
```
