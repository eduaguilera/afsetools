# Plot Linear Regression with Statistics

Creates a scatter plot with linear regression line and displays model
statistics (R², intercept, slope, and p-value) in the title.

## Usage

``` r
ggplotRegression(fit)
```

## Arguments

- fit:

  A linear model object created with lm()

## Value

A ggplot2 plot object

## Examples

``` r
if (FALSE) { # \dontrun{
model <- lm(mpg ~ wt, data = mtcars)
ggplotRegression(model)
} # }
```
