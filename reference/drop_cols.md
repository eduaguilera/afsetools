# Drop columns from data frame

Drops an undefined number of columns from a data frame. Silently ignores
columns that don't exist in the data frame.

## Usage

``` r
drop_cols(df, ...)
```

## Arguments

- df:

  Data frame

- ...:

  Column names to drop (character strings or character vector)

## Value

Data frame without the specified columns

## Examples

``` r
df <- data.frame(a = 1:3, b = 4:6, c = 7:9)
drop_cols(df, "a", "b")
#>   c
#> 1 7
#> 2 8
#> 3 9
drop_cols(df, c("a", "nonexistent"))  
#>   b c
#> 1 4 7
#> 2 5 8
#> 3 6 9
```
