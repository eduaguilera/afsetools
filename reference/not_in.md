# Exclude operator

Negation of the \`

## Usage

``` r
x %!in% y
```

## Arguments

- x:

  Vector or NULL: the values to be matched

- y:

  Vector or NULL: the values to be matched against

## Value

A logical vector indicating if there is no match for each element of x

## Examples

``` r
c(1, 2, 3) %!in% c(2, 4, 6) # Returns TRUE FALSE TRUE
#> [1]  TRUE FALSE  TRUE
```
