# Fill gaps in time series

Fills gaps by linear interpolation when possible, or carrying
forward/backward when not possible. Labels output according to filling
method.

## Usage

``` r
Filling(data, var, Index)
```

## Arguments

- data:

  Data frame (grouped if needed)

- var:

  Variable to be filled (unquoted column name)

- Index:

  Time index (usually year)

## Value

Data frame with filled variable and source column indicating filling
method

## Details

Remember to use \`group_by()\` and \`ungroup()\` when needed (do NOT use
\`.by\` with this function). Always add \`dplyr::ungroup()\` immediately
after using this function to prevent grouped data from flowing
downstream.

The function creates a Source\_\* column indicating: - "Original": Value
was present in original data - "Linear interpolation": Value filled by
linear interpolation - "Last value carried forward": Value filled by
carrying last known value forward - "First value carried backwards":
Value filled by carrying first known value backward

## Examples

``` r
if (FALSE) { # \dontrun{
data |>
  group_by(Region, Item) |>
  Filling(Production, Year) |>
  ungroup()
} # }
```
