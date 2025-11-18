# Fill gaps using proxy variable

Fills gaps by using changes in a proxy variable, using ratios between
filled variable and proxy variable. Labels output according to filling
method.

## Usage

``` r
FillingProxy(data, var, proxyvar, Index)
```

## Arguments

- data:

  Data frame (grouped if needed)

- var:

  Variable to be filled (unquoted column name)

- proxyvar:

  Variable used as proxy (unquoted column name)

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
was present in original data - "Proxy interpolated": Value filled using
interpolated proxy ratio - "Proxy carried forward": Value filled using
proxy ratio carried forward - "Proxy carried backwards": Value filled
using proxy ratio carried backward

## Examples

``` r
if (FALSE) { # \dontrun{
data |>
  group_by(Region, Item) |>
  FillingProxy(Production, Area, Year) |>
  ungroup()
} # }
```
