# Allocate Impacts to Products Based on Economic Value

Performs economic allocation of environmental impacts among co-products
(e.g., cotton lint and cottonseed) and creates draught animal items.

## Usage

``` r
Allocate_impacts_to_products(df, draught_shares)
```

## Arguments

- df:

  A data frame with impact data for products

- draught_shares:

  Draught animal allocation shares by \`Year\`, \`area\`, \`Live_anim\`

## Value

A data frame with allocated impacts (Allocation, Impact_u, u_ton columns
added)

## Examples

``` r
if (FALSE) { # \dontrun{
allocated_impacts <- Allocate_impacts_to_products(impact_data, draught_shares)
} # }
```
