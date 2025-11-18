# Allocate Impacts to Products Based on Economic Value

Performs economic allocation of environmental impacts among co-products
(e.g., cotton lint and cottonseed) and creates draught animal items.

## Usage

``` r
Allocate_impacts_to_products(df)
```

## Arguments

- df:

  A data frame with impact data for products

## Value

A data frame with allocated impacts (Allocation, Impact_u, u_ton columns
added)

## Examples

``` r
if (FALSE) { # \dontrun{
allocated_impacts <- Allocate_impacts_to_products(impact_data)
} # }
```
