# Get Global Average Export Footprint

Calculates the global average footprint of exported products, weighted
by export shares.

## Usage

``` r
get_global_export_footprint(df, cbs)
```

## Arguments

- df:

  A data frame with product footprints

- cbs:

  Commodity balance sheet data used to calculate export shares

## Value

A data frame with global average footprints (u_ton_glob)

## Examples

``` r
if (FALSE) { # \dontrun{
global_footprint <- get_global_export_footprint(product_footprints, cbs)
} # }
```
