# Calculate Available Footprint Using Gross Trade

Calculates the footprint of product availability (production + import)
using gross trade data (no bilateral trade detail).

## Usage

``` r
calc_avail_fp_gt(filtered_cbs, df, cbs)
```

## Arguments

- filtered_cbs:

  Filtered CBS data with production and import

- df:

  Product footprint data frame

- cbs:

  Commodity balance sheet data

## Value

A data frame with availability footprints including imports

## Examples

``` r
if (FALSE) { # \dontrun{
avail_fp <- calc_avail_fp_gt(filtered_cbs, product_footprints, cbs)
} # }
```
