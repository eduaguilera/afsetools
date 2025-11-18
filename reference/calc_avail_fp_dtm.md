# Calculate Available Footprint Using Detailed Trade Matrix

Calculates the footprint of product availability using bilateral trade
data (DTM).

## Usage

``` r
calc_avail_fp_dtm(filtered_cbs, df)
```

## Arguments

- filtered_cbs:

  Filtered CBS data with production and import

- df:

  Product footprint data frame

## Value

A data frame with availability footprints including bilateral import
footprints

## Examples

``` r
if (FALSE) { # \dontrun{
avail_fp <- calc_avail_fp_dtm(filtered_cbs, product_footprints)
} # }
```
