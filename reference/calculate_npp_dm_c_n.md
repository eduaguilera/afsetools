# Calculate NPP in Dry Matter, Carbon, and Nitrogen

Converts NPP components to dry matter, carbon, and nitrogen units,
including crop and weed biomass.

## Usage

``` r
calculate_npp_dm_c_n(AreaNPP)
```

## Arguments

- AreaNPP:

  A data frame with NPP components and biomass coefficients

## Value

A data frame with NPP expressed in DM, C, and N for all components

## Examples

``` r
if (FALSE) { # \dontrun{
npp_nutrients <- calculate_npp_dm_c_n(area_npp_data)
} # }
```
