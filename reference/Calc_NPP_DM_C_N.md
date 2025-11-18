# Calculate NPP in Dry Matter, Carbon, and Nitrogen

Converts NPP components to dry matter, carbon, and nitrogen units,
including crop and weed biomass.

## Usage

``` r
Calc_NPP_DM_C_N(AreaNPP)
```

## Arguments

- AreaNPP:

  A data frame with NPP components and biomass coefficients

## Value

A data frame with NPP expressed in DM, C, and N for all components

## Examples

``` r
if (FALSE) { # \dontrun{
npp_nutrients <- Calc_NPP_DM_C_N(area_npp_data)
} # }
```
