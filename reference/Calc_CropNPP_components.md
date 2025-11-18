# Calculate Cropland NPP Components Including Weeds

Calculates complete cropland NPP including weed biomass, scaled by
potential NPP and considering fallow periods.

## Usage

``` r
Calc_CropNPP_components(Crop_NPPpot)
```

## Arguments

- Crop_NPPpot:

  A data frame with potential NPP values and crop data

## Value

A data frame with complete cropland NPP components (crop + weeds) in DM,
C, N

## Examples

``` r
if (FALSE) { # \dontrun{
cropland_npp <- Calc_CropNPP_components(crop_npp_potential)
} # }
```
