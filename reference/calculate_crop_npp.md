# Calculate Crop Net Primary Production Components

Calculates the components of crop NPP including product, residue, and
root biomass in dry matter units.

## Usage

``` r
calculate_crop_npp(Dataset, HI)
```

## Arguments

- Dataset:

  A data frame with crop area and production data

- HI:

  A data frame with harvest index (HI) values

## Value

A data frame with NPP components (Prod_MgDM, Residue_MgDM, Root_MgDM)

## Examples

``` r
if (FALSE) { # \dontrun{
crop_npp <- calculate_crop_npp(crop_data, harvest_index)
} # }
```
