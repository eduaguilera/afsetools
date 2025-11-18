# Calculate Crop Net Primary Production Components

Calculates the components of crop NPP including product, residue, and
root biomass in dry matter units.

## Usage

``` r
Calculate_crop_NPP(Dataset, HI, ...)
```

## Arguments

- Dataset:

  A data frame with crop area and production data

- HI:

  A data frame with harvest index (HI) values

- ...:

  Additional grouping variables to preserve in output

## Value

A data frame with NPP components (Prod_MgDM, Residue_MgDM, Root_MgDM)

## Examples

``` r
if (FALSE) { # \dontrun{
crop_npp <- Calculate_crop_NPP(crop_data, harvest_index)
} # }
```
