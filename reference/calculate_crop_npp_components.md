# Calculate Cropland NPP Components Including Weeds

Calculates complete cropland NPP including weed biomass, scaled by
potential NPP and considering fallow periods.

## Usage

``` r
calculate_crop_npp_components(Crop_NPPpot, .by = NULL)
```

## Arguments

- Crop_NPPpot:

  A data frame with potential NPP values and crop data

- .by:

  Character vector of column names for grouping operations (e.g.,
  c("Year", "Region")). Used for scaling weeds calculations. If NULL, no
  grouping is applied.

## Value

A data frame with complete cropland NPP components (crop + weeds) in DM,
C, N

## Examples

``` r
if (FALSE) { # \dontrun{
# With grouping by Year
cropland_npp <- calculate_crop_npp_components(crop_npp_potential, .by = "Year")

# With multiple grouping variables
cropland_npp <- calculate_crop_npp_components(crop_npp_potential, 
                                                .by = c("Year", "Region"))

# Without grouping
cropland_npp <- calculate_crop_npp_components(crop_npp_potential, .by = NULL)
} # }
```
