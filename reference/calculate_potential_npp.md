# Calculate Potential Net Primary Production (NPP)

Calculates potential NPP using various models including Miami, NCEAS,
and Rosenzweig. This function estimates ecosystem productivity based on
temperature and precipitation.

## Usage

``` r
calculate_potential_npp(Dataset)
```

## Arguments

- Dataset:

  A data frame containing climate data with columns: TMP (temperature),
  Water_input_mm (Precipitation + Irrigation), AET_mm (actual
  evapotranspiration)

## Value

A data frame with calculated NPP values from different models in MgDM/ha

## Examples

``` r
if (FALSE) { # \dontrun{
climate_data <- data.frame(TMP = 15, WaterInput_mm = 800, AET_mm = 700)
npp_results <- calculate_potential_npp(climate_data)
} # }
```
