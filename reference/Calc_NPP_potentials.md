# Calculate Potential Net Primary Production (NPP)

Calculates potential NPP using various models including Miami, NCEAS,
and Rosenzweig. This function estimates ecosystem productivity based on
temperature and precipitation.

## Usage

``` r
Calc_NPP_potentials(Dataset)
```

## Arguments

- Dataset:

  A data frame containing climate data with columns: TMP (temperature),
  MAP (precipitation), PET (potential evapotranspiration), AET (actual
  evapotranspiration)

## Value

A data frame with calculated NPP values from different models in MgDM/ha

## Examples

``` r
if (FALSE) { # \dontrun{
climate_data <- data.frame(TMP = 15, MAP = 800, PET = 1000, AET = 700)
npp_results <- Calc_NPP_potentials(climate_data)
} # }
```
