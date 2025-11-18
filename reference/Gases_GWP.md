# Classify GHG Emissions and Calculate Global Warming Potential

Classifies greenhouse gas emissions and calculates GWP100 CO2
equivalents.

## Usage

``` r
Gases_GWP(x)
```

## Arguments

- x:

  A data frame with columns: Gas_raw, Gas_type, value

## Value

A data frame with Gas, Gas_categ, and CO2e_Tg calculated

## Examples

``` r
if (FALSE) { # \dontrun{
ghg_with_gwp <- Gases_GWP(emissions_data)
} # }
```
