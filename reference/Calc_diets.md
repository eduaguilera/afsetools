# Calculate Nutrient Composition of Diets

Calculates the nutrient composition of diets including energy, protein,
lipids, carbohydrates, calcium, and vitamin A.

## Usage

``` r
Calc_diets(PIE_dest_df, Pop)
```

## Arguments

- PIE_dest_df:

  A data frame with food destiny data (columns: Year, area, item_cbs,
  Element, Destiny, FM_Mg)

- Pop:

  A data frame with population data (columns: Year, area, Pop_Mpeop)

## Value

A data frame with dietary nutrient availability per capita per day

## Examples

``` r
if (FALSE) { # \dontrun{
diet_nutrients <- Calc_diets(food_destiny_data, population_data)
} # }
```
