# Biological Nitrogen Fixation (Legacy Wrapper)

Legacy wrapper around
[`calc_bnf`](https://eduaguilera.github.io/afsetools/reference/calc_bnf.md)
for backward compatibility. New code should use
[`calc_bnf`](https://eduaguilera.github.io/afsetools/reference/calc_bnf.md)
directly, which adds literature-based environmental modifiers for N
inputs, temperature, water stress, soil organic matter, and soil pH.

## Usage

``` r
Calc_N_fix(x)
```

## Arguments

- x:

  A data frame with crop NPP data including columns: Crop_NPP_MgN,
  Prod_MgN, Weeds_NPP_MgN, LandUse, Area_ygpit_ha, Legs_Seeded,
  Seeded_CC_share, Name_biomass

## Value

A data frame with BNF calculations: CropBNF, WeedsBNF, NSBNF, and total
BNF

## See also

[`calc_bnf`](https://eduaguilera.github.io/afsetools/reference/calc_bnf.md)
for the improved version with environmental modifiers.

## Examples

``` r
if (FALSE) { # \dontrun{
bnf_results <- Calc_N_fix(npp_data)
# Prefer: bnf_results <- calc_bnf(npp_data)
} # }
```
