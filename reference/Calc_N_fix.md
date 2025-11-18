# Biological Nitrogen Fixation Functions

Calculate biological nitrogen fixation from crops, weeds, and
non-symbiotic sources.

## Usage

``` r
Calc_N_fix(x)
```

## Arguments

- x:

  A data frame with crop NPP data including columns: Crop_NPP_MgN,
  Prod_MgN, Weeds_NPP_MgN, LandUse, Area_ygpit_ha

## Value

A data frame with BNF calculations: CropBNF, WeedsBNF, NSBNF, and total
BNF

## Examples

``` r
if (FALSE) { # \dontrun{
bnf_results <- Calc_N_fix(npp_data)
} # }
```
