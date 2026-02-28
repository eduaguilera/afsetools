# Summarize BNF Results by Crop or Region

Produces a concise summary of BNF components and environmental
adjustment factors from the output of
[`calc_bnf`](https://eduaguilera.github.io/afsetools/reference/calc_bnf.md)
or its component functions. Useful for diagnostics, parameter
sensitivity analysis, and reporting.

## Usage

``` r
summarize_bnf(x, group_by = "Name_biomass")
```

## Arguments

- x:

  Data frame output from
  [`calc_bnf()`](https://eduaguilera.github.io/afsetools/reference/calc_bnf.md)
  or similar, containing BNF result columns.

- group_by:

  Character vector of columns to group by. Default `c("Name_biomass")`.
  Set to `NULL` for overall summary.

## Value

A tibble with per-group summaries:

- n:

  Number of observations.

- Cat_leg:

  Legume category from `Pure_legs`: "Grain", "Fodder_pure", or NA for
  non-legume/mixed systems.

- total_CropBNF_MgN:

  Sum of symbiotic crop BNF.

- total_WeedsBNF_MgN:

  Sum of weed/CC BNF.

- total_NSBNF_MgN:

  Sum of non-symbiotic BNF.

- total_BNF_MgN:

  Sum of total BNF.

- mean_Ndfa_adj:

  Mean adjusted Ndfa.

- mean_f_env_symb:

  Mean symbiotic env factor.

- mean_f_env_ns:

  Mean non-symbiotic env factor.

- pct_CropBNF:

  Percent of total from crop.

- pct_WeedsBNF:

  Percent of total from weeds.

- pct_NSBNF:

  Percent of total from NSBNF.

## Examples

``` r
if (FALSE) { # \dontrun{
load_general_data()
bnf_results <- npp_data |> calc_bnf()
summarize_bnf(bnf_results)
summarize_bnf(bnf_results, group_by = c("Region", "Year"))
} # }
```
