# Calculate Weed and Cover Crop Symbiotic BNF

Weed BNF is calculated as: \$\$WeedsBNF = Weeds\\NPP\\MgN \times
Ndfa\_{weeds} \times Leg\\share\_{weighted}\$\$

Where the weighted legume share accounts for both spontaneous leguminous
weeds and deliberately seeded legume cover crops: \$\$Leg\\share =
Leg\\spont \times (1 - CC\\share) + Leg\\seeded \times CC\\share\$\$

Environmental adjustments (N inhibition, temperature, water) are applied
to the weed Ndfa when the corresponding columns are present (same
mechanism as crop BNF).

## Usage

``` r
calc_weed_bnf(
  x,
  k_n_synth = 0.0035,
  k_n_org = 0.0018,
  t_opt = 22,
  t_sigma = 12,
  ai_threshold = 0.45
)
```

## Arguments

- x:

  Data frame with weed NPP data. Required columns:

  Weeds_NPP_MgN

  :   Weed NPP in Mg N.

  LandUse

  :   Land use type (Cropland, Grassland, etc.).

  Legs_Seeded

  :   Legume fraction in seeded cover crops (0-1).

  Seeded_CC_share

  :   Share of area with seeded cover crops (0-1).

  Optional columns:

  Legs_SpontWeeds

  :   Pre-computed legume fraction in spontaneous vegetation (0-1), e.g.
      differentiated by LandUse. If absent, defaults to BNF table value
      for Cropland and 0.15 for non-cropland (grassland, forest,
      dehesa), where leguminous shrubs (Genista, Ulex, Cytisus) also
      contribute.

  Environmental columns: see
  [`calc_crop_bnf`](https://eduaguilera.github.io/afsetools/reference/calc_crop_bnf.md).

- k_n_synth:

  Numeric. Rate constant for N inhibition by synthetic N (default
  0.0035).

- k_n_org:

  Numeric. Rate constant for N inhibition by organic N (default 0.0018).

- t_opt:

  Numeric. Optimal temperature (default 25).

- t_sigma:

  Numeric. Temperature Gaussian width (default 8).

## Value

Data frame with added columns:

- Weeds_Ndfa_ref:

  Reference Ndfa for weeds.

- Weeds_Ndfa:

  Adjusted Ndfa for weeds.

- Weeds_leg_share:

  Weighted legume fraction.

- f_env_weed:

  Environmental adjustment factor.

- WeedsBNF:

  Weed BNF in Mg N.

## Details

Estimates symbiotic BNF from leguminous weeds and seeded cover crops.
The legume fraction in field vegetation is a weighted average of
spontaneous weeds and seeded cover crops, based on management data.

Requires BNF object from \`load_general_data()\` (reads "Weeds" row for
reference Ndfa and default spontaneous legume share). Cover crop seeded
share is only applied on cropland.

If \`Legs_SpontWeeds\` is already present in the input data, the
function respects those values instead of overwriting with the default.
This supports LandUse-differentiated legume shares based on literature:

- Cropland weeds: ~0.05 (Storkey et al. 2012; Fried et al. 2009 —
  herbicide pressure suppresses legumes)

- Grassland/dehesa/forest understory: ~0.15 (Luscher et al. 2014 — 4-15%
  legumes in semi-natural grassland; includes proxy for N-fixing shrubs:
  Genista, Ulex, Cytisus, Retama; Dovrat et al. 2019; Viera-Rodriguez et
  al. 2005)

## References

Dovrat G et al. (2019) New Phytol 221:1361-1370. Fried G et al. (2009) J
Veg Sci 20:49-58. Luscher A et al. (2014) Grass Forage Sci 69:206-228.
Peoples MB et al. (2009) Symbiosis 48:1-17. Storkey J et al. (2012) Proc
R Soc B 279:1421-1429. Viera-Rodriguez MA et al. (2005) Can J For Res
35:1200-1209.

## Examples

``` r
if (FALSE) { # \dontrun{
load_general_data()
weed_data |> calc_weed_bnf()
} # }
```
