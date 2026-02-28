# Calculate Total Biological Nitrogen Fixation

Total BNF is the sum of three components: \$\$BNF = CropBNF + WeedsBNF +
NSBNF\$\$

Each component is estimated using reference parameters from the BNF data
table, optionally adjusted for local environmental conditions (N inputs,
temperature, moisture, SOM, pH) when these variables are present in the
input data.

\*\*When no environmental columns are present\*\*, the function produces
results comparable to \`Calc_N_fix()\` (all adjustment factors = 1),
ensuring backward compatibility.

## Usage

``` r
calc_bnf(
  x,
  k_n_symb_synth = 0.0035,
  k_n_symb_org = 0.0018,
  k_n_ns_synth = 0.005,
  k_n_ns_org = 0.0025,
  t_opt = 25,
  t_sigma_symb = 8,
  t_sigma_ns = 10,
  nsbnf_default_kgha = 5,
  k_som = 2,
  som_ref = 2.5,
  ph_opt = 6.8,
  ph_sigma = 1.5,
  k_clay = 20,
  clay_ref = 25
)
```

## Arguments

- x:

  Data frame with crop NPP and area data. Required columns:

  Name_biomass

  :   Crop name matching Names_BNF.

  Crop_NPP_MgN

  :   Crop NPP in Mg N.

  Prod_MgN

  :   Product nitrogen in Mg.

  Weeds_NPP_MgN

  :   Weed NPP in Mg N.

  LandUse

  :   Land use type (Cropland, etc.).

  Area_ygpit_ha

  :   Harvested area in hectares.

  Legs_Seeded

  :   Legume fraction in seeded cover crops.

  Seeded_CC_share

  :   Share of area with seeded CCs.

  Optional environmental columns (auto-detected):

  N_synth_kgha

  :   Synthetic N fertilizer (kg N/ha).

  N_org_kgha

  :   Organic N inputs (kg N/ha).

  TMP

  :   Mean temperature (degrees C).

  WaterInput_mm

  :   Water input (mm). Or provide precip_mm and optionally irrig_mm.

  PET_mm

  :   Potential evapotranspiration (mm).

  SOM_pct

  :   Soil organic matter (percent).

  soil_pH

  :   Soil pH.

- k_n_symb_synth:

  Numeric. Synthetic N inhibition constant for symbiotic BNF (default
  0.0035).

- k_n_symb_org:

  Numeric. Organic N inhibition constant for symbiotic BNF (default
  0.0018).

- k_n_ns_synth:

  Numeric. Synthetic N inhibition constant for non-symbiotic BNF
  (default 0.005).

- k_n_ns_org:

  Numeric. Organic N inhibition constant for non-symbiotic BNF (default
  0.0025).

- t_opt:

  Numeric. Optimal temperature (default 25).

- t_sigma_symb:

  Numeric. Temperature width for symbiotic (default 8).

- t_sigma_ns:

  Numeric. Temperature width for non-symbiotic (default 10).

- nsbnf_default_kgha:

  Numeric. Default NSBNF base rate (default 5 kg N/ha/yr).

- k_som:

  Numeric. SOM half-saturation (default 2.0).

- som_ref:

  Numeric. Reference SOM (default 2.5).

- ph_opt:

  Numeric. Optimal pH (default 6.8).

- ph_sigma:

  Numeric. pH Gaussian width (default 1.5).

- k_clay:

  Numeric. Clay half-saturation for NSBNF (default 20).

- clay_ref:

  Numeric. Reference clay percent (default 25).

## Value

Data frame with all intermediate columns plus:

- Fert_type:

  "BNF" character flag.

- CropBNF:

  Symbiotic crop BNF, NPP method (Mg N).

- CropBNF2:

  Symbiotic crop BNF, Anglade method (Mg N).

- WeedsBNF:

  Symbiotic weed/cover crop BNF (Mg N).

- NSBNF:

  Non-symbiotic BNF (Mg N).

- BNF:

  Total BNF (Mg N).

- f_N_symb, f_temp_symb, f_water_symb:

  Symbiotic adjustment factors.

- f_N_ns, f_temp_ns, f_water_ns, f_SOM_ns, f_pH_ns, f_clay_ns:

  Non-symbiotic adjustment factors.

## Details

Master function that calculates all BNF components (symbiotic crop,
symbiotic weed/cover crop, and non-symbiotic) using literature-based
environmental modifiers. Replaces and improves upon \`Calc_N_fix()\`.

\## Symbiotic BNF (crop legumes)

Two methods are calculated in parallel: - \*\*NPP method\*\*: Uses total
crop NPP nitrogen and Ndfa. - \*\*Anglade method\*\*: Uses product
nitrogen with below-ground N (BGN) and N harvest index (NHI)
corrections.

Ndfa is adjusted for N fertilization, temperature, and water.

\## Symbiotic BNF (weeds and cover crops)

Based on weed NPP nitrogen, with legume share as a weighted average of
spontaneous weeds and seeded cover crops. Same environmental adjustments
as crop legume BNF.

\## Non-symbiotic BNF

Base rate from BNF table (rice 33, sugarcane 25 kg N/ha/yr) or default
(5 kg N/ha/yr). Adjusted for: - N inputs (free-living fixers avoid
fixation cost when N available) - Temperature (broad Gaussian, sigma =
10) - Water (aridity index threshold) - SOM (C energy for heterotrophic
fixers, Michaelis-Menten) - pH (Gaussian around 6.8)

## References

Anglade J et al. (2015) Nutr. Cycl. Agroecosyst. 103:37-56.

Cleveland CC et al. (1999) Global Biogeochem. Cycles 13:623-645.

Dynarski KA, Houlton BZ (2018) New Phytologist 217:68-85.

Herridge DF et al. (2008) Plant and Soil 311:1-18.

Hungria M, Vargas MAT (2000) Field Crops Res. 65:151-164.

Ladha JK et al. (2016) Scientific Reports 6:19355.

Peoples MB et al. (2009) Symbiosis 48:1-17.

Reed SC et al. (2011) Annu. Rev. Ecol. Evol. Syst. 42:489-512.

Salvagiotti F et al. (2008) Field Crops Res. 108:1-13.

Serraj R et al. (1999) Plant Physiology 120:577-586.

Urquiaga S et al. (2012) Plant and Soil 356:5-21.

## See also

[`calc_crop_bnf`](https://eduaguilera.github.io/afsetools/reference/calc_crop_bnf.md)
for standalone crop legume BNF.
[`calc_weed_bnf`](https://eduaguilera.github.io/afsetools/reference/calc_weed_bnf.md)
for standalone weed/cover crop BNF.
[`calc_nonsymbiotic_bnf`](https://eduaguilera.github.io/afsetools/reference/calc_nonsymbiotic_bnf.md)
for standalone NSBNF.

## Examples

``` r
if (FALSE) { # \dontrun{
load_general_data()

# Backward-compatible usage (no environmental data):
bnf_results <- npp_data |> calc_bnf()

# With full environmental characterisation:
bnf_results <- npp_data |>
  dplyr::mutate(
    N_synth_kgha = 80, N_org_kgha = 20,
    TMP = 18, precip_mm = 650, irrig_mm = 100,
    PET_mm = 900, SOM_pct = 3.0, soil_pH = 6.5
  ) |>
  calc_bnf()
} # }
```
