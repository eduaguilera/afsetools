# Calculate Crop Below-Ground (Root) Biomass

Three estimation approaches are combined:

1\. \*\*IPCC root:shoot ratio\*\* (IPCC 2019 Refinement, Vol.4, Ch.11):
\`Root_DM = Aerial_DM \* RS_ratio\`, where RS_ratio is adjusted for N
input and irrigation from the \`IPCC_root_coefs\` table.

2\. \*\*Reference root biomass\*\* (Biomass_coefs
\`BG_Biomass_kgDM_ha\`): A fixed per-hectare root biomass value,
independent of yield. \`Root_ref = BG_Biomass_kgDM_ha / 1000 \*
Area_ha\`

3\. \*\*Biomass_coefs RS fallback\*\*: When IPCC coefficients are not
available, uses the \`Root_Shoot_ratio\` from Biomass_coefs.

The final estimate averages the RS-based and reference-based approaches,
capped at 3x the default RS ratio to prevent unrealistic values.

## Usage

``` r
calculate_crop_roots(Dataset, w_ref = 0.5)
```

## Arguments

- Dataset:

  Data frame with above-ground biomass already calculated. Required
  columns:

  Name_biomass

  :   Crop name matching Biomass_coefs.

  Prod_MgDM

  :   Product dry matter (Mg).

  Residue_MgDM

  :   Residue dry matter (Mg).

  Area_ygpit_ha

  :   Harvested area in hectares.

  Water_regime

  :   "Irrigated", "Rainfed", or "Mixed".

  N_input_kgha

  :   Nitrogen application rate (kg N/ha/yr).

- w_ref:

  Numeric weight for the reference root biomass approach in the ensemble
  (0-1). Default 0.5. The RS-based approach gets weight \`1 - w_ref\`.

## Value

Data frame with added column:

- Root_MgDM:

  Estimated below-ground root dry matter (Mg).

## Details

Estimates root biomass dry matter from above-ground biomass using an
ensemble of IPCC root:shoot ratios, reference root biomass values, and
adjustments for N input and irrigation regime.

\*\*N-input adjustment\*\*: Higher nitrogen availability decreases root
allocation due to functional equilibrium (Poorter & Nagel 2000). The
\`N_input_RS_adj\` table classifies N rates into 5 classes with
multiplicative RS factors (0.80 for \>200 kg N/ha to 1.20 for \<20 kg
N/ha).

\*\*Irrigation adjustment\*\*: Irrigated crops develop shallower root
systems (Benjamin et al. 2014). Factor from \`Irrigation_adj\` table
(default 0.85 for irrigated).

\*\*RS selection priority\*\*: IPCC_root_coefs \> Biomass_coefs
fallback. When irrigation or N-input columns are present, the IPCC table
provides context-specific RS values; otherwise the default
\`RS_default\` is used.

\*\*Ensemble and cap\*\*: Final root = weighted average of RS-based and
reference-based estimates, capped at 3x the default RS ratio times
aerial biomass.

Requires from \`load_general_data()\`: - \`Biomass_coefs\`
(Root_Shoot_ratio, BG_Biomass_kgDM_ha) - \`IPCC_root_coefs\`,
\`IPCC_crop_mapping\` - \`N_input_RS_adj\`, \`Irrigation_adj\`

## References

IPCC (2019) 2019 Refinement to the 2006 IPCC Guidelines for National
Greenhouse Gas Inventories, Volume 4, Chapter 11.

Bolinder, M.A. et al. (2007) Root biomass and shoot to root ratios as
related to above ground biomass. J Agric Sci 145:127-137.

Poorter, H. & Nagel, O. (2000) The role of biomass allocation in the
growth response of plants to different levels of light, CO2, nutrients
and water. New Phytologist 147:135-147.

Benjamin, J.G. et al. (2014) Water deficit effects on root distribution
of soybean, field pea and chickpea. Agronomy Journal 106:2033-2040.

## Examples

``` r
if (FALSE) { # \dontrun{
load_general_data()
crop_data |>
  calculate_crop_residues() |>
  calculate_crop_roots(w_ref = 0.5)
} # }
```
