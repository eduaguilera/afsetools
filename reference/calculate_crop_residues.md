# Calculate Crop Above-Ground Residue Biomass

Three estimation methods are combined:

1\. \*\*IPCC linear model\*\* (IPCC 2019 Refinement, Vol.4, Ch.11, Table
11.1a): \`AG_residue_DM = Slope \* Yield_DM + Intercept\`

2\. \*\*Biomass_coefs ratio model\*\* (package default): \`Residue_FM =
Yield_FM \* kg_residue_kg_product_FM\`

3\. \*\*Modern variety adjustment\*\* (Krausmann et al. 2013; Evenson &
Gollin 2003): Historical correction for pre-Green-Revolution varieties
that had lower harvest index (more residue per unit product).

The final estimate is a weighted mean of methods 1 and 2, with
irrigation and variety-era adjustments applied.

## Usage

``` r
calculate_crop_residues(Dataset, w_ipcc = 0.5, simple = FALSE)
```

## Arguments

- Dataset:

  Data frame with crop production data. Required columns:

  Name_biomass

  :   Crop name matching Biomass_coefs classification.

  Prod_ygpit_Mg

  :   Production in Mg fresh matter.

  Area_ygpit_ha

  :   Harvested area in hectares.

  Year

  :   Year of production (for modern variety adjustment).

  region_HANPP

  :   Region name matching Modern_variety_adoption.

  Water_regime

  :   One of "Irrigated", "Rainfed", or "Mixed".

- w_ipcc:

  Numeric weight for the IPCC linear model in the ensemble (0-1).
  Default 0.5. The Biomass_coefs ratio gets weight \`1 - w_ipcc\`.

- simple:

  Logical. If \`TRUE\`, forces all context-dependent corrections off
  (irrigation, modern-variety HI), setting all adjustment factors to 1 —
  even if the relevant columns exist. If \`FALSE\` (default), each
  adjustment is \*\*auto-detected\*\*: it activates only when its
  required columns are present in \`Dataset\`. Specifically: -
  Irrigation adjustment requires \`Water_regime\` - Modern variety HI
  correction requires \`Year\` + \`region_HANPP\`

## Value

Data frame with added columns:

- Prod_MgDM:

  Product dry matter (Mg).

- Residue_MgDM:

  Estimated above-ground residue dry matter (Mg).

- Yield_DM_Mgha:

  Yield in Mg DM per hectare (intermediate).

## Details

Estimates crop residue (straw, stover, etc.) dry matter from yield data
using an ensemble of IPCC 2019 linear models, Biomass_coefs ratios, and
context-dependent adjustments for irrigation, N input, and modern
variety adoption.

\*\*IPCC linear model\*\*: For each crop mapped via
\`IPCC_crop_mapping\`, the per-hectare residue is estimated as:
\`Residue_IPCC_Mgha = Slope_AG \* Yield_DM_Mgha + Intercept_AG_MgDMha\`
This captures the empirical finding (Lassaletta et al. 2014) that
residue production is not purely proportional to yield — there is a base
structural component (intercept).

\*\*Irrigation adjustment\*\*: Irrigated crops may have different
harvest index (Sadras 2007). The base adjustment factor (0.90 for
irrigated) from \`Irrigation_adj\` is scaled by crop-group-specific
sensitivity from \`Irr_residue_crop_adj\` (Ludemann et al. 2025). Only
maize shows clear irrigated RPR \< rainfed; wheat, rice, legumes do not.

\*\*Modern variety correction\*\*: Pre-Green-Revolution varieties had
lower harvest index. The \`Modern_variety_adoption\` table provides
crop-group- specific regional time-series of adoption rates from Evenson
& Gollin (2003), interpolated to annual resolution. Combined with
\`HI_crop_ranges\` (crop-specific HI gap factors), this computes:
\`HI_correction = 1 + (1 - Modern_share) \* (HI_gap_factor - 1)\`.

\*\*Ensemble\*\*: The final residue estimate blends both methods:
\`Residue_MgDM = w_ipcc \* IPCC_estimate + (1 - w_ipcc) \*
ratio_estimate\`

Requires these objects from \`load_general_data()\`: - \`Biomass_coefs\`
(with \`Product_kgDM_kgFM\`, \`Residue_kgDM_kgFM\`,
\`kg_residue_kg_product_FM\`) - \`IPCC_residue_coefs\`,
\`IPCC_crop_mapping\` (with \`crop_group\` column) - \`Irrigation_adj\`,
\`Modern_variety_adoption\`, \`HI_crop_ranges\`

## References

IPCC (2019) 2019 Refinement to the 2006 IPCC Guidelines for National
Greenhouse Gas Inventories, Volume 4, Chapter 11, Table 11.1a.

Krausmann et al. (2013) Global human appropriation of net primary
production doubled in the 20th century. PNAS 110:10324-10329.

Lassaletta et al. (2014) 50 year trends in nitrogen use efficiency of
world cropping systems. Biogeosciences 11:2889-2907.

Sadras (2007) Evolutionary aspects of the trade-off between seed size
and number in crops. Field Crops Research 100:125-138.

## Examples

``` r
if (FALSE) { # \dontrun{
load_general_data()
crop_data |>
  calculate_crop_residues(w_ipcc = 0.5)
} # }
```
