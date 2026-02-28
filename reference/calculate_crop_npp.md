# Calculate Crop Net Primary Production Components

This function replaces and enhances the previous
\`calculate_crop_npp()\` by decomposing the estimation into specialized
sub-functions that each use IPCC 2019 guidelines, literature-based
coefficients, and context-dependent adjustments for irrigation, N input,
and variety era.

The calculation pipeline is: 1. \*\*Residues\*\*:
\`calculate_crop_residues()\` estimates above-ground residue biomass
from yield using IPCC linear model + ratio ensemble. 2. \*\*Roots\*\*:
\`calculate_crop_roots()\` estimates below-ground biomass from aerial
biomass using IPCC RS ratios + reference values. 3. \*\*Assembly\*\*:
Total crop NPP = Product + Residue + Root (all in DM).

## Usage

``` r
calculate_crop_npp(Dataset, w_ipcc = 0.5, w_ref = 0.5)
```

## Arguments

- Dataset:

  Data frame with crop area and production data. Required columns:

  Name_biomass

  :   Crop name matching Biomass_coefs.

  Prod_ygpit_Mg

  :   Production in Mg fresh matter.

  Area_ygpit_ha

  :   Harvested area in hectares.

  Year

  :   Production year (for modern variety correction).

  region_HANPP

  :   Region name (for modern variety correction).

  Water_regime

  :   "Irrigated", "Rainfed", or "Mixed".

  N_input_kgha

  :   Nitrogen application rate (kg N/ha/yr).

- w_ipcc:

  Numeric (0-1). Weight for IPCC linear residue model in the residue
  estimation ensemble. Default 0.5.

- w_ref:

  Numeric (0-1). Weight for the reference root biomass in the root
  estimation ensemble. Default 0.5.

## Value

Data frame with added columns:

- Prod_MgDM:

  Product dry matter (Mg).

- Residue_MgDM:

  Above-ground residue dry matter (Mg).

- Root_MgDM:

  Below-ground root dry matter (Mg).

- Crop_NPP_MgDM:

  Total crop NPP = Prod + Residue + Root (Mg DM).

- Yield_DM_Mgha:

  Yield in Mg DM per hectare.

Intermediate coefficient columns from Biomass_coefs are dropped.

## Details

Wrapper function that calculates complete crop NPP by calling
\`calculate_crop_residues()\` for above-ground residue estimation and
\`calculate_crop_roots()\` for below-ground root estimation, then
assembles total crop NPP.

\*\*Backward compatibility\*\*: This function produces the same output
columns as the previous \`calculate_crop_npp()\`. The old function
signature using an \`HI\` parameter for dynamic harvest index is
preserved via the legacy alias \`Calculate_crop_NPP()\`.

\*\*Required context columns\*\*: The Dataset must include Year,
region_HANPP, Water_regime, and N_input_kgha columns. These drive the
IPCC context-dependent adjustments for modern variety era, irrigation
regime, and nitrogen input effects on root allocation.

Requires from \`load_general_data()\`: - \`Biomass_coefs\` -
\`IPCC_residue_coefs\`, \`IPCC_root_coefs\`, \`IPCC_crop_mapping\` -
\`Irrigation_adj\`, \`Modern_variety_adoption\`, \`N_input_RS_adj\`

## References

IPCC (2019) 2019 Refinement to the 2006 IPCC Guidelines, Vol.4, Ch.11.

Bolinder et al. (2007) Root biomass and shoot to root ratios. J Agric
Sci 145:127-137.

Krausmann et al. (2013) Global human appropriation of net primary
production. PNAS 110:10324-10329.

## Examples

``` r
if (FALSE) { # \dontrun{
load_general_data()

# Dataset must include all required columns
crop_npp <- crop_data |>
  calculate_crop_npp(w_ipcc = 0.5, w_ref = 0.5)
} # }
```
