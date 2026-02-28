# IPCC Crop Residue Coefficients

Linear model coefficients for estimating above-ground crop residue
biomass from yield, based on IPCC 2006 Guidelines (Vol.4, Ch.11, Table
11.2).

## Format

A data frame with columns:

- IPCC_crop:

  IPCC crop category name.

- Slope_AG:

  Slope of the linear residue model (Mg DM residue per Mg DM yield per
  hectare).

- Intercept_AG_MgDMha:

  Intercept (Mg DM / ha), representing structural baseline residue
  production.

- RS_ratio_IPCC:

  Default root-to-shoot ratio from IPCC.

- N_AG_residue:

  Nitrogen content of above-ground residue (kg N / kg DM).

- N_BG_residue:

  Nitrogen content of below-ground residue (kg N / kg DM).

- Source:

  Literature source reference.

## Source

IPCC (2006) Guidelines for National Greenhouse Gas Inventories, Vol.4,
Ch.11, Table 11.2. Bolinder et al. (2007). Gan et al. (2009).
