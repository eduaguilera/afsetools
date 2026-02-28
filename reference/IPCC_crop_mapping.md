# IPCC Crop Name Mapping

Maps Name_biomass crop classifications from Biomass_coefs to IPCC crop
categories used in IPCC_residue_coefs and IPCC_root_coefs, and to
broader crop groups used for variety adoption and RS N sensitivity.

## Format

A data frame with columns:

- Name_biomass:

  Crop name matching Biomass_coefs classification.

- IPCC_crop:

  Corresponding IPCC crop category (31 categories).

- crop_group:

  Broad crop group for adoption and root-sensitivity tables (Wheat,
  Rice, Maize, Sorghum_millet, Legumes, Root_tuber, Oilseeds, Other).

## Source

Package-internal mapping based on IPCC crop categories.
