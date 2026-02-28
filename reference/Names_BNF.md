# BNF Crop Name Mapping

Maps crop-level `Name_biomass` values (from `Biomass_coefs`) to BNF
parameter categories (`Name_BNF` in the `BNF` table). Contains 38 rows
covering all leguminous, partially leguminous, and non-symbiotic
fixation crops present in the biomass coefficients.

## Format

A data frame with 38 rows and 3 columns:

- Name_biomass:

  Crop name matching `Biomass_coefs` (join key to input data).

- Name_BNF:

  BNF parameter category (join key to `BNF` table). One of 17
  categories.

- CB_Item:

  Commodity Balance item name for linking to trade data. NA for most
  entries; populated only for major crops.

## Source

Internal classification based on FAO crop nomenclature and botanical
taxonomy.

## Details

Crops not present in this table receive NA for all BNF parameters after
joining, which correctly yields zero symbiotic BNF. Non-symbiotic BNF
still applies using the default base rate (5 kg N/ha/yr).
