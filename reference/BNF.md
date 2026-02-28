# Biological Nitrogen Fixation Parameters

Parameters for calculating biological nitrogen fixation (BNF) by crop or
system type. Contains 17 rows covering pure legumes (grain and fodder),
non-symbiotic fixation crops (rice, sugarcane), and partial legume
systems (mixed swards, meadows, fallow, weeds). Used by
[`calc_crop_bnf`](https://eduaguilera.github.io/afsetools/reference/calc_crop_bnf.md),
[`calc_nonsymbiotic_bnf`](https://eduaguilera.github.io/afsetools/reference/calc_nonsymbiotic_bnf.md),
and
[`calc_bnf`](https://eduaguilera.github.io/afsetools/reference/calc_bnf.md).

## Format

A data frame with 17 rows and 7 columns:

- Name_BNF:

  Crop or system name (join key from `Names_BNF`).

- Ndfa:

  Proportion of nitrogen derived from atmosphere (0-1). NA for
  non-symbiotic crops (rice, sugarcane).

- NHI:

  Nitrogen harvest index: fraction of above-ground N in harvested
  product. NA for fallow/weeds (Anglade method not applicable). Values
  from Anglade et al. (2015).

- BGN:

  Below-ground N factor: ratio of total plant N to above-ground N. NA
  for fallow/weeds.

- kgNha:

  Non-symbiotic BNF base rate (kg N/ha/yr). Only set for rice (33) and
  sugarcane (25); NA for other crops (default 5 used by
  `calc_nonsymbiotic_bnf`).

- Leguminous_share:

  Fraction of leguminous species in the crop or system (0-1). Pure
  legumes = 1, rice/sugarcane = 0, mixed swards = 0.33, weeds = 0.20,
  etc.

- Source:

  Literature reference for the parameter values.

## Source

Herridge et al. (2008) Plant and Soil 311:1-18; Anglade et al. (2015)
Nutr. Cycl. Agroecosyst. 103:37-56; Lassaletta et al. (2014)
Biogeosciences 11:2889-2907; Ladha et al. (2016) Scientific Reports
6:19355.
