# Modern Variety Adoption Timeline

Crop-group-specific regional time-series of modern (high-yielding)
variety adoption shares. Loaded at decadal resolution from the Excel
file and interpolated to annual resolution by
[`load_general_data()`](https://eduaguilera.github.io/afsetools/reference/load_general_data.md).
Used together with `HI_crop_ranges` to compute crop-specific HI
correction factors for residue estimation.

## Format

A data frame with columns:

- region_HANPP:

  World region name (8 regions matching `regions_full$region_HANPP`).

- crop_group:

  Crop group (Wheat, Rice, Maize, Sorghum_millet, Legumes, Root_tuber,
  Oilseeds, Other).

- Year:

  Year (annual from 1900 to 2020 after interpolation).

- Modern_share:

  Share of area under modern varieties (0-1).

## Source

Evenson & Gollin (2003) Science 300:758-762. Griliches (1957) hybrid
maize adoption. Dalrymple (1986) semi-dwarf wheat. Gollin et al. (2021)
roles of agricultural innovation.
