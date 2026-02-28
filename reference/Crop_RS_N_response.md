# Crop-Group Root:Shoot N-Response Sensitivity

Crop-group-specific scaling factors for the root:shoot ratio response to
soil nitrogen availability. Used by
[`calculate_crop_roots()`](https://eduaguilera.github.io/afsetools/reference/calculate_crop_roots.md)
to modulate the generic `N_input_RS_adj` factor per crop group. Value
1.0 means full response to N; 0.0 means no response.

## Format

A data frame with columns:

- crop_group:

  Crop group name (8 groups).

- RS_N_sensitivity:

  Sensitivity multiplier (0-1.1). Applied as: effective_factor = 1 +
  (raw_factor - 1) \* RS_N_sensitivity.

- Source:

  Literature source references.

## Source

Poorter & Nagel (2000) New Phytologist 147:135-147. Peng & Ismail (2004)
root allometry in rice systems. Unkovich et al. (2010) ACIAR Monograph
136.
