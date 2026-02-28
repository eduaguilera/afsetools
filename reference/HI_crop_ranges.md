# Crop-Group Harvest Index Ranges

Traditional and modern harvest index values per crop group, and the
derived HI gap factor that quantifies how much more residue is produced
per unit product under traditional versus modern varieties. Used by
[`calculate_crop_residues()`](https://eduaguilera.github.io/afsetools/reference/calculate_crop_residues.md)
to compute crop-specific `HI_correction_factor` from
`Modern_variety_adoption` shares.

## Format

A data frame with columns:

- crop_group:

  Crop group name (8 groups).

- HI_traditional:

  Typical harvest index of traditional/landrace varieties
  (dimensionless, 0-1).

- HI_modern:

  Typical harvest index of modern/improved varieties (dimensionless,
  0-1).

- HI_gap_factor:

  Ratio of residue:product under traditional versus modern HI:
  ((1-HI_trad)/HI_trad) / ((1-HI_mod)/HI_mod).

- Source:

  Literature source references.

## Source

Austin et al. (1980) J Agric Sci 94:675-689. Hay (1995) Ann Appl Biol
126:1-36. Khush (2001) Plant Mol Biol 35:25-34. Lorenz et al. (2010)
Agron J 102:935-947. Unkovich et al. (2010) ACIAR Monograph 136.
Sinclair (1998) Field Crops Res 56:171-181.
