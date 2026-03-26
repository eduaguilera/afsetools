# Changelog

## afsetools 0.3.0

### New features

- **BNF functions module** (`R/bnf_functions.R`): 5 exported functions
  for biological nitrogen fixation —
  [`calc_crop_bnf()`](https://eduaguilera.github.io/afsetools/reference/calc_crop_bnf.md),
  [`calc_weed_bnf()`](https://eduaguilera.github.io/afsetools/reference/calc_weed_bnf.md),
  [`calc_nonsymbiotic_bnf()`](https://eduaguilera.github.io/afsetools/reference/calc_nonsymbiotic_bnf.md),
  [`calc_bnf()`](https://eduaguilera.github.io/afsetools/reference/calc_bnf.md),
  and
  [`summarize_bnf()`](https://eduaguilera.github.io/afsetools/reference/summarize_bnf.md)
  — with literature-based environmental modifiers for temperature, water
  stress, soil organic matter, pH, and clay content.

- **IPCC biomass coefficients**: 6 new sheets in `Biomass_coefs.xlsx`
  with crop residue, root, and mapping coefficients verified against
  IPCC 2006 Vol.4 Ch.11 Table 11.2.

- **Data-driven NPP model**: NPP model coefficients extracted to Excel
  (`NPP_model_coefs` sheet).
  [`calculate_potential_npp()`](https://eduaguilera.github.io/afsetools/reference/calculate_potential_npp.md)
  now reads coefficients from the data table instead of hardcoded
  literals.

- **Simple mode** for crop NPP functions:
  [`calculate_crop_npp()`](https://eduaguilera.github.io/afsetools/reference/calculate_crop_npp.md),
  [`calculate_crop_residues()`](https://eduaguilera.github.io/afsetools/reference/calculate_crop_residues.md),
  [`calculate_crop_roots()`](https://eduaguilera.github.io/afsetools/reference/calculate_crop_roots.md),
  and
  [`calculate_npp_dm_c_n()`](https://eduaguilera.github.io/afsetools/reference/calculate_npp_dm_c_n.md)
  gain a `mode` parameter (`"full"` / `"simple"`) that auto-detects
  available columns and adjusts calculations accordingly.

- **Crop-group-specific residue/root helper data**: new lookup tables
  for residue shares, weed NPP scaling, fallow cover, and root-shoot N
  response.

- **Feed distribution**:
  [`redistribute_feed()`](https://eduaguilera.github.io/afsetools/reference/redistribute_feed.md)
  function for multi-level feed allocation with diagnostics, plus
  `max_intake_share` loaded from `Livestock_coefs.xlsx`.

### Bug fixes

- Fixed crop-specific irrigation residue factor and reduced rice
  `HI_gap_factor`.
- Fixed N-input root-shoot adjustment:
  [`calculate_crop_roots()`](https://eduaguilera.github.io/afsetools/reference/calculate_crop_roots.md)
  now uses the `N_input_RS_adj` data table via
  [`findInterval()`](https://rdrr.io/r/base/findInterval.html) instead
  of ignoring it.
- Added `DescTools` and `zoo` to package dependencies (previously
  undeclared).
- Fixed [`stats::quantile()`](https://rdrr.io/r/stats/quantile.html)
  call in
  [`calculate_footprints()`](https://eduaguilera.github.io/afsetools/reference/calculate_footprints.md)
  draught share calculation (was missing namespace prefix).
- Replaced non-ASCII box-drawing characters in
  [`redistribute_feed()`](https://eduaguilera.github.io/afsetools/reference/redistribute_feed.md)
  diagnostics with ASCII equivalents for portable package compliance.
- Fixed `%!in%` operator documentation to avoid invalid Rd `\name`
  field.
- Moved unused `data.table` and `readxl` from Imports to Suggests.
- Added comprehensive
  [`globalVariables()`](https://rdrr.io/r/utils/globalVariables.html)
  declarations for all NSE column names across the package (eliminates R
  CMD check NOTEs).

### Documentation

- Expanded `DATA_REFERENCE.md` to 80+ objects with IPCC and NPP
  sections.
- Complete roxygen2 rewrite for
  [`calculate_potential_npp()`](https://eduaguilera.github.io/afsetools/reference/calculate_potential_npp.md)
  and NPP model coefficient documentation.
- Added `Names_BNF`, `Pure_legs`, `NPP_model_coefs`, and
  `max_intake_share` to pkgdown reference index.

## afsetools 0.2.0

- Removed `Filling`, `FillingProxy`, `fill_na_with_sum` (migrated to
  `whep`).
- Cross-repo dependency fixes and code audit.
- Added
  [`redistribute_feed()`](https://eduaguilera.github.io/afsetools/reference/redistribute_feed.md)
  function.
- Aligned
  [`calculate_footprints()`](https://eduaguilera.github.io/afsetools/reference/calculate_footprints.md)
  with General reference data.
