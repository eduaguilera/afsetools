# Calculate Complete Environmental Footprints Along Supply Chains

Main workflow function that traces environmental footprints through
global supply chains, from primary production through processing, feed,
and final products. This function orchestrates the entire footprint
calculation process and returns all intermediate and final footprint
data frames.

## Usage

``` r
calculate_footprints(dtm = NULL)
```

## Arguments

- dtm:

  Optional detailed trade matrix. If \`NULL\` (default), the function
  uses gross-trade mode (\`calc_avail_fp_gt()\`). If provided, it
  automatically uses detailed bilateral trade mode
  (\`calc_avail_fp_dtm()\`). Required columns when provided: \`Year\`,
  \`area_code\`, \`area_code_p\`, \`area_p\`, \`item_code_cbs\`,
  \`Element\`, \`Country_share\` (with \`Element == "Import"\` rows
  used). \`Impact\` is generated internally by expanding \`dtm\` rows
  across impacts present in \`Impact_prod\` for each year.

## Value

A named list of intermediate and final footprint tables, including:
\`FP_prim\`, \`FP_prim_i\`, \`FP_prim_i_global\`, \`FP_prim_ds\`,
\`FP_prim_ds_i\`, \`FP_processed_raw\`, \`FP_processed_raw_i\`,
\`FP_processed_ds\`, \`FP_processed_ds_i\`, \`FP_reprocessed_raw\`,
\`FP_reprocessed_raw_i\`, \`FP_raw_all\`, \`FP_feed_raw\`, \`FP_feed\`,
\`FP_feed_i\`, \`FP_feed_ds\`, \`FP_feed_ds_i\`, \`FP_ioc\`, \`FP_i\`,
\`FP_final\`, \`Seed_share\`, \`draught_shares\`, \`Import_share\`, and
\`Processing_shares\`.

## Details

This function implements a complete footprint accounting system that:

- Calculates seed shares and removes them from production

- Allocates impacts to co-products using economic allocation

- Traces impacts through processing chains

- Accounts for international trade (gross trade when \`dtm\` is
  \`NULL\`, bilateral trade matrix when \`dtm\` is provided)

- Handles feed products and livestock production

- Allocates draught animal services to crop production

`calculate_footprints()` only takes `dtm` explicitly. All other required
inputs are read implicitly from objects in the calling environment.

Required workflow objects (created outside
[`load_general_data()`](https://eduaguilera.github.io/afsetools/reference/load_general_data.md)):

- `CBS`:

  Commodity balance table. Columns used: `Year`, `area`, `area_code`,
  `item_cbs`, `item_code_cbs`, `Element`, `Value`.

- `Primary_all`:

  Primary production / co-product table. Columns used: `Year`, `area`,
  `area_code`, `item_prod`, `item_code_prod`, `unit`, `Value`,
  `item_cbs`, `item_code_cbs`, `Live_anim`, `Live_anim_code`.

- `Impact_prod`:

  Production impact table. Columns used: `Year`, `area`,
  `item_code_prod`, `Impact`, `Value`, `u_FU` (and optionally
  `item_prod`).

- `Crop_NPPr_NoFallow`:

  Crop NPP / residue table. Columns used: `Year`, `area`, `item_prod`,
  `item_cbs`, `Product_residue`, `Prod_ygpit_Mg`.

- `Feed_intake`:

  Feed intake table. Columns used: `Year`, `area`, `Live_anim`,
  `item_cbs`, `item_code_cbs`, `Supply`, `Intake_DM`. `area_code` is
  recovered from footprint tables during the feed join.

- `Primary_prices`:

  Primary product prices. Columns used: `Year`, `item_code_prod`,
  `Price`.

- `CBS_item_prices`:

  CBS item prices. Columns used: `Year`, `Element`, `item_cbs`,
  `item_code_cbs`, `Price`.

- `Processing_coefs`:

  Processing conversion coefficients. Columns used: `Year`, `area_code`,
  `item_code_cbs`, `item_cbs`, `cf`.

- `Relative_residue_price`:

  Numeric scalar used to value residues relative to product prices.

Required auxiliary objects loaded by
[`load_general_data()`](https://eduaguilera.github.io/afsetools/reference/load_general_data.md):

- `items_full` (sheet `items_full` in `Codes_coefs.xlsx`):

  Columns used: `item_cbs`, `item_code_cbs`, `group`.

- `items_prod_full` (sheet `items_prod_full` in `Codes_coefs.xlsx`):

  Columns used: `item_prod`, `item_code_prod`, `Name_biomass`.

- `Animals_codes` (sheet `Animals_codes` in `Codes_coefs.xlsx`):

  Columns used: `item_cbs`, `item_code_cbs`.

- `Primary_double` (sheet `Primary_double` in `Codes_coefs.xlsx`):

  Columns used (via
  [`Prepare_prim()`](https://eduaguilera.github.io/afsetools/reference/Prepare_prim.md)):
  `Item_area`, `item_prod`, `item_code_prod`, `Multi_type`.

- `Biomass_coefs` (`Biomass_coefs.xlsx`):

  Columns used: `Name_biomass`, `Product_kgDM_kgFM`, `Product_kgN_kgDM`.

## Examples

``` r
if (FALSE) { # \dontrun{
library(afsetools)
load_general_data()

# Requires workflow objects in the environment:
# CBS, Primary_all, Impact_prod, Crop_NPPr_NoFallow, Feed_intake,
# Primary_prices, CBS_item_prices, Processing_coefs, Relative_residue_price

# Calculate footprints using gross trade (omit dtm)
footprints <- calculate_footprints()

# To use bilateral trade, pass dtm = my_dtm_data (or dtm = DTM)

# Access individual footprint tables
fp_primary <- footprints$FP_prim
fp_final <- footprints$FP_final
} # }
```
