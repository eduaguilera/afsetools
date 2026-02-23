# Calculate Complete Environmental Footprints Along Supply Chains

Main workflow function that traces environmental footprints through
global supply chains, from primary production through processing, feed,
and final products. This function orchestrates the entire footprint
calculation process and returns all intermediate and final footprint
data frames.

## Usage

``` r
calculate_footprints(
  cbs,
  primary,
  impact_prod,
  crop_nppr,
  feed_intake,
  dtm = NULL
)
```

## Arguments

- cbs:

  Commodity Balance Sheets table. Required columns: \`Year\`, \`area\`,
  \`area_code\`, \`item_cbs\`, \`item_code_cbs\`, \`Element\`,
  \`Value\`. This function uses at least the following \`Element\`
  values: \`Production\`, \`Import\`, \`Export\`, \`Seed\`,
  \`Processing\`, and \`Domestic_supply\`.

- primary:

  Primary production / co-product table (new schema). Required columns:
  \`Year\`, \`area\`, \`area_code\`, \`item_prod\`, \`item_code_prod\`,
  \`unit\`, \`Value\`, \`item_cbs\`, \`item_code_cbs\`, \`Live_anim\`,
  \`Live_anim_code\`, \`Relative_residue_price\`. The workflow expects
  \`unit == "tonnes"\` rows and (for draught allocation) \`unit ==
  "LU"\` livestock rows.

- impact_prod:

  Production impact table (new schema). Required columns: \`Year\`,
  \`area\`, \`item_prod\`, \`item_code_prod\`, \`Impact\`, \`Value\`,
  \`u_FU\`. \`Value\` is treated as the functional unit amount (\`FU\`)
  when joining production impacts to primary production.

- crop_nppr:

  Crop NPP / residue table. Required columns: \`Year\`, \`area\`,
  \`item_prod\`, \`item_cbs\`, \`Product_residue\`, \`Prod_ygpit_Mg\`.
  Residue flows are taken from rows where \`Product_residue ==
  "Residue"\`.

- feed_intake:

  Feed intake table (new schema). Required columns: \`Year\`, \`area\`,
  \`area_code\`, \`Live_anim\`, \`item_cbs\`, \`item_code_cbs\`,
  \`Supply\`, \`Intake_DM\`.

- dtm:

  Optional detailed trade matrix. If \`NULL\` (default), the function
  uses gross-trade mode (\`calc_avail_fp_gt()\`). If provided, it
  automatically uses detailed bilateral trade mode
  (\`calc_avail_fp_dtm()\`). Required columns when provided: \`Year\`,
  \`area_code\`, \`area_code_p\`, \`area_p\`, \`item_code\`,
  \`Element\`, \`Impact\`, \`Country_share\` (with \`Element ==
  "Import"\` rows used).

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

The calculation requires that \`load_general_data()\` has been called
first to load all necessary coefficient tables and classification data.

## Examples

``` r
if (FALSE) { # \dontrun{
library(afsetools)
load_general_data()

# Calculate footprints using gross trade (omit dtm)
footprints <- calculate_footprints(
  cbs = my_cbs_data,
  primary = my_primary_data,
  impact_prod = my_impact_data,
  crop_nppr = my_npp_data,
  feed_intake = my_feed_intake
)

# To use bilateral trade, pass dtm = my_dtm_data

# Access individual footprint tables
fp_primary <- footprints$FP_prim
fp_final <- footprints$FP_final
} # }
```
