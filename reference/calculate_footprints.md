# Calculate Complete Environmental Footprints Along Supply Chains

Main workflow function that traces environmental footprints through
global supply chains, from primary production through processing, feed,
and final products. This function orchestrates the entire footprint
calculation process and returns all intermediate and final footprint
data frames.

## Usage

``` r
calculate_footprints(
  CBS,
  Primary_all,
  Impact_prod,
  Crop_NPPr_NoFallow,
  DTM = NULL,
  trade_mode = "gt"
)
```

## Arguments

- CBS:

  Commodity Balance Sheets data frame with columns: Year, area,
  area_code, item, item_code, Element, Value

- Primary_all:

  Primary production data frame with crop areas and production

- Impact_prod:

  Impact data at production level with columns: Year, area_code,
  item_code, Impact, Value, u_FU

- Crop_NPPr_NoFallow:

  Crop NPP data excluding fallow periods

- DTM:

  Detailed Trade Matrix data (optional, depending on trade_mode)

- trade_mode:

  Character string: "gt" for gross trade or "dtm" for detailed trade
  matrix. Default is "gt".

## Value

A named list containing all footprint data frames:

- FP_prim:

  Primary production footprints with economic allocation

- FP_prim_ds:

  Primary product footprints including domestic supply

- FP_processed_raw:

  Processed product footprints (raw calculation)

- FP_processed_ds:

  Processed product footprints with domestic supply

- FP_feed:

  Feed product footprints

- FP_feed_ds:

  Feed product footprints with domestic supply

- FP_final:

  Final comprehensive footprint data frame

- Seed_share:

  Calculated seed shares by crop

- draught_shares:

  Draught animal allocation shares

## Details

This function implements a complete footprint accounting system that:

- Calculates seed shares and removes them from production

- Allocates impacts to co-products using economic allocation

- Traces impacts through processing chains

- Accounts for international trade (gross trade or bilateral trade
  matrix)

- Handles feed products and livestock production

- Allocates draught animal services to crop production

The calculation requires that \`load_general_data()\` has been called
first to load all necessary coefficient tables and classification data.

## Examples

``` r
if (FALSE) { # \dontrun{
library(afsetools)
load_general_data()

# Calculate footprints using gross trade
footprints <- calculate_footprints(
  CBS = my_cbs_data,
  Primary_all = my_primary_data,
  Impact_prod = my_impact_data,
  Crop_NPPr_NoFallow = my_npp_data,
  trade_mode = "gt"
)

# Access individual footprint tables
fp_primary <- footprints$FP_prim
fp_final <- footprints$FP_final
} # }
```
