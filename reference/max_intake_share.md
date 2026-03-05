# Maximum Feed Intake Shares by Livestock Category

Maximum share of each feed item that can be allocated to each livestock
category. Used by
[`redistribute_feed()`](https://eduaguilera.github.io/afsetools/reference/redistribute_feed.md)
to cap feed distribution.

## Format

A data frame with 28 rows and 3 columns:

- Livestock_cat:

  Livestock category (e.g., Cattle_milk, Pigs, Poultry, Sheep, Goats)

- item_cbs:

  Feed item name in CBS nomenclature

- max_intake_share:

  Maximum intake share (0-1). Zero means the livestock category cannot
  consume this feed item.

## Source

Livestock_coefs.xlsx, sheet "max_intake".
