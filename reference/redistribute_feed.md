# Redistribute available supply among Livestock_cat based on their demand

\`redistribute_feed()\` matches livestock feed demand to available feed
items through a hierarchical allocation that follows the remaining-share
principle to avoid exceeding availability.The redistribution path adapts
to the \`fixed_demand\` column in the demand table. \`fixed_demand =
TRUE\` uses a six-level hierarchy that guarantees all demand of each
Livestock_cat-Province_name-Year group is met (by resorting to
provincial grassland), while \`fixed_demand = FALSE\` uses a five-level
hierarchy that honours observed availability and can exceed the original
demand when supply is abundant.

The modes operate in each year following hierarchy levels in order:

\* \*\*Fixed demand (\`fixed_demand = TRUE\`)\*\* – six levels: 1.
Item-level matches (provincial then national) 2. Substitution within
\`Cat_1\` (prov. then national) 3. Substitution within \`Cat_feed\`
(prov. then national) 4. Inter-provincial trade of Provincial items,
excluding those in \`Cat_feed == "Grass"\` 5. Substitution within all
remaining non-grassland availability (provincial then national) 6.
Fulfil any residual demand with unlimited provincial grassland

\* \*\*Availability-driven (\`fixed_demand = FALSE\`)\*\* – five
levels: 1. Item-level matches (provincial then national) 2. Substitution
within \`Cat_1\` (prov. then national) 3. Substitution within
\`Cat_feed\` (prov. then national) 4. Substitution within all remaining
non-grassland availability (provincial then national) 5. Distribute any
surplus availability of each \`item_cbs\` across all
Year-Province-Livestock demand combinations proportionally to their
demand, respecting the provincial or national nature of the item. Intake
can exceed the original demand at this stage.

The function relies on the environment objects \`items_full\`, \`Cats\`,
and optionally \`Monogastric\` (character vector of monogastric
livestock categories). Call \`load_general_data()\` before using this
function.

## Usage

``` r
redistribute_feed(
  Feed_demand,
  Feed_avail,
  zoot_fixed_max_multiplier = 3,
  territory_col = "Territory",
  sub_territory_col = "Sub_territory",
  verbose = TRUE
)
```

## Arguments

- Feed_demand:

  A data frame with columns \`Year\`, \`Livestock_cat\`, \`item_cbs\`,
  \`Cat_1\`, \`Cat_feed\`, \`demand_MgDM\`, and \`fixed_demand\`
  (logical), plus the user-selected territory columns indicated by
  \`territory_col\` and \`sub_territory_col\`. The \`fixed_demand\`
  column controls the redistribution mode row-wise for each demand
  record, allowing mixed fixed and availability-driven livestock
  categories within the same \`Year x Territory\` combination.

- Feed_avail:

  A data frame with columns \`Year\`, \`item_cbs\`, \`Cat_1\`,
  \`Cat_feed\`, \`Avail_MgDM\`, plus the user-selected territory columns
  indicated by \`territory_col\` and \`sub_territory_col\`.
  \`Feed_scale\` is inferred in the function from \`sub_territory_col\`
  (\`"Provincial"\` when present, \`"National"\` otherwise). The table
  must be pre-aggregated at the item level.

- zoot_fixed_max_multiplier:

  Numeric multiplier (default 3) controlling the maximum ratio of intake
  to availability for Zoot_fixed items. When availability of Zoot_fixed
  is zero or NA, no cap is applied and intake equals demand.

- territory_col:

  Character scalar indicating the broad territory column name shared by
  \`Feed_demand\` and \`Feed_avail\`. Redistribution boundaries are
  defined by \`Year x territory_col\`.

- sub_territory_col:

  Character scalar indicating the local territory column name shared by
  \`Feed_demand\` and \`Feed_avail\` (e.g., provinces).

- verbose:

  Logical (default \`TRUE\`). When \`TRUE\`, prints stage-by-stage
  diagnostic messages. Set to \`FALSE\` for silent operation.

## Value

A tibble with the columns \`Year\`, the column named by
\`territory_col\`, the column named by \`sub_territory_col\`,
\`Livestock_cat\`, \`item_cbs\`, \`Cat_1\`, \`Cat_feed\`,
\`demand_MgDM\`, \`intake_MgDM\`, \`scaling_factor\`,
\`hierarchy_level\`, \`original_demand_item\`, \`original_Province\`,
and \`fixed_demand\`.

\`item_cbs\` shows the actual item consumed (from availability), while
\`original_demand_item\` shows what was originally requested. When
substitution occurs, these differ. \`original_Province\` shows the
province the feed came from (NA for national items). \`Cat_1\` is
appended at the end using the lookup table \`items_full\`, and
\`Cat_feed\` using the lookup table \`Cats\`.

\`hierarchy_level\` values depend on the redistribution mode:

\* Fixed demand (\`TRUE\`): 1. \`"1_item_exact"\` 2. \`"2_Cat_1_sub"\`
3. \`"3_Cat_feed_sub"\` 4. \`"4_inter_prov_trade"\` 5.
\`"5_all_substitute"\` 6. \`"6_grassland_unlimited"\`

\* Availability-driven (\`FALSE\`): 1. \`"1_item_exact"\` 2.
\`"2_Cat_1_sub"\` 3. \`"3_Cat_feed_sub"\` 4. \`"4_all_substitute"\` 5.
\`"5_surplus_distribution"\`

If both modes are present for any demand records in the same run, the
function executes the mixed workflow: fixed rows can reach
\`"6_grassland_unlimited"\`, while variable groups receive surplus
allocation.

## Details

The allocation follows these rules:

1\. Items in \`Cat_feed == "Zoot_fixed"\` are kept unchanged (intake
equals demand, even if availability is lower), but capped at
\`zoot_fixed_max_multiplier \* availability\` to prevent extreme
violations. When availability is zero or NA, no cap is applied. If a cap
is applied, the excess demand should be met through other available
items following the hierarchy. 2. Monogastric categories are allocated
first. \`Monogastric\` is a vector present in the environment defining
which Livestock_cat are monogastric. 3. Items without a defined
\`feedtype_graniv\` are ignored for monogastrics. \`feedtype_graniv\` is
joined from the \`items_full\` lookup table. 4. Ruminant Livestock_cat
categories are processed with the remaining availability. 5. The
availability of each item_cbs is redistributed based on demand,
respecting the hierarchy levels and the provincial/national nature of
each item. 6. During levels 1-3, it must be ensured that, if the
availability of a given feed category is higher than the demand, all the
demand should be satisfied before moving to the next level. 7. During
overall redistribution at level 5 (\`fixed_demand=TRUE\`) or 4
(\`fixed_demand=FALSE\`), available items are prioritized according to
\`catfeed_priority\` (Lactation, High_quality, Low_quality, Residues,
Grass). This should result in all available high-priority feedstuff
being allocated before lower-priority items are considered.

\`original_demand_item\` always records the item requested in the input
demand, even when intake is met by a different item after substitution.
The resulting \`scaling_factor\` is computed per demand row as
\`sum(intake_MgDM) / demand_MgDM\`.

## Examples

``` r
if (FALSE) { # \dontrun{
library(afsetools)
load_general_data()

Feed_intake <- redistribute_feed(
  Feed_demand = Feed_demand_ygiac |>
    dplyr::mutate(fixed_demand = TRUE),
  Feed_avail = Feed_avail_ygi
)
} # }
```
