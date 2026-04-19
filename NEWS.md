# afsetools (unreleased)

## `redistribute_feed()` — behavior changes (round 2)

* **`max_intake_share` excess is now redirected via an availability-aware
  chain that preserves supply conservation.** When an item violates a
  Livestock_cat's `max_intake_share`, the reduced DM is carried over to
  substitute items in this order:
  1. Grassland (unlimited fallback) if not capped for the Livestock_cat.
  2. Otherwise, non-Grassland items from `items_full` in the main-pipeline
     priority order (Lactation / High_quality → Low_quality → Residues →
     Grass), skipping items capped for this Livestock_cat. Each substitute
     is bounded by `avail$avail_remaining` and **decrements it** (prov
     avail preferred, nat used if prov insufficient), keeping supply
     conservation intact.
  3. If all non-Grassland substitutes are exhausted (capped or
     avail-depleted) and excess remains, the remainder is pushed to
     Grassland regardless of its cap, with a warning — per user request
     ("chaining to grass if not enough other substitutes"). Previously the
     redirect emitted synthetic intake rows with `avail_id = NA`, silently
     violating supply conservation for any non-Grassland substitute.

## `redistribute_feed()` — additional bug fixes

* **Final `arrange()` now deterministic** on all tiebreakers
  (`item_cbs`, `original_Province` appended) so two runs producing the
  same allocation can't differ in row order.
* `compute_scaling()` no longer collapses intended `NA_real_` results
  (demand = 0, intake > 0) to `0`; aligns with the prior `case_when`
  semantics.
* Removed dead `can_receive_priority` column computed twice and never
  referenced.
* Removed dead first call of `compute_scaling()` that was always
  overwritten by the final pass.
* `allocate_grassland()` no longer manually zeroes `demand$remaining` —
  `add_alloc()` already decrements by the actually-allocated amount, and
  the manual zero could drift from the true intake if the cap ever bound.

## `redistribute_feed()` — round-2 speed

* Replaced `stats::aggregate()` in the priority-pool inner loop with
  `rowsum()` (same pattern as `add_alloc()`; lower per-call overhead).
* Pre-bucketed demand row indices by `(Year, Territory)` at preprocessing
  so the priority-pool and surplus loops subset in O(1) instead of
  filtering the full demand table once per bucket.
* Converted `allocate_cartesian()` and `allocate_grouped()` (the two
  primary-level join-heavy helpers, called 12+ times per run) to
  **data.table**. `data.table` moved from `Suggests` to `Imports`.
* Net result on the 25 k-demand reference fixture: baseline 2.62 s →
  current 1.38 s (**1.9× speedup**) *with* the new chained-redirect
  behavior absorbing the complexity.

## `redistribute_feed()` — behavior changes (round 1)

* **Zoot_fixed cap is now per-Livestock_cat.** Each demand row's intake is
  bounded by `zoot_fixed_max_multiplier * Avail_MgDM` independently of
  other Livestock_cats competing for the same `(Year, Territory, Province,
  item_cbs)` pool. Previously the cap applied to the group sum, so an
  individual Livestock_cat could be scaled down because of another's
  demand.
* **New parameter `prioritize_monogastric` (default `TRUE`).** When `FALSE`,
  monogastric and ruminant demand are allocated in a single combined pass
  through the primary hierarchy, so monogastrics no longer get first pick
  on scarce high-priority feed.

## `redistribute_feed()` — coherence fixes and speed

* Determinism: stabilised the per-group cap inside `add_alloc()` so that two
  runs with identical inputs produce bit-identical outputs. Previously the
  `cumsum()` cap depended on arbitrary row order from upstream joins.
* Fixed a local-cache bug in the priority-pool inner loop where multiple
  allocations to the same `demand_id` within one `(Year, Territory)` batch
  were silently dropped by index overwrite, allowing minor over-allocation.
* Trade allocations (release pass, `allow_trade = TRUE`) now correctly
  record the sourcing province in `original_Province`. They previously
  carried the demand's own province.
* Removed dead code (no-op weight refresh in `distribute_surplus()`; a
  duplicate `ensure_item_metadata()` call inside `allocate_grouped()`).
* Added structural-invariant tests: determinism, supply conservation per
  source province, scaling-factor consistency, Zoot_fixed cap, and
  inter-provincial trade province tracking.
* Speed: replaced join-based updates in `add_alloc()` with indexed
  writes (composite-key map), vectorised `reroute_excess_grass()`,
  vectorised final `scaling_factor` computation, short-circuited
  `diag_snapshot()` when `verbose = FALSE`, and replaced the
  monogastric/ruminant `%in% target_ids` filter with a pre-computed
  logical mask. Together these deliver ~2× speedup on the reference
  fixture (inst/bench/bench_redistribute_feed.R: 2.62s → 1.26s).

# afsetools 0.3.0

## New features

* **BNF functions module** (`R/bnf_functions.R`): 5 exported functions for biological
  nitrogen fixation — `calc_crop_bnf()`, `calc_weed_bnf()`, `calc_nonsymbiotic_bnf()`,
  `calc_bnf()`, and `summarize_bnf()` — with literature-based environmental modifiers
  for temperature, water stress, soil organic matter, pH, and clay content.

* **IPCC biomass coefficients**: 6 new sheets in `Biomass_coefs.xlsx` with crop residue,
  root, and mapping coefficients verified against IPCC 2006 Vol.4 Ch.11 Table 11.2.

* **Data-driven NPP model**: NPP model coefficients extracted to Excel
  (`NPP_model_coefs` sheet). `calculate_potential_npp()` now reads coefficients from
  the data table instead of hardcoded literals.

* **Simple mode** for crop NPP functions: `calculate_crop_npp()`,
  `calculate_crop_residues()`, `calculate_crop_roots()`, and
  `calculate_npp_dm_c_n()` gain a `mode` parameter (`"full"` / `"simple"`) that
  auto-detects available columns and adjusts calculations accordingly.

* **Crop-group-specific residue/root helper data**: new lookup tables for
  residue shares, weed NPP scaling, fallow cover, and root-shoot N response.

* **Feed distribution**: `redistribute_feed()` function for multi-level feed
  allocation with diagnostics, plus `max_intake_share` loaded from
  `Livestock_coefs.xlsx`.

## Bug fixes

* Fixed crop-specific irrigation residue factor and reduced rice `HI_gap_factor`.
* Fixed N-input root-shoot adjustment: `calculate_crop_roots()` now uses the
  `N_input_RS_adj` data table via `findInterval()` instead of ignoring it.
* Added `DescTools` and `zoo` to package dependencies (previously undeclared).
* Fixed `stats::quantile()` call in `calculate_footprints()` draught share
  calculation (was missing namespace prefix).
* Replaced non-ASCII box-drawing characters in `redistribute_feed()` diagnostics
  with ASCII equivalents for portable package compliance.
* Fixed `%!in%` operator documentation to avoid invalid Rd `\name` field.
* Moved unused `data.table` and `readxl` from Imports to Suggests.
* Added comprehensive `globalVariables()` declarations for all NSE column names
  across the package (eliminates R CMD check NOTEs).

## Documentation

* Expanded `DATA_REFERENCE.md` to 80+ objects with IPCC and NPP sections.
* Complete roxygen2 rewrite for `calculate_potential_npp()` and NPP model
  coefficient documentation.
* Added `Names_BNF`, `Pure_legs`, `NPP_model_coefs`, and `max_intake_share`
  to pkgdown reference index.

# afsetools 0.2.0

* Removed `Filling`, `FillingProxy`, `fill_na_with_sum` (migrated to `whep`).
* Cross-repo dependency fixes and code audit.
* Added `redistribute_feed()` function.
* Aligned `calculate_footprints()` with General reference data.
