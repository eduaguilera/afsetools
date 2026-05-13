# Tests for feed distribution functions

# ---------------------------------------------------------------------------
# Setup: create minimal mock environment objects that redistribute_feed needs
# ---------------------------------------------------------------------------
items_full <- tibble::tribble(
  ~item_cbs,        ~Cat_1,       ~feedtype_graniv,
  "Barley",         "Barley",     "cereal",
  "Maize (corn)",   "Maize",      "cereal",
  "Wheat",          "Wheat",      "cereal",
  "Grassland",      "Grassland",  NA_character_,
  "Milk_feed",      "Milk_feed",  NA_character_,
  "Acorns",         "Acorns",     NA_character_
)
assign("items_full", items_full, envir = .GlobalEnv)

Cats <- tibble::tribble(
  ~Cat_1,        ~Cat_feed,
  "Barley",      "High_quality",
  "Maize",       "High_quality",
  "Wheat",       "High_quality",
  "Grassland",   "Grass",
  "Milk_feed",   "Zoot_fixed",
  "Acorns",      "Grass"
)
assign("Cats", Cats, envir = .GlobalEnv)

Monogastric <- c("Pigs", "Poultry", "Other_birds",
                 "Fur animals", "Other", "Pets", "Aquaculture")
assign("Monogastric", Monogastric, envir = .GlobalEnv)

# Clean up after tests
withr::defer({
  rm("items_full", "Cats", "Monogastric",
     envir = .GlobalEnv)
}, envir = parent.frame())

# ---------------------------------------------------------------------------
# Helper: build minimal demand / avail tables for redistribute_feed()
# ---------------------------------------------------------------------------
make_demand <- function(fixed = TRUE) {
  tibble::tribble(
    ~Year, ~Territory, ~Sub_territory, ~Livestock_cat,
    ~item_cbs, ~Cat_1, ~Cat_feed, ~demand_MgDM,
    ~fixed_demand,
    2020, "T1", "P1", "Cattle_milk",
    "Barley", "Barley", "High_quality", 100,
    fixed,
    2020, "T1", "P1", "Pigs",
    "Maize (corn)", "Maize", "High_quality", 50,
    fixed,
    2020, "T1", "P1", "Cattle_milk",
    "Grassland", "Grassland", "Grass", 200,
    fixed
  )
}

make_avail <- function() {
  tibble::tribble(
    ~Year, ~Territory, ~Sub_territory, ~item_cbs,
    ~Cat_1, ~Cat_feed, ~Avail_MgDM,
    2020, "T1", "P1", "Barley",
    "Barley", "High_quality", 80,
    2020, "T1", "P1", "Maize (corn)",
    "Maize", "High_quality", 60,
    2020, "T1", "P1", "Grassland",
    "Grassland", "Grass", 300,
    2020, "T1", NA, "Wheat",
    "Wheat", "High_quality", 40
  )
}

# ---------------------------------------------------------------------------
# Test: basic fixed-demand run returns expected structure
# ---------------------------------------------------------------------------
test_that("redistribute_feed returns correct columns (fixed)", {
  result <- redistribute_feed(
    Feed_demand = make_demand(fixed = TRUE),
    Feed_avail = make_avail(),
    verbose = FALSE
  )

  expected_cols <- c(
    "Year", "Territory", "Sub_territory", "Livestock_cat",
    "item_cbs", "Cat_1", "Cat_feed", "demand_MgDM",
    "intake_MgDM", "scaling_factor", "hierarchy_level",
    "original_demand_item", "fixed_demand", "original_Province"
  )
  expect_true(all(expected_cols %in% names(result)))
  expect_s3_class(result, "tbl_df")
})

# ---------------------------------------------------------------------------
# Test: basic variable-demand run returns expected columns
# ---------------------------------------------------------------------------
test_that("redistribute_feed returns correct columns (variable)", {
  result <- redistribute_feed(
    Feed_demand = make_demand(fixed = FALSE),
    Feed_avail = make_avail(),
    verbose = FALSE
  )

  expect_true("hierarchy_level" %in% names(result))
  expect_true(all(result$fixed_demand == FALSE))
})

# ---------------------------------------------------------------------------
# Test: total intake never exceeds availability (fixed-demand, levels 1-5)
# ---------------------------------------------------------------------------
test_that("intake does not exceed availability (fixed, pre-grassland)", {
  result <- redistribute_feed(
    Feed_demand = make_demand(fixed = TRUE),
    Feed_avail = make_avail(),
    verbose = FALSE
  )

  non_grass <- result |>
    dplyr::filter(hierarchy_level != "6_grassland_unlimited",
                  item_cbs != "Grassland")

  avail_totals <- make_avail() |>
    dplyr::summarize(
      .by = c(Year, item_cbs),
      total_avail = sum(Avail_MgDM, na.rm = TRUE)
    )

  intake_totals <- non_grass |>
    dplyr::summarize(
      .by = c(Year, item_cbs),
      total_intake = sum(intake_MgDM, na.rm = TRUE)
    )

  check <- dplyr::left_join(
    intake_totals, avail_totals,
    by = c("Year", "item_cbs")
  ) |>
    dplyr::mutate(total_avail = dplyr::coalesce(total_avail, 0))

  expect_true(
    all(check$total_intake <= check$total_avail + 1e-6),
    info = "Intake should not exceed availability before grassland level"
  )
})

# ---------------------------------------------------------------------------
# Test: fixed-demand guarantees all demand is met (scaling >= 1)
# ---------------------------------------------------------------------------
test_that("fixed_demand mode satisfies all demand", {
  result <- redistribute_feed(
    Feed_demand = make_demand(fixed = TRUE),
    Feed_avail = make_avail(),
    verbose = FALSE
  )

  sf <- result |>
    dplyr::summarize(
      .by = c(Year, Sub_territory, Livestock_cat, original_demand_item),
      total_intake = sum(intake_MgDM, na.rm = TRUE),
      demand = max(demand_MgDM)
    ) |>
    dplyr::mutate(ratio = total_intake / demand)

  expect_true(
    all(sf$ratio >= 1 - 1e-6),
    info = "All fixed-demand rows must be fully satisfied"
  )
})

# ---------------------------------------------------------------------------
# Test: verbose = FALSE produces no console output
# ---------------------------------------------------------------------------
test_that("verbose = FALSE suppresses output", {
  out <- capture.output(
    type = "output",
    {
      res <- suppressWarnings(
        redistribute_feed(
          Feed_demand = make_demand(fixed = TRUE),
          Feed_avail = make_avail(),
          verbose = FALSE
        )
      )
    }
  )

  expect_equal(length(out), 0,
               info = "No console output when verbose = FALSE")
})

# ---------------------------------------------------------------------------
# Test: validation errors
# ---------------------------------------------------------------------------
test_that("redistribute_feed validates inputs", {
  d <- make_demand()
  a <- make_avail()


  # Missing required column
  expect_error(
    redistribute_feed(
      Feed_demand = dplyr::select(d, -demand_MgDM),
      Feed_avail = a,
      verbose = FALSE
    ),
    "missing columns"
  )

  # Non-logical fixed_demand
  bad_demand <- d |> dplyr::mutate(fixed_demand = "yes")
  expect_error(
    redistribute_feed(
      Feed_demand = bad_demand,
      Feed_avail = a,
      verbose = FALSE
    ),
    "logical"
  )

  # NA in fixed_demand
  na_demand <- d |>
    dplyr::mutate(fixed_demand = dplyr::if_else(
      dplyr::row_number() == 1, NA, fixed_demand
    ))
  expect_error(
    redistribute_feed(
      Feed_demand = na_demand,
      Feed_avail = a,
      verbose = FALSE
    ),
    "NA"
  )
})

# ---------------------------------------------------------------------------
# Test: Zoot_fixed items are allocated even without availability
# ---------------------------------------------------------------------------
test_that("Zoot_fixed items have intake even without availability", {
  zoot_demand <- tibble::tribble(
    ~Year, ~Territory, ~Sub_territory, ~Livestock_cat,
    ~item_cbs, ~Cat_1, ~Cat_feed, ~demand_MgDM,
    ~fixed_demand,
    2020, "T1", "P1", "Cattle_milk",
    "Milk_feed", "Milk_feed", "Zoot_fixed", 10,
    TRUE
  )

  result <- redistribute_feed(
    Feed_demand = zoot_demand,
    Feed_avail = make_avail(),
    verbose = FALSE
  )

  zoot_rows <- result |>
    dplyr::filter(Cat_feed == "Zoot_fixed")
  expect_true(nrow(zoot_rows) > 0)
  expect_true(sum(zoot_rows$intake_MgDM) > 0,
              info = "Zoot_fixed intake should be positive")
})

# ---------------------------------------------------------------------------
# Test: mixed mode (both fixed and variable rows)
# ---------------------------------------------------------------------------
test_that("mixed mode handles both fixed and variable rows", {
  demand_mixed <- dplyr::bind_rows(
    make_demand(fixed = TRUE) |>
      dplyr::filter(Livestock_cat == "Cattle_milk"),
    make_demand(fixed = FALSE) |>
      dplyr::filter(Livestock_cat == "Pigs")
  )

  result <- redistribute_feed(
    Feed_demand = demand_mixed,
    Feed_avail = make_avail(),
    verbose = FALSE
  )

  expect_true(any(result$fixed_demand == TRUE))
  expect_true(any(result$fixed_demand == FALSE))
})

# ---------------------------------------------------------------------------
# Test: territory / sub_territory column renaming
# ---------------------------------------------------------------------------
test_that("custom territory column names are preserved", {
  d <- make_demand() |>
    dplyr::rename(Country = Territory, Province = Sub_territory)
  a <- make_avail() |>
    dplyr::rename(Country = Territory, Province = Sub_territory)

  result <- redistribute_feed(
    Feed_demand = d,
    Feed_avail = a,
    territory_col = "Country",
    sub_territory_col = "Province",
    verbose = FALSE
  )

  expect_true("Country" %in% names(result))
  expect_true("Province" %in% names(result))
})

# ---------------------------------------------------------------------------
# Test: empty demand returns zero-row tibble with correct structure
# ---------------------------------------------------------------------------
test_that("empty demand returns empty tibble", {
  empty_d <- make_demand() |> dplyr::filter(FALSE)

  result <- redistribute_feed(
    Feed_demand = empty_d,
    Feed_avail = make_avail(),
    verbose = FALSE
  )

  expect_equal(nrow(result), 0)
  expect_s3_class(result, "tbl_df")
})

# ===========================================================================
# Structural-invariant tests
# ===========================================================================
# The fixtures below exercise non-trivial (multi-province, multi-item) cases
# so the invariants are meaningful. Tolerances use 1e-6 on DM magnitudes.

make_demand_multi <- function() {
  tibble::tribble(
    ~Year, ~Territory, ~Sub_territory, ~Livestock_cat,
    ~item_cbs, ~Cat_1, ~Cat_feed, ~demand_MgDM, ~fixed_demand,
    2020, "T1", "P1", "Cattle_milk", "Barley",       "Barley",    "High_quality", 100, TRUE,
    2020, "T1", "P1", "Cattle_milk", "Grassland",    "Grassland", "Grass",        200, TRUE,
    2020, "T1", "P1", "Pigs",        "Maize (corn)", "Maize",     "High_quality",  50, TRUE,
    2020, "T1", "P2", "Cattle_milk", "Wheat",        "Wheat",     "High_quality",  80, TRUE,
    2020, "T1", "P2", "Cattle_milk", "Grassland",    "Grassland", "Grass",        150, TRUE,
    2020, "T1", "P2", "Pigs",        "Barley",       "Barley",    "High_quality",  40, TRUE,
    2021, "T1", "P1", "Cattle_milk", "Barley",       "Barley",    "High_quality", 110, TRUE,
    2021, "T1", "P2", "Pigs",        "Maize (corn)", "Maize",     "High_quality",  55, TRUE
  )
}

make_avail_multi <- function() {
  tibble::tribble(
    ~Year, ~Territory, ~Sub_territory, ~item_cbs,
    ~Cat_1, ~Cat_feed, ~Avail_MgDM,
    2020, "T1", "P1", "Barley",       "Barley",    "High_quality", 80,
    2020, "T1", "P1", "Maize (corn)", "Maize",     "High_quality", 60,
    2020, "T1", "P1", "Grassland",    "Grassland", "Grass",       180,
    2020, "T1", "P2", "Barley",       "Barley",    "High_quality", 50,
    2020, "T1", "P2", "Wheat",        "Wheat",     "High_quality", 70,
    2020, "T1", "P2", "Grassland",    "Grassland", "Grass",       140,
    2020, "T1", NA,   "Wheat",        "Wheat",     "High_quality", 40,
    2021, "T1", "P1", "Barley",       "Barley",    "High_quality", 95,
    2021, "T1", "P2", "Maize (corn)", "Maize",     "High_quality", 45
  )
}

# ---------------------------------------------------------------------------
# Invariant 1: determinism — same inputs produce bit-identical outputs
# ---------------------------------------------------------------------------
test_that("redistribute_feed is deterministic (fixed)", {
  r1 <- redistribute_feed(make_demand_multi(), make_avail_multi(), verbose = FALSE)
  r2 <- redistribute_feed(make_demand_multi(), make_avail_multi(), verbose = FALSE)
  expect_true(isTRUE(all.equal(r1, r2)),
              info = "Two runs with identical inputs must match")
})

test_that("redistribute_feed is deterministic (variable)", {
  d <- make_demand_multi() |> dplyr::mutate(fixed_demand = FALSE)
  r1 <- redistribute_feed(d, make_avail_multi(), verbose = FALSE)
  r2 <- redistribute_feed(d, make_avail_multi(), verbose = FALSE)
  expect_true(isTRUE(all.equal(r1, r2)),
              info = "Two runs with identical inputs must match")
})

# ---------------------------------------------------------------------------
# Invariant 2: supply conservation per (Year, Territory, item_cbs, source_Province)
# ---------------------------------------------------------------------------
test_that("non-grassland intake respects availability per source province", {
  result <- redistribute_feed(make_demand_multi(), make_avail_multi(), verbose = FALSE)

  # Sum intake by (Year, Territory, source_Province, item_cbs). source_Province
  # = original_Province (NA for national items).
  intake_by_source <- result |>
    dplyr::filter(hierarchy_level != "6_grassland_unlimited",
                  item_cbs != "Grassland") |>
    dplyr::summarize(
      .by = c(Year, Territory, original_Province, item_cbs),
      total_intake = sum(intake_MgDM, na.rm = TRUE)
    )

  avail_by_source <- make_avail_multi() |>
    dplyr::mutate(Province_key = Sub_territory) |>
    dplyr::summarize(
      .by = c(Year, Territory, Province_key, item_cbs),
      total_avail = sum(Avail_MgDM, na.rm = TRUE)
    ) |>
    dplyr::rename(original_Province = Province_key)

  check <- dplyr::left_join(
    intake_by_source, avail_by_source,
    by = c("Year", "Territory", "original_Province", "item_cbs")
  ) |>
    dplyr::mutate(total_avail = dplyr::coalesce(total_avail, 0))

  expect_true(
    all(check$total_intake <= check$total_avail + 1e-6),
    info = "Non-grassland intake must not exceed availability at the source province"
  )
})

# ---------------------------------------------------------------------------
# Invariant 3: scaling_factor == sum(intake)/demand per demand_id group
# ---------------------------------------------------------------------------
test_that("scaling_factor is consistent with intake sums", {
  result <- redistribute_feed(make_demand_multi(), make_avail_multi(), verbose = FALSE)

  # For each (Year, Territory, Sub_territory, Livestock_cat, original_demand_item)
  # the scaling_factor rows should all be equal and match sum(intake)/demand.
  check <- result |>
    dplyr::summarize(
      .by = c(Year, Territory, Sub_territory, Livestock_cat, original_demand_item),
      total_intake = sum(intake_MgDM, na.rm = TRUE),
      demand = dplyr::first(demand_MgDM),
      sf = dplyr::first(scaling_factor),
      sf_min = min(scaling_factor),
      sf_max = max(scaling_factor)
    ) |>
    dplyr::mutate(
      expected_sf = dplyr::case_when(
        demand > 0 ~ total_intake / demand,
        total_intake > 0 ~ NA_real_,
        TRUE ~ 0
      )
    )

  expect_true(all(abs(check$sf_max - check$sf_min) < 1e-9),
              info = "scaling_factor must be constant within a demand group")
  expect_true(all(
    is.na(check$expected_sf) |
      abs(check$sf - check$expected_sf) < 1e-6
  ),
    info = "scaling_factor must equal sum(intake)/demand"
  )
})

# ---------------------------------------------------------------------------
# Invariant 4: non-negative intake, finite scaling_factor
# ---------------------------------------------------------------------------
test_that("intake is non-negative and scaling_factor finite", {
  result <- redistribute_feed(make_demand_multi(), make_avail_multi(), verbose = FALSE)
  expect_true(all(result$intake_MgDM >= 0))
  expect_true(all(is.na(result$scaling_factor) | is.finite(result$scaling_factor)))
})

# ---------------------------------------------------------------------------
# Invariant 5: Zoot_fixed cap respected when availability > 0
# ---------------------------------------------------------------------------
test_that("Zoot_fixed intake respects zoot_fixed_max_multiplier when avail > 0", {
  zoot_demand <- tibble::tribble(
    ~Year, ~Territory, ~Sub_territory, ~Livestock_cat,
    ~item_cbs, ~Cat_1, ~Cat_feed, ~demand_MgDM, ~fixed_demand,
    2020, "T1", "P1", "Cattle_milk", "Milk_feed", "Milk_feed", "Zoot_fixed", 1000, TRUE
  )
  zoot_avail <- tibble::tribble(
    ~Year, ~Territory, ~Sub_territory, ~item_cbs,
    ~Cat_1, ~Cat_feed, ~Avail_MgDM,
    2020, "T1", "P1", "Milk_feed", "Milk_feed", "Zoot_fixed", 100,
    2020, "T1", "P1", "Grassland", "Grassland", "Grass",      500
  )

  result <- redistribute_feed(zoot_demand, zoot_avail,
                              zoot_fixed_max_multiplier = 2,
                              verbose = FALSE)

  zoot_intake <- result |>
    dplyr::filter(Cat_feed == "Zoot_fixed") |>
    dplyr::summarize(total = sum(intake_MgDM, na.rm = TRUE)) |>
    dplyr::pull(total)

  # Cap is 2 * 100 = 200 when avail > 0
  expect_true(zoot_intake <= 200 + 1e-6,
              info = "Zoot_fixed intake must be <= multiplier * avail when avail > 0")
})

# ---------------------------------------------------------------------------
# Invariant 6: trade allocations carry the source province
# ---------------------------------------------------------------------------
test_that("trade allocations record the source province in original_Province", {
  # Province P1 has excess Barley, P2 has unmet Barley demand.
  trade_demand <- tibble::tribble(
    ~Year, ~Territory, ~Sub_territory, ~Livestock_cat,
    ~item_cbs, ~Cat_1, ~Cat_feed, ~demand_MgDM, ~fixed_demand,
    2020, "T1", "P2", "Pigs", "Barley", "Barley", "High_quality", 100, TRUE
  )
  trade_avail <- tibble::tribble(
    ~Year, ~Territory, ~Sub_territory, ~item_cbs,
    ~Cat_1, ~Cat_feed, ~Avail_MgDM,
    2020, "T1", "P1", "Barley", "Barley", "High_quality", 200
  )

  result <- redistribute_feed(trade_demand, trade_avail, verbose = FALSE)

  trade_rows <- result |>
    dplyr::filter(item_cbs == "Barley", intake_MgDM > 1e-9)

  # Any Barley actually consumed at P2 must be sourced from P1.
  expect_true(nrow(trade_rows) > 0)
  expect_true(all(trade_rows$original_Province == "P1" |
                    is.na(trade_rows$original_Province) &
                      trade_rows$item_cbs != "Barley"),
              info = "Feed traded across provinces must record source province")
})

# ---------------------------------------------------------------------------
# Invariant 7: Zoot_fixed cap is per Livestock_cat (independent)
# ---------------------------------------------------------------------------
test_that("Zoot_fixed cap is per Livestock_cat (independent)", {
  zoot_demand <- tibble::tribble(
    ~Year, ~Territory, ~Sub_territory, ~Livestock_cat,
    ~item_cbs, ~Cat_1, ~Cat_feed, ~demand_MgDM, ~fixed_demand,
    2020, "T1", "P1", "Cattle_milk", "Milk_feed", "Milk_feed", "Zoot_fixed",  80, TRUE,
    2020, "T1", "P1", "Cattle_meat", "Milk_feed", "Milk_feed", "Zoot_fixed",  80, TRUE
  )
  zoot_avail <- tibble::tribble(
    ~Year, ~Territory, ~Sub_territory, ~item_cbs,
    ~Cat_1, ~Cat_feed, ~Avail_MgDM,
    2020, "T1", "P1", "Milk_feed", "Milk_feed", "Zoot_fixed", 100,
    2020, "T1", "P1", "Grassland", "Grassland", "Grass",      500
  )
  res <- redistribute_feed(zoot_demand, zoot_avail,
                           zoot_fixed_max_multiplier = 2,
                           verbose = FALSE)
  per_lc <- res |>
    dplyr::filter(Cat_feed == "Zoot_fixed") |>
    dplyr::summarize(.by = Livestock_cat, intake = sum(intake_MgDM))
  # Each LC demand = 80, cap = 2 * 100 = 200 → no binding.
  expect_true(all(abs(per_lc$intake - 80) < 1e-6),
              info = "Per-LC zoot cap leaves LCs below cap untouched")

  # Push one LC above cap. Cattle_milk demand = 500, cap = 200.
  zoot_demand2 <- zoot_demand
  zoot_demand2$demand_MgDM[zoot_demand2$Livestock_cat == "Cattle_milk"] <- 500
  res2 <- redistribute_feed(zoot_demand2, zoot_avail,
                            zoot_fixed_max_multiplier = 2,
                            verbose = FALSE)
  per_lc2 <- res2 |>
    dplyr::filter(Cat_feed == "Zoot_fixed") |>
    dplyr::summarize(.by = Livestock_cat, intake = sum(intake_MgDM))
  cm <- per_lc2$intake[per_lc2$Livestock_cat == "Cattle_milk"]
  ct <- per_lc2$intake[per_lc2$Livestock_cat == "Cattle_meat"]
  expect_true(abs(cm - 200) < 1e-6,
              info = "Cattle_milk capped at multiplier * avail = 200")
  expect_true(abs(ct - 80)  < 1e-6,
              info = "Cattle_meat unaffected by Cattle_milk's cap")
})

# ---------------------------------------------------------------------------
# Invariant 8: max_intake_share excess is redirected to Grassland
# ---------------------------------------------------------------------------
test_that("max_intake_share excess is redirected to Grassland", {
  # Build a max_intake_share table that forces Barley into a 30% cap for
  # Cattle_milk, and inject it into the global env the same way the function
  # expects it.
  withr::local_options(list(warn = -1))  # silence redirect warning on other LCs
  mis <- tibble::tibble(
    Livestock_cat = "Cattle_milk",
    item_cbs = "Barley",
    max_intake_share = 0.3
  )
  assign("max_intake_share", mis, envir = .GlobalEnv)
  on.exit(rm("max_intake_share", envir = .GlobalEnv), add = TRUE)

  d <- tibble::tribble(
    ~Year, ~Territory, ~Sub_territory, ~Livestock_cat,
    ~item_cbs, ~Cat_1, ~Cat_feed, ~demand_MgDM, ~fixed_demand,
    2020, "T1", "P1", "Cattle_milk", "Barley", "Barley", "High_quality", 100, TRUE
  )
  a <- tibble::tribble(
    ~Year, ~Territory, ~Sub_territory, ~item_cbs,
    ~Cat_1, ~Cat_feed, ~Avail_MgDM,
    2020, "T1", "P1", "Barley",    "Barley",    "High_quality", 200,
    2020, "T1", "P1", "Grassland", "Grassland", "Grass",        500
  )
  res <- redistribute_feed(d, a, verbose = FALSE)

  # Barley intake for Cattle_milk should be capped at 30% of total intake.
  # Total demand = 100. At cap, if barley is sole feed, 30% cap forces
  # barley = 0.3*total, so total = barley/0.3. The redirect pushes the
  # remaining 70% to Grassland, giving total = barley + grass.
  totals <- res |>
    dplyr::summarize(
      .by = Livestock_cat,
      barley   = sum(intake_MgDM[item_cbs == "Barley"],    na.rm = TRUE),
      grass    = sum(intake_MgDM[item_cbs == "Grassland"], na.rm = TRUE),
      total    = sum(intake_MgDM,                          na.rm = TRUE)
    )
  # Barley must be <= 30% of total after redirection.
  expect_true(totals$barley / totals$total <= 0.3 + 1e-6,
              info = "Barley intake must not exceed 30% of total after cap")
  # Grassland must absorb the redirected DM.
  expect_true(totals$grass > 0,
              info = "Excess DM should be redirected to Grassland")
})

# ---------------------------------------------------------------------------
# Invariant 9: prioritize_monogastric = FALSE produces a single-pass result
# ---------------------------------------------------------------------------
test_that("prioritize_monogastric = FALSE runs a single combined pass", {
  # Scarce high-quality feed; pigs (monogastric) and cattle (ruminant)
  # compete. Under TRUE, pigs should get disproportionately more Barley.
  # Under FALSE they should get a roughly proportional share.
  # Override max_intake_share in .GlobalEnv so the package's Pigs/Grassland
  # cap from extdata doesn't muddy the comparison.
  assign("max_intake_share",
         tibble::tibble(Livestock_cat = character(),
                        item_cbs = character(),
                        max_intake_share = numeric()),
         envir = .GlobalEnv)
  on.exit(rm("max_intake_share", envir = .GlobalEnv), add = TRUE)

  d <- tibble::tribble(
    ~Year, ~Territory, ~Sub_territory, ~Livestock_cat,
    ~item_cbs, ~Cat_1, ~Cat_feed, ~demand_MgDM, ~fixed_demand,
    2020, "T1", "P1", "Pigs",        "Barley",    "Barley",    "High_quality", 100, TRUE,
    2020, "T1", "P1", "Cattle_milk", "Barley",    "Barley",    "High_quality", 100, TRUE,
    2020, "T1", "P1", "Pigs",        "Grassland", "Grassland", "Grass",        100, TRUE,
    2020, "T1", "P1", "Cattle_milk", "Grassland", "Grassland", "Grass",        100, TRUE
  )
  a <- tibble::tribble(
    ~Year, ~Territory, ~Sub_territory, ~item_cbs,
    ~Cat_1, ~Cat_feed, ~Avail_MgDM,
    2020, "T1", "P1", "Barley",    "Barley",    "High_quality",  60,
    2020, "T1", "P1", "Grassland", "Grassland", "Grass",        500
  )
  res_pri <- redistribute_feed(d, a,
                               prioritize_monogastric = TRUE,
                               verbose = FALSE)
  res_flat <- redistribute_feed(d, a,
                                prioritize_monogastric = FALSE,
                                verbose = FALSE)

  barley_pigs_pri <- res_pri |>
    dplyr::filter(item_cbs == "Barley", Livestock_cat == "Pigs") |>
    dplyr::summarize(v = sum(intake_MgDM)) |> dplyr::pull(v)
  barley_pigs_flat <- res_flat |>
    dplyr::filter(item_cbs == "Barley", Livestock_cat == "Pigs") |>
    dplyr::summarize(v = sum(intake_MgDM)) |> dplyr::pull(v)

  # With prioritization: pigs eat all 60 Mg of Barley first, cattle get 0.
  expect_true(abs(barley_pigs_pri - 60) < 1e-6,
              info = "Monogastric priority should give pigs all scarce Barley")
  # Without prioritization: pigs share Barley with cattle (demand_share
  # weighted). Both demand 100 → roughly 30 Mg each, strictly less than 60.
  expect_true(barley_pigs_flat < barley_pigs_pri,
              info = "Without priority, pigs share Barley with ruminants")
})

# ---------------------------------------------------------------------------
# Invariant 10: prioritize_monogastric does not break conservation / scaling
# ---------------------------------------------------------------------------
test_that("prioritize_monogastric = FALSE preserves supply conservation", {
  d <- make_demand_multi() |>
    dplyr::mutate(
      Livestock_cat = dplyr::if_else(Livestock_cat == "Pigs",
                                     "Pigs", Livestock_cat)
    )
  res <- redistribute_feed(d, make_avail_multi(),
                           prioritize_monogastric = FALSE,
                           verbose = FALSE)
  # Non-grassland intake per source province must not exceed availability.
  intake <- res |>
    dplyr::filter(hierarchy_level != "6_grassland_unlimited",
                  item_cbs != "Grassland") |>
    dplyr::summarize(.by = c(Year, Territory, original_Province, item_cbs),
                     total_intake = sum(intake_MgDM, na.rm = TRUE))
  avail <- make_avail_multi() |>
    dplyr::mutate(original_Province = Sub_territory) |>
    dplyr::summarize(.by = c(Year, Territory, original_Province, item_cbs),
                     total_avail = sum(Avail_MgDM, na.rm = TRUE))
  check <- dplyr::left_join(intake, avail,
                            by = c("Year", "Territory",
                                   "original_Province", "item_cbs")) |>
    dplyr::mutate(total_avail = dplyr::coalesce(total_avail, 0))
  expect_true(all(check$total_intake <= check$total_avail + 1e-6),
              info = "Combined-pass allocation must still respect availability")
})

# ===========================================================================
# High-priority gap tests (Category A: B1 redirect chain + variable Zoot +
# multi-year) — drafted in isolation per gap; each overrides max_intake_share
# to an empty tibble to avoid interference from the extdata caps.
# ===========================================================================

test_that("max_intake_share chain: Grassland capped, falls back to non-capped item with avail decrement", {
  assign("max_intake_share",
         tibble::tibble(Livestock_cat = c("Cattle_milk", "Cattle_milk"),
                        item_cbs = c("Barley", "Grassland"),
                        max_intake_share = c(0.3, 0.1)),
         envir = .GlobalEnv)
  on.exit(rm("max_intake_share", envir = .GlobalEnv), add = TRUE)

  d <- tibble::tribble(
    ~Year, ~Territory, ~Sub_territory, ~Livestock_cat,
    ~item_cbs, ~Cat_1, ~Cat_feed, ~demand_MgDM, ~fixed_demand,
    2020, "T1", "P1", "Cattle_milk", "Barley", "Barley", "High_quality", 100, TRUE
  )
  a <- tibble::tribble(
    ~Year, ~Territory, ~Sub_territory, ~item_cbs, ~Cat_1, ~Cat_feed, ~Avail_MgDM,
    2020, "T1", "P1", "Barley",    "Barley",    "High_quality", 500,
    2020, "T1", "P1", "Grassland", "Grassland", "Grass",        500,
    2020, "T1", "P1", "Wheat",     "Wheat",     "High_quality", 200
  )

  result <- redistribute_feed(d, a, verbose = FALSE)
  cm <- result |> dplyr::filter(Livestock_cat == "Cattle_milk")
  wheat_intake  <- sum(cm$intake_MgDM[cm$item_cbs == "Wheat"])
  barley_intake <- sum(cm$intake_MgDM[cm$item_cbs == "Barley"])
  total_intake  <- sum(cm$intake_MgDM)

  expect_true(wheat_intake > 0,
              info = "Wheat must absorb redirect since Grassland is capped for Cattle_milk")
  expect_true(barley_intake / total_intake <= 0.3 + 1e-6,
              info = "Barley share of LC diet must respect the 0.3 cap")
  expect_true(wheat_intake <= 200 + 1e-6,
              info = "Wheat intake must not exceed Wheat availability (supply decrement)")
})

test_that("max_intake_share chain: non-grass substitutes exhausted, strict cap drops leftover", {
  # Strict cap (no force-back-to-Grassland). With Barley cap 0.3 and Grassland
  # cap 0.1 for Cattle_milk, the share-of-final math reduces Barley to its
  # cap, Grassland is unreachable (capped), and only the 5 Mg of Wheat is
  # available as a substitute. The leftover excess is DROPPED (no warning,
  # no Grassland row at final level); scaling_factor on the affected
  # demand_id reflects the resulting supply shortfall.
  assign("max_intake_share",
         tibble::tibble(Livestock_cat = c("Cattle_milk", "Cattle_milk"),
                        item_cbs = c("Barley", "Grassland"),
                        max_intake_share = c(0.3, 0.1)),
         envir = .GlobalEnv)
  on.exit(rm("max_intake_share", envir = .GlobalEnv), add = TRUE)

  d <- tibble::tribble(
    ~Year, ~Territory, ~Sub_territory, ~Livestock_cat,
    ~item_cbs, ~Cat_1, ~Cat_feed, ~demand_MgDM, ~fixed_demand,
    2020, "T1", "P1", "Cattle_milk", "Barley", "Barley", "High_quality", 1000, TRUE
  )
  a <- tibble::tribble(
    ~Year, ~Territory, ~Sub_territory, ~item_cbs, ~Cat_1, ~Cat_feed, ~Avail_MgDM,
    2020, "T1", "P1", "Barley",    "Barley",    "High_quality", 2000,
    2020, "T1", "P1", "Wheat",     "Wheat",     "High_quality",    5,
    2020, "T1", "P1", "Grassland", "Grassland", "Grass",        5000
  )

  result <- expect_silent(
    redistribute_feed(d, a, verbose = FALSE)
  )

  cm <- result |> dplyr::filter(Livestock_cat == "Cattle_milk")

  # No final-level Grassland row should be emitted for Cattle_milk — the
  # strict cap drops leftover rather than violating Grassland's own cap.
  final_lvl <- utils::tail(sort(unique(result$hierarchy_level)), 1)
  grass_final <- cm |>
    dplyr::filter(item_cbs == "Grassland",
                  hierarchy_level == final_lvl)
  expect_true(nrow(grass_final) == 0 || sum(grass_final$intake_MgDM) <= 1e-6,
              info = "Strict cap must not push leftover excess into capped Grassland")

  # Cattle_milk should still respect both caps after reduction + reallocation.
  total_intake <- sum(cm$intake_MgDM, na.rm = TRUE)
  barley_share <- sum(cm$intake_MgDM[cm$item_cbs == "Barley"]) /
    pmax(total_intake, 1e-9)
  grass_share  <- sum(cm$intake_MgDM[cm$item_cbs == "Grassland"]) /
    pmax(total_intake, 1e-9)
  expect_true(barley_share <= 0.3 + 1e-6,
              info = "Strict Barley cap (0.3) must hold after reduction")
  expect_true(grass_share <= 0.1 + 1e-6,
              info = "Strict Grassland cap (0.1) must hold after reduction")

  # Supply shortfall must show up in scaling_factor (< 1).
  sf <- unique(cm$scaling_factor)
  expect_true(any(sf < 1 - 1e-6),
              info = "Dropped leftover must surface as scaling_factor < 1")
})

test_that("max_intake_share redirect preserves total LC diet (fixed mode)", {
  assign("max_intake_share",
         tibble::tibble(Livestock_cat = "Cattle_milk",
                        item_cbs = "Barley",
                        max_intake_share = 0.25),
         envir = .GlobalEnv)
  on.exit(rm("max_intake_share", envir = .GlobalEnv), add = TRUE)

  d <- tibble::tribble(
    ~Year, ~Territory, ~Sub_territory, ~Livestock_cat,
    ~item_cbs, ~Cat_1, ~Cat_feed, ~demand_MgDM, ~fixed_demand,
    2020, "T1", "P1", "Cattle_milk", "Barley", "Barley", "High_quality", 100, TRUE
  )
  a <- tibble::tribble(
    ~Year, ~Territory, ~Sub_territory, ~item_cbs, ~Cat_1, ~Cat_feed, ~Avail_MgDM,
    2020, "T1", "P1", "Barley",    "Barley",    "High_quality", 500,
    2020, "T1", "P1", "Wheat",     "Wheat",     "High_quality", 500,
    2020, "T1", "P1", "Grassland", "Grassland", "Grass",        500
  )

  result <- redistribute_feed(d, a, verbose = FALSE)
  cm <- result |> dplyr::filter(Livestock_cat == "Cattle_milk")
  total_intake <- sum(cm$intake_MgDM)
  total_demand <- sum(unique(cm$demand_MgDM))

  expect_equal(total_intake, total_demand, tolerance = 1e-6,
               info = "Redirect must preserve total LC intake == total LC demand in fixed mode")
})

test_that("max_intake_share redirect: supply conservation for non-Grassland redirect targets", {
  assign("max_intake_share",
         tibble::tibble(Livestock_cat = "Cattle_milk",
                        item_cbs = "Grassland",
                        max_intake_share = 0.1),
         envir = .GlobalEnv)
  on.exit(rm("max_intake_share", envir = .GlobalEnv), add = TRUE)

  d <- tibble::tribble(
    ~Year, ~Territory, ~Sub_territory, ~Livestock_cat,
    ~item_cbs, ~Cat_1, ~Cat_feed, ~demand_MgDM, ~fixed_demand,
    2020, "T1", "P1", "Cattle_milk", "Barley", "Barley", "High_quality", 100, TRUE,
    2020, "T1", "P1", "Pigs",        "Wheat",  "Wheat",  "High_quality",  50, TRUE
  )
  a <- tibble::tribble(
    ~Year, ~Territory, ~Sub_territory, ~item_cbs, ~Cat_1, ~Cat_feed, ~Avail_MgDM,
    2020, "T1", "P1", "Barley",    "Barley",    "High_quality", 400,
    2020, "T1", "P1", "Wheat",     "Wheat",     "High_quality", 150,
    2020, "T1", "P1", "Grassland", "Grassland", "Grass",        500
  )

  result <- redistribute_feed(d, a, verbose = FALSE)
  wheat_total <- sum(result$intake_MgDM[result$item_cbs == "Wheat"])

  expect_true(wheat_total <= 150 + 1e-6,
              info = "Total Wheat intake across all Livestock_cats must not exceed Wheat availability")
})

test_that("variable-mode: Zoot_fixed intake is upscaled when non-Zoot intake exceeds demand", {
  assign("max_intake_share",
         tibble::tibble(Livestock_cat = character(),
                        item_cbs = character(),
                        max_intake_share = numeric()),
         envir = .GlobalEnv)
  on.exit(rm("max_intake_share", envir = .GlobalEnv), add = TRUE)

  d <- tibble::tribble(
    ~Year, ~Territory, ~Sub_territory, ~Livestock_cat,
    ~item_cbs, ~Cat_1, ~Cat_feed, ~demand_MgDM, ~fixed_demand,
    2020, "T1", "P1", "Cattle_milk", "Barley",    "Barley",    "High_quality", 100, FALSE,
    2020, "T1", "P1", "Cattle_milk", "Milk_feed", "Milk_feed", "Zoot_fixed",    20, FALSE
  )
  a <- tibble::tribble(
    ~Year, ~Territory, ~Sub_territory, ~item_cbs, ~Cat_1, ~Cat_feed, ~Avail_MgDM,
    2020, "T1", "P1", "Barley",    "Barley",    "High_quality", 300,
    2020, "T1", "P1", "Milk_feed", "Milk_feed", "Zoot_fixed",    20
  )

  result <- redistribute_feed(d, a, verbose = FALSE)
  zoot <- result |> dplyr::filter(Cat_feed == "Zoot_fixed",
                                  Livestock_cat == "Cattle_milk")
  zoot_intake <- sum(zoot$intake_MgDM)

  expect_true(zoot_intake > 20 + 1e-6,
              info = "Zoot_fixed intake must be upscaled when non-Zoot intake exceeds non-Zoot demand (variable mode)")
})

test_that("variable-mode: Zoot_fixed intake not scaled when non-Zoot avail equals non-Zoot demand", {
  assign("max_intake_share",
         tibble::tibble(Livestock_cat = character(),
                        item_cbs = character(),
                        max_intake_share = numeric()),
         envir = .GlobalEnv)
  on.exit(rm("max_intake_share", envir = .GlobalEnv), add = TRUE)

  d <- tibble::tribble(
    ~Year, ~Territory, ~Sub_territory, ~Livestock_cat,
    ~item_cbs, ~Cat_1, ~Cat_feed, ~demand_MgDM, ~fixed_demand,
    2020, "T1", "P1", "Cattle_milk", "Barley",    "Barley",    "High_quality", 100, FALSE,
    2020, "T1", "P1", "Cattle_milk", "Milk_feed", "Milk_feed", "Zoot_fixed",    20, FALSE
  )
  a <- tibble::tribble(
    ~Year, ~Territory, ~Sub_territory, ~item_cbs, ~Cat_1, ~Cat_feed, ~Avail_MgDM,
    2020, "T1", "P1", "Barley",    "Barley",    "High_quality", 100,
    2020, "T1", "P1", "Milk_feed", "Milk_feed", "Zoot_fixed",    20
  )

  result <- redistribute_feed(d, a, verbose = FALSE)
  zoot <- result |> dplyr::filter(Cat_feed == "Zoot_fixed",
                                  Livestock_cat == "Cattle_milk")
  zoot_intake <- sum(zoot$intake_MgDM)

  expect_equal(zoot_intake, 20, tolerance = 1e-6,
               info = "Zoot_fixed intake must remain at initial demand when non-Zoot avail equals non-Zoot demand")
})

test_that("multi-year runs: each year has independent allocation and scaling", {
  assign("max_intake_share",
         tibble::tibble(Livestock_cat = character(),
                        item_cbs = character(),
                        max_intake_share = numeric()),
         envir = .GlobalEnv)
  on.exit(rm("max_intake_share", envir = .GlobalEnv), add = TRUE)

  d <- tibble::tribble(
    ~Year, ~Territory, ~Sub_territory, ~Livestock_cat,
    ~item_cbs, ~Cat_1, ~Cat_feed, ~demand_MgDM, ~fixed_demand,
    2019, "T1", "P1", "Cattle_milk", "Barley", "Barley", "High_quality",  50, FALSE,
    2020, "T1", "P1", "Cattle_milk", "Barley", "Barley", "High_quality", 100, FALSE,
    2021, "T1", "P1", "Cattle_milk", "Barley", "Barley", "High_quality", 200, FALSE
  )
  a <- tibble::tribble(
    ~Year, ~Territory, ~Sub_territory, ~item_cbs, ~Cat_1, ~Cat_feed, ~Avail_MgDM,
    2019, "T1", "P1", "Barley", "Barley", "High_quality",  10,
    2020, "T1", "P1", "Barley", "Barley", "High_quality",  80,
    2021, "T1", "P1", "Barley", "Barley", "High_quality", 400
  )

  result <- redistribute_feed(d, a, verbose = FALSE)

  per_year <- result |>
    dplyr::filter(item_cbs == "Barley") |>
    dplyr::summarize(.by = Year, total_intake = sum(intake_MgDM, na.rm = TRUE))
  avail_per_year <- c(`2019` = 10, `2020` = 80, `2021` = 400)
  for (yy in as.character(per_year$Year)) {
    expect_true(
      per_year$total_intake[per_year$Year == as.integer(yy)] <=
        avail_per_year[[yy]] + 1e-6,
      info = paste("Year", yy, "Barley intake must not exceed that year's availability")
    )
  }

  sfs <- result |>
    dplyr::summarize(.by = Year, sf = dplyr::first(scaling_factor))
  expect_true(length(unique(sfs$sf)) > 1,
              info = "Scaling factors must differ across years when demand/avail ratios differ")
})

# ===========================================================================
# High-priority gap tests (Category B: national-only avail, prioritize modes,
# reroute_excess_grass, zoot_fixed_max_multiplier, hierarchy substitution)
# ===========================================================================

test_that("national-only availability: demand at province P can source from national pool", {
  assign("max_intake_share",
         tibble::tibble(Livestock_cat = character(),
                        item_cbs = character(),
                        max_intake_share = numeric()),
         envir = .GlobalEnv)
  on.exit(rm("max_intake_share", envir = .GlobalEnv), add = TRUE)

  demand <- tibble::tribble(
    ~Year, ~Territory, ~Sub_territory, ~Livestock_cat,
    ~item_cbs, ~Cat_1, ~Cat_feed, ~demand_MgDM, ~fixed_demand,
    2020, "T1", "P1", "Cattle_milk",
    "Barley", "Barley", "High_quality", 80, TRUE
  )

  avail <- tibble::tribble(
    ~Year, ~Territory, ~Sub_territory, ~item_cbs,
    ~Cat_1, ~Cat_feed, ~Avail_MgDM,
    2020, "T1", NA, "Barley", "Barley", "High_quality", 100
  )

  result <- redistribute_feed(demand, avail, verbose = FALSE)

  barley_rows <- result[result$Livestock_cat == "Cattle_milk" &
                          result$item_cbs == "Barley", , drop = FALSE]
  expect_gt(sum(barley_rows$intake_MgDM, na.rm = TRUE), 0)
  expect_true(all(is.na(barley_rows$original_Province)))
})

test_that("national-only availability: supply conservation holds across provinces", {
  assign("max_intake_share",
         tibble::tibble(Livestock_cat = character(),
                        item_cbs = character(),
                        max_intake_share = numeric()),
         envir = .GlobalEnv)
  on.exit(rm("max_intake_share", envir = .GlobalEnv), add = TRUE)

  demand <- tibble::tribble(
    ~Year, ~Territory, ~Sub_territory, ~Livestock_cat,
    ~item_cbs, ~Cat_1, ~Cat_feed, ~demand_MgDM, ~fixed_demand,
    2020, "T1", "P1", "Cattle_milk", "Barley", "Barley", "High_quality", 120, TRUE,
    2020, "T1", "P2", "Cattle_milk", "Barley", "Barley", "High_quality", 120, TRUE
  )
  avail <- tibble::tribble(
    ~Year, ~Territory, ~Sub_territory, ~item_cbs,
    ~Cat_1, ~Cat_feed, ~Avail_MgDM,
    2020, "T1", NA, "Barley", "Barley", "High_quality", 150
  )

  result <- redistribute_feed(demand, avail, verbose = FALSE)
  barley_rows <- result[result$item_cbs == "Barley", , drop = FALSE]
  expect_lte(sum(barley_rows$intake_MgDM, na.rm = TRUE), 150 + 1e-6)
})

test_that("prioritize_monogastric = FALSE + variable mode: no preferential mono ordering", {
  assign("max_intake_share",
         tibble::tibble(Livestock_cat = character(),
                        item_cbs = character(),
                        max_intake_share = numeric()),
         envir = .GlobalEnv)
  on.exit(rm("max_intake_share", envir = .GlobalEnv), add = TRUE)

  demand <- tibble::tribble(
    ~Year, ~Territory, ~Sub_territory, ~Livestock_cat,
    ~item_cbs, ~Cat_1, ~Cat_feed, ~demand_MgDM, ~fixed_demand,
    2020, "T1", "P1", "Pigs",        "Barley", "Barley", "High_quality", 100, FALSE,
    2020, "T1", "P1", "Cattle_milk", "Barley", "Barley", "High_quality", 100, FALSE
  )
  avail <- tibble::tribble(
    ~Year, ~Territory, ~Sub_territory, ~item_cbs,
    ~Cat_1, ~Cat_feed, ~Avail_MgDM,
    2020, "T1", "P1", "Barley", "Barley", "High_quality", 50
  )

  result <- redistribute_feed(demand, avail,
                              prioritize_monogastric = FALSE,
                              verbose = FALSE)
  pigs_barley <- sum(result$intake_MgDM[result$Livestock_cat == "Pigs" &
                                           result$item_cbs == "Barley"], na.rm = TRUE)
  cattle_barley <- sum(result$intake_MgDM[result$Livestock_cat == "Cattle_milk" &
                                             result$item_cbs == "Barley"], na.rm = TRUE)
  expect_lte(pigs_barley + cattle_barley, 50 + 1e-6)
  expect_gt(cattle_barley, 0)
  expect_lt(abs(pigs_barley - cattle_barley), 5)
})

test_that("prioritize_monogastric = FALSE + mixed mode: both fixed and variable rows present", {
  assign("max_intake_share",
         tibble::tibble(Livestock_cat = character(),
                        item_cbs = character(),
                        max_intake_share = numeric()),
         envir = .GlobalEnv)
  on.exit(rm("max_intake_share", envir = .GlobalEnv), add = TRUE)

  demand <- tibble::tribble(
    ~Year, ~Territory, ~Sub_territory, ~Livestock_cat,
    ~item_cbs, ~Cat_1, ~Cat_feed, ~demand_MgDM, ~fixed_demand,
    2020, "T1", "P1", "Pigs",        "Barley",    "Barley",    "High_quality",  60, TRUE,
    2020, "T1", "P1", "Cattle_milk", "Barley",    "Barley",    "High_quality",  60, FALSE,
    2020, "T1", "P1", "Cattle_milk", "Grassland", "Grassland", "Grass",        100, TRUE
  )
  avail <- tibble::tribble(
    ~Year, ~Territory, ~Sub_territory, ~item_cbs,
    ~Cat_1, ~Cat_feed, ~Avail_MgDM,
    2020, "T1", "P1", "Barley",    "Barley",    "High_quality",  80,
    2020, "T1", "P1", "Grassland", "Grassland", "Grass",        200
  )

  result <- redistribute_feed(demand, avail,
                              prioritize_monogastric = FALSE,
                              verbose = FALSE)

  expect_true(any(result$fixed_demand))
  expect_true(any(!result$fixed_demand))
  barley_total <- sum(result$intake_MgDM[result$item_cbs == "Barley"], na.rm = TRUE)
  expect_lte(barley_total, 80 + 1e-6)
})

test_that("prioritize_monogastric = TRUE + variable mode: monogastric still goes first", {
  assign("max_intake_share",
         tibble::tibble(Livestock_cat = character(),
                        item_cbs = character(),
                        max_intake_share = numeric()),
         envir = .GlobalEnv)
  on.exit(rm("max_intake_share", envir = .GlobalEnv), add = TRUE)

  demand <- tibble::tribble(
    ~Year, ~Territory, ~Sub_territory, ~Livestock_cat,
    ~item_cbs, ~Cat_1, ~Cat_feed, ~demand_MgDM, ~fixed_demand,
    2020, "T1", "P1", "Pigs",        "Barley", "Barley", "High_quality", 100, FALSE,
    2020, "T1", "P1", "Cattle_milk", "Barley", "Barley", "High_quality", 100, FALSE
  )
  avail <- tibble::tribble(
    ~Year, ~Territory, ~Sub_territory, ~item_cbs,
    ~Cat_1, ~Cat_feed, ~Avail_MgDM,
    2020, "T1", "P1", "Barley", "Barley", "High_quality", 40
  )

  result <- redistribute_feed(demand, avail,
                              prioritize_monogastric = TRUE,
                              verbose = FALSE)
  pigs_barley <- sum(result$intake_MgDM[result$Livestock_cat == "Pigs" &
                                           result$item_cbs == "Barley"], na.rm = TRUE)
  cattle_barley <- sum(result$intake_MgDM[result$Livestock_cat == "Cattle_milk" &
                                             result$item_cbs == "Barley"], na.rm = TRUE)
  expect_gt(pigs_barley, cattle_barley)
})

test_that("non-Grassland Grass intake never exceeds its availability", {
  assign("max_intake_share",
         tibble::tibble(Livestock_cat = character(),
                        item_cbs = character(),
                        max_intake_share = numeric()),
         envir = .GlobalEnv)
  on.exit(rm("max_intake_share", envir = .GlobalEnv), add = TRUE)

  demand <- tibble::tribble(
    ~Year, ~Territory, ~Sub_territory, ~Livestock_cat,
    ~item_cbs, ~Cat_1, ~Cat_feed, ~demand_MgDM, ~fixed_demand,
    2020, "T1", "P1", "Pigs",        "Acorns",    "Acorns",    "Grass", 500, TRUE,
    2020, "T1", "P1", "Cattle_milk", "Acorns",    "Acorns",    "Grass", 500, TRUE,
    2020, "T1", "P1", "Cattle_milk", "Grassland", "Grassland", "Grass", 100, TRUE
  )
  avail <- tibble::tribble(
    ~Year, ~Territory, ~Sub_territory, ~item_cbs,
    ~Cat_1, ~Cat_feed, ~Avail_MgDM,
    2020, "T1", "P1", "Acorns",    "Acorns",    "Grass",   50,
    2020, "T1", "P1", "Grassland", "Grassland", "Grass", 2000
  )

  result <- redistribute_feed(demand, avail, verbose = FALSE)
  acorn_total <- sum(result$intake_MgDM[result$item_cbs == "Acorns" &
                                           result$Cat_feed == "Grass"], na.rm = TRUE)
  expect_lte(acorn_total, 50 + 1e-6)
})

test_that("zoot_fixed_max_multiplier = 1 caps intake strictly at Avail_MgDM", {
  assign("max_intake_share",
         tibble::tibble(Livestock_cat = character(),
                        item_cbs = character(),
                        max_intake_share = numeric()),
         envir = .GlobalEnv)
  on.exit(rm("max_intake_share", envir = .GlobalEnv), add = TRUE)

  demand <- tibble::tribble(
    ~Year, ~Territory, ~Sub_territory, ~Livestock_cat,
    ~item_cbs, ~Cat_1, ~Cat_feed, ~demand_MgDM, ~fixed_demand,
    2020, "T1", "P1", "Cattle_milk", "Milk_feed", "Milk_feed", "Zoot_fixed", 500, TRUE
  )
  avail <- tibble::tribble(
    ~Year, ~Territory, ~Sub_territory, ~item_cbs,
    ~Cat_1, ~Cat_feed, ~Avail_MgDM,
    2020, "T1", "P1", "Milk_feed", "Milk_feed", "Zoot_fixed", 100
  )

  result <- redistribute_feed(demand, avail,
                              zoot_fixed_max_multiplier = 1,
                              verbose = FALSE)
  zoot_total <- sum(result$intake_MgDM[result$Cat_feed == "Zoot_fixed"], na.rm = TRUE)
  expect_lte(zoot_total, 100 + 1e-6)
})

test_that("zoot_fixed_max_multiplier = 5 allows up to 5x avail", {
  assign("max_intake_share",
         tibble::tibble(Livestock_cat = character(),
                        item_cbs = character(),
                        max_intake_share = numeric()),
         envir = .GlobalEnv)
  on.exit(rm("max_intake_share", envir = .GlobalEnv), add = TRUE)

  demand <- tibble::tribble(
    ~Year, ~Territory, ~Sub_territory, ~Livestock_cat,
    ~item_cbs, ~Cat_1, ~Cat_feed, ~demand_MgDM, ~fixed_demand,
    2020, "T1", "P1", "Cattle_milk", "Milk_feed", "Milk_feed", "Zoot_fixed", 500, TRUE
  )
  avail <- tibble::tribble(
    ~Year, ~Territory, ~Sub_territory, ~item_cbs,
    ~Cat_1, ~Cat_feed, ~Avail_MgDM,
    2020, "T1", "P1", "Milk_feed", "Milk_feed", "Zoot_fixed", 100
  )

  result <- redistribute_feed(demand, avail,
                              zoot_fixed_max_multiplier = 5,
                              verbose = FALSE)
  zoot_total <- sum(result$intake_MgDM[result$Cat_feed == "Zoot_fixed"], na.rm = TRUE)
  expect_gt(zoot_total, 100 + 1e-6)
  expect_lte(zoot_total, 500 + 1e-6)
})

test_that("demand with missing Cat_1 and Cat_feed gets filled from items_full / Cats", {
  assign("max_intake_share",
         tibble::tibble(Livestock_cat = character(),
                        item_cbs = character(),
                        max_intake_share = numeric()),
         envir = .GlobalEnv)
  on.exit(rm("max_intake_share", envir = .GlobalEnv), add = TRUE)

  d <- tibble::tribble(
    ~Year, ~Territory, ~Sub_territory, ~Livestock_cat,
    ~item_cbs, ~Cat_1, ~Cat_feed, ~demand_MgDM, ~fixed_demand,
    2020, "T1", "P1", "Cattle_milk",
    "Barley", NA_character_, NA_character_, 50, TRUE
  )
  a <- tibble::tribble(
    ~Year, ~Territory, ~Sub_territory, ~item_cbs,
    ~Cat_1, ~Cat_feed, ~Avail_MgDM,
    2020, "T1", "P1", "Barley", NA_character_, NA_character_, 100
  )
  result <- redistribute_feed(d, a, verbose = FALSE)
  # Cat_1 and Cat_feed should be resolved from items_full/Cats lookups.
  expect_true(any(result$Cat_1 == "Barley" & result$item_cbs == "Barley"),
              info = "Cat_1 lookup from items_full should fill missing Cat_1")
  expect_true(any(result$Cat_feed == "High_quality"),
              info = "Cat_feed lookup from Cats should fill missing Cat_feed")
})

test_that("Territory column absent: function falls back to 'All_territories'", {
  assign("max_intake_share",
         tibble::tibble(Livestock_cat = character(),
                        item_cbs = character(),
                        max_intake_share = numeric()),
         envir = .GlobalEnv)
  on.exit(rm("max_intake_share", envir = .GlobalEnv), add = TRUE)

  d <- tibble::tribble(
    ~Year, ~Sub_territory, ~Livestock_cat,
    ~item_cbs, ~Cat_1, ~Cat_feed, ~demand_MgDM, ~fixed_demand,
    2020, "P1", "Cattle_milk",
    "Barley", "Barley", "High_quality", 50, TRUE
  )
  a <- tibble::tribble(
    ~Year, ~Sub_territory, ~item_cbs,
    ~Cat_1, ~Cat_feed, ~Avail_MgDM,
    2020, "P1", "Barley", "Barley", "High_quality", 100
  )

  result <- redistribute_feed(d, a, verbose = FALSE)
  expect_true("Territory" %in% names(result))
  expect_true(all(result$Territory == "All_territories"),
              info = "Missing Territory column should default to 'All_territories'")
})

test_that("empty Feed_avail: every fixed demand row gets Grassland fallback", {
  assign("max_intake_share",
         tibble::tibble(Livestock_cat = character(),
                        item_cbs = character(),
                        max_intake_share = numeric()),
         envir = .GlobalEnv)
  on.exit(rm("max_intake_share", envir = .GlobalEnv), add = TRUE)

  d <- tibble::tribble(
    ~Year, ~Territory, ~Sub_territory, ~Livestock_cat,
    ~item_cbs, ~Cat_1, ~Cat_feed, ~demand_MgDM, ~fixed_demand,
    2020, "T1", "P1", "Cattle_milk",
    "Barley", "Barley", "High_quality", 50, TRUE
  )
  a <- tibble::tribble(
    ~Year, ~Territory, ~Sub_territory, ~item_cbs,
    ~Cat_1, ~Cat_feed, ~Avail_MgDM
  )

  result <- redistribute_feed(d, a, verbose = FALSE)
  # Fixed mode with no avail still fills demand via unlimited Grassland.
  cm_total <- sum(result$intake_MgDM[result$Livestock_cat == "Cattle_milk"],
                  na.rm = TRUE)
  expect_equal(cm_total, 50, tolerance = 1e-6,
               info = "Empty avail should still fulfil fixed demand via Grassland fallback")
  expect_true(all(result$item_cbs == "Grassland"),
              info = "All intake rows should be Grassland when avail is empty")
})

test_that("multi-province trade: excess in one province supplies deficit in another", {
  assign("max_intake_share",
         tibble::tibble(Livestock_cat = character(),
                        item_cbs = character(),
                        max_intake_share = numeric()),
         envir = .GlobalEnv)
  on.exit(rm("max_intake_share", envir = .GlobalEnv), add = TRUE)

  d <- tibble::tribble(
    ~Year, ~Territory, ~Sub_territory, ~Livestock_cat,
    ~item_cbs, ~Cat_1, ~Cat_feed, ~demand_MgDM, ~fixed_demand,
    2020, "T1", "P1", "Cattle_milk", "Barley", "Barley", "High_quality", 200, TRUE,
    2020, "T1", "P2", "Cattle_milk", "Barley", "Barley", "High_quality",  10, TRUE
  )
  a <- tibble::tribble(
    ~Year, ~Territory, ~Sub_territory, ~item_cbs,
    ~Cat_1, ~Cat_feed, ~Avail_MgDM,
    2020, "T1", "P1", "Barley", "Barley", "High_quality",  50,
    2020, "T1", "P2", "Barley", "Barley", "High_quality", 300
  )

  result <- redistribute_feed(d, a, verbose = FALSE)
  # P1 needed extra Barley; P2 had excess. Trade level should fire.
  p1_rows <- result[result$Sub_territory == "P1" & result$item_cbs == "Barley", ]
  trade_rows <- p1_rows[p1_rows$original_Province == "P2" &
                          !is.na(p1_rows$original_Province), , drop = FALSE]
  expect_true(nrow(trade_rows) > 0,
              info = "P2 Barley should trade to P1 via inter-provincial trade")
  # Total Barley intake <= total Barley availability
  expect_true(sum(result$intake_MgDM[result$item_cbs == "Barley"]) <= 350 + 1e-6,
              info = "Trade must not violate total availability")
})

test_that("primary-hierarchy substitution fires when item_cbs exact is unavailable", {
  assign("max_intake_share",
         tibble::tibble(Livestock_cat = character(),
                        item_cbs = character(),
                        max_intake_share = numeric()),
         envir = .GlobalEnv)
  on.exit(rm("max_intake_share", envir = .GlobalEnv), add = TRUE)

  demand <- tibble::tribble(
    ~Year, ~Territory, ~Sub_territory, ~Livestock_cat,
    ~item_cbs, ~Cat_1, ~Cat_feed, ~demand_MgDM, ~fixed_demand,
    2020, "T1", "P1", "Cattle_milk", "Barley", "Barley", "High_quality", 50, TRUE
  )
  avail <- tibble::tribble(
    ~Year, ~Territory, ~Sub_territory, ~item_cbs,
    ~Cat_1, ~Cat_feed, ~Avail_MgDM,
    2020, "T1", "P1", "Wheat", "Wheat", "High_quality", 80
  )

  result <- redistribute_feed(demand, avail, verbose = FALSE)
  sub_rows <- result[result$Livestock_cat == "Cattle_milk" &
                       result$intake_MgDM > 1e-9, , drop = FALSE]
  expect_true(any(sub_rows$hierarchy_level %in%
                    c("2_Cat_1_sub", "3_Cat_feed_sub")))
  expect_true(any(sub_rows$item_cbs == "Wheat"))
  expect_true(all(sub_rows$original_demand_item == "Barley"))
})
