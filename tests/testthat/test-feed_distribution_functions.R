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
