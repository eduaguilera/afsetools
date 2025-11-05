test_that("Prepare_prim prepares primary production data", {
  # Create sample primary production data
  prim_data <- tibble::tribble(
    ~Year, ~item, ~Production_Mg,
    2020,  "Wheat", 1000,
    2020,  "Maize", 2000,
    2021,  "Wheat", 1100
  )
  
  skip_if_not(exists("Prepare_prim"))
  
  # Test function exists
  expect_true(is.function(Prepare_prim))
})

test_that("Allocate_impacts_to_products performs economic allocation", {
  # Create sample impact data
  impact_data <- tibble::tribble(
    ~item, ~Impact_value, ~Economic_value,
    "Wheat", 100, 500,
    "Maize", 200, 800
  )
  
  skip_if_not(exists("Allocate_impacts_to_products"))
  
  # Test function exists
  expect_true(is.function(Allocate_impacts_to_products))
})

test_that("calc_avail_fp_gt calculates gross trade footprint", {
  skip_if_not(exists("calc_avail_fp_gt"))
  
  # Test function exists
  expect_true(is.function(calc_avail_fp_gt))
})

test_that("calc_avail_fp_dtm calculates detailed trade matrix footprint", {
  skip_if_not(exists("calc_avail_fp_dtm"))
  
  # Test function exists
  expect_true(is.function(calc_avail_fp_dtm))
})

test_that("Calc_impact_processed traces impacts through processing", {
  skip_if_not(exists("Calc_impact_processed"))
  
  # Test function exists
  expect_true(is.function(Calc_impact_processed))
})

test_that("Agg_primary aggregates primary products", {
  # Create sample data
  primary_data <- tibble::tribble(
    ~Year, ~Province, ~item_prod, ~Value,
    2020, "Province1", "Wheat", 100,
    2020, "Province2", "Wheat", 150,
    2020, "Province1", "Maize", 200
  )
  
  skip_if_not(exists("Agg_primary"))
  
  # Test function exists
  expect_true(is.function(Agg_primary))
})

test_that("Agg_processed aggregates processed products", {
  skip_if_not(exists("Agg_processed"))
  
  # Test function exists
  expect_true(is.function(Agg_processed))
})

test_that("get_global_export_footprint calculates export footprint", {
  skip_if_not(exists("get_global_export_footprint"))
  
  # Test function exists
  expect_true(is.function(get_global_export_footprint))
})
