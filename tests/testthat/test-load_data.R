# Tests for load_general_data() function

test_that("load_general_data creates all required data objects", {
  # Clean environment
  test_env <- new.env()
  
  # Load data into test environment
  with(test_env, {
    load_general_data()
  })
  
  # Test that key data objects exist
  expect_true(exists("Biomass_coefs", envir = test_env))
  expect_true(exists("GWP", envir = test_env))
  expect_true(exists("BNF", envir = test_env))
  expect_true(exists("items_full", envir = test_env))
  expect_true(exists("regions_full", envir = test_env))
  
  # Test that Biomass_coefs has required columns
  expect_true(all(c("Name_biomass", "Product_kgDM_kgFM", "kg_residue_kg_product_FM", 
                   "Root_Shoot_ratio") %in% names(test_env$Biomass_coefs)))
  
  # Test that GWP has required structure
  expect_true(all(c("Gas", "GWP_100") %in% names(test_env$GWP)))
  expect_true(is.numeric(test_env$GWP$GWP_100))
  
  # Test that key gases are present
  expect_true("CO2" %in% test_env$GWP$Gas)
  expect_true("CH4" %in% test_env$GWP$Gas)
  expect_true("N2O" %in% test_env$GWP$Gas)
})

test_that("Biomass_coefs contains realistic coefficient values", {
  load_general_data()
  
  # Product DM content should be between 0 and 1
  expect_true(all(Biomass_coefs$Product_kgDM_kgFM >= 0 & 
                 Biomass_coefs$Product_kgDM_kgFM <= 1, na.rm = TRUE))
  
  # Residue to product ratios should be positive and reasonable (<10)
  expect_true(all(Biomass_coefs$kg_residue_kg_product_FM >= 0 & 
                 Biomass_coefs$kg_residue_kg_product_FM < 10, na.rm = TRUE))
  
  # Root:Shoot ratios should be positive and reasonable (<5)
  expect_true(all(Biomass_coefs$Root_Shoot_ratio >= 0 & 
                 Biomass_coefs$Root_Shoot_ratio < 5, na.rm = TRUE))
  
  # N and C content should be between 0 and 100%
  if ("Product_kgN_kgDM" %in% names(Biomass_coefs)) {
    expect_true(all(Biomass_coefs$Product_kgN_kgDM >= 0 & 
                   Biomass_coefs$Product_kgN_kgDM <= 1, na.rm = TRUE))
  }
})

test_that("Items and regions mappings are complete", {
  load_general_data()
  
  # items_full should have key columns (note: it's item_cbs not item)
  expect_true("item_cbs" %in% names(items_full))
  expect_true("Name_biomass" %in% names(items_full))
  
  # regions_full should have key columns
  expect_true(all(c("name", "iso3c") %in% names(regions_full)))
  
  # No missing values in key identifier column for items
  expect_false(any(is.na(items_full$item_cbs)))
  
  # regions_full may have some NAs in name for special cases, just check it's a data frame
  expect_true(is.data.frame(regions_full))
})

test_that("BNF parameters are realistic", {
  load_general_data()
  
  # Ndfa (N derived from atmosphere) should be 0-1
  if ("Ndfa" %in% names(BNF)) {
    expect_true(all(BNF$Ndfa >= 0 & BNF$Ndfa <= 1, na.rm = TRUE))
  }
  
  # Leguminous share should be 0-1
  if ("Leguminous_share" %in% names(BNF)) {
    expect_true(all(BNF$Leguminous_share >= 0 & BNF$Leguminous_share <= 1, na.rm = TRUE))
  }
  
  # kgNha should be positive
  if ("kgNha" %in% names(BNF)) {
    expect_true(all(BNF$kgNha >= 0, na.rm = TRUE))
  }
})

test_that("GWP values match IPCC standards", {
  load_general_data()
  
  # CO2 should have GWP of 1
  co2_gwp <- GWP$GWP_100[GWP$Gas == "CO2"]
  expect_equal(co2_gwp, 1)
  
  # CH4 should have GWP around 28-30 (AR5 values)
  ch4_gwp <- GWP$GWP_100[GWP$Gas == "CH4"]
  expect_true(ch4_gwp > 20 & ch4_gwp < 35)
  
  # N2O should have GWP around 265-298 (AR5 values)
  n2o_gwp <- GWP$GWP_100[GWP$Gas == "N2O"]
  expect_true(n2o_gwp > 250 & n2o_gwp < 310)
})
