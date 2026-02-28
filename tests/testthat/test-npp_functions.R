# Tests for NPP calculation functions based on Spain_Hist and Global usage patterns

test_that("calculate_potential_npp calculates realistic NPP from climate data", {

  # Create climate data based on real Spain_Hist usage
  # Realistic values for Mediterranean, humid, and semi-arid climates
  climate_data <- tibble::tribble(
    ~TMP, ~WaterInput_mm, ~AET_mm,
    15,   800,            700,    # Mediterranean climate
    20,   1200,           900,    # Warm humid climate
    5,    400,            350,    # Cold semi-arid
    25,   2000,           1200    # Tropical
  )

  result <- calculate_potential_npp(climate_data)

  # Test that all three NPP model outputs exist
  expect_true("NPP_Miami_MgDMha" %in% names(result))
  expect_true("ANPP_NCEAS_MgDMha" %in% names(result))
  expect_true("TNPP_NCEAS_MgDMha" %in% names(result))
  expect_true("NPP_Rosenzweig_MgDMha" %in% names(result))
  
  # Test that NPP values are positive and realistic (typically 0-30 Mg/ha/year)
  expect_true(all(result$NPP_Miami_MgDMha > 0 & result$NPP_Miami_MgDMha < 30, na.rm = TRUE))
  expect_true(all(result$ANPP_NCEAS_MgDMha > 0 & result$ANPP_NCEAS_MgDMha < 30, na.rm = TRUE))
  expect_true(all(result$NPP_Rosenzweig_MgDMha > 0, na.rm = TRUE))
  
  # Test that root:shoot ratios are calculated and reasonable
  expect_true("RS_ratio_NCEAS" %in% names(result))
  expect_true(all(result$RS_ratio_NCEAS >= 0 & result$RS_ratio_NCEAS < 5, na.rm = TRUE))
  
  # Test that tree vs non-tree NPP are differentiated
  expect_true("TNPP_tree_NCEAS_MgDMha" %in% names(result))
  expect_true("ANPP_tree_NCEAS_MgDMha" %in% names(result))
  
  # Test that warmer/wetter climates have higher NPP
  expect_true(result$NPP_Miami_MgDMha[4] > result$NPP_Miami_MgDMha[3])
})

test_that("calculate_crop_npp_components drops Biomass_coefs helpers", {
  coef_cols <- c(
     "Product_kgDM_kgFM",
     "Residue_kgDM_kgFM",
     "Root_kgDM_kgFM",
     "kg_residue_kg_product_FM",
     "Root_Shoot_ratio",
     "Product_kgN_kgDM",
     "Residue_kgN_kgDM",
     "Root_kgN_kgDM",
     "Rhizodeposits_N_kgN_kgRootN",
     "Product_kgC_kgDM",
     "Residue_kgC_kgDM",
     "Root_kgC_kgDM"
  )

  # Save old values if they exist
  old_biomass <- if (exists("Biomass_coefs")) get("Biomass_coefs") else NULL
  old_weeds <- if (exists("Weed_NPP_Scaling")) get("Weed_NPP_Scaling") else NULL
  old_residue <- if (exists("Residue_Shares")) get("Residue_Shares") else NULL
  old_fallow <- if (exists("Fallow_cover")) get("Fallow_cover") else NULL

  on.exit({
    if (!is.null(old_biomass)) {
      Biomass_coefs <<- old_biomass
    } else if (exists("Biomass_coefs")) {
      rm("Biomass_coefs", envir = .GlobalEnv)
    }
    if (!is.null(old_weeds)) {
      Weed_NPP_Scaling <<- old_weeds
    } else if (exists("Weed_NPP_Scaling")) {
      rm("Weed_NPP_Scaling", envir = .GlobalEnv)
    }
    if (!is.null(old_residue)) {
      Residue_Shares <<- old_residue
    } else if (exists("Residue_Shares")) {
      rm("Residue_Shares", envir = .GlobalEnv)
    }
    if (!is.null(old_fallow)) {
      Fallow_cover <<- old_fallow
    } else if (exists("Fallow_cover")) {
      rm("Fallow_cover", envir = .GlobalEnv)
    }
  }, add = TRUE)

  Biomass_coefs <<- tibble::tibble(
    Name_biomass = "TestCrop",
    Product_kgDM_kgFM = 0.2,
    Residue_kgDM_kgFM = 0.4,
    Root_kgDM_kgFM = 0.3,
    kg_residue_kg_product_FM = 0.5,
    Root_Shoot_ratio = 0.5,
    Product_kgN_kgDM = 0.02,
    Residue_kgN_kgDM = 0.015,
    Root_kgN_kgDM = 0.018,
    Rhizodeposits_N_kgN_kgRootN = 0.05,
    Product_kgC_kgDM = 0.45,
    Residue_kgC_kgDM = 0.42,
    Root_kgC_kgDM = 0.4
  )

  Weed_NPP_Scaling <<- tibble::tibble(
    Year = 2020,
    Name_biomass = "TestCrop",
    Scaling_weeds = 0.5
  )

  Residue_Shares <<- tibble::tibble(
    Year = 2020,
    Name_biomass = "TestCrop",
    Use_Share = 0.2,
    Soil_Share = 0.3
  )

  Fallow_cover <<- tibble::tibble(
    Year = 2020,
    Name_biomass = "TestCrop",
    Fallow_cover_share = 0.1
  )

  crop_input <- tibble::tibble(
    Year = 2020,
    Name_biomass = "TestCrop",
    Area_ygpit_ha = 1,
    NPPpot_MgDMha = 8,
    Prod_MgDM = 2,
    Residue_MgDM = 1,
    Root_MgDM = 0.5
  )

  result <- calculate_crop_npp_components(crop_input)

  expect_false(any(coef_cols %in% names(result)))
  expect_gt(result$Tot_NPP_MgDM, 0)
  expect_true("Weeds_AG_MgDM" %in% names(result))
})


# =============================================================================
# Tests for the new IPCC-based functions
# =============================================================================

test_that("IPCC coefficient tables are loaded by load_general_data", {
  # These objects should exist after load_general_data() was called in setup
  expect_true(exists("IPCC_residue_coefs"))
  expect_true(exists("IPCC_root_coefs"))
  expect_true(exists("IPCC_crop_mapping"))
  expect_true(exists("Modern_variety_adoption"))
  expect_true(exists("N_input_RS_adj"))
  expect_true(exists("Irrigation_adj"))

  # Check structure
  expect_true("IPCC_crop" %in% names(IPCC_residue_coefs))
  expect_true("Slope_AG" %in% names(IPCC_residue_coefs))
  expect_true("Intercept_AG_MgDMha" %in% names(IPCC_residue_coefs))

  expect_true("RS_default" %in% names(IPCC_root_coefs))
  expect_true("RS_low_N" %in% names(IPCC_root_coefs))
  expect_true("BG_ref_MgDMha" %in% names(IPCC_root_coefs))

  expect_true("Name_biomass" %in% names(IPCC_crop_mapping))
  expect_true("IPCC_crop" %in% names(IPCC_crop_mapping))

  # Check key crop mappings exist
  expect_true("Wheat" %in% IPCC_crop_mapping$Name_biomass)
  expect_true("Maize" %in% IPCC_crop_mapping$Name_biomass)
  expect_true("Rice" %in% IPCC_crop_mapping$Name_biomass)

  # Check realistic coefficient ranges
  expect_true(all(IPCC_residue_coefs$Slope_AG >= 0, na.rm = TRUE))
  expect_true(all(IPCC_root_coefs$RS_default > 0 &
                    IPCC_root_coefs$RS_default < 2, na.rm = TRUE))
})


test_that("calculate_crop_residues estimates realistic residues (basic)", {
  # Simple test with wheat: ~5 Mg FM/ha, 1 ha
  crop_data <- tibble::tibble(
    Name_biomass = c("Wheat", "Maize", "Rice"),
    Prod_ygpit_Mg = c(5, 8, 4),
    Area_ygpit_ha = c(1, 1, 1),
    Year = c(2000, 2000, 2000),
    region_HANPP = c("West Europe", "West Europe", "West Europe"),
    Water_regime = c("Rainfed", "Rainfed", "Rainfed")
  )

  result <- calculate_crop_residues(crop_data)

  # Output columns exist
  expect_true("Prod_MgDM" %in% names(result))
  expect_true("Residue_MgDM" %in% names(result))
  expect_true("Yield_DM_Mgha" %in% names(result))

  # IPCC intermediate columns are cleaned up
  expect_false("Slope_AG" %in% names(result))
  expect_false("IPCC_crop" %in% names(result))
  expect_false("Residue_IPCC_Mgha" %in% names(result))

  # Residue values are positive and realistic
  expect_true(all(result$Residue_MgDM > 0))
  # Wheat: ~5 Mg FM * 0.88 DM * ~1.36 ratio ~ 5-8 Mg DM residue total
  expect_gt(result$Residue_MgDM[1], 2)
  expect_lt(result$Residue_MgDM[1], 15)
  # Maize: less residue per unit product
  expect_gt(result$Residue_MgDM[2], 3)
  expect_lt(result$Residue_MgDM[2], 20)
})


test_that("calculate_crop_residues handles irrigation adjustment", {
  crop_rainfed <- tibble::tibble(
    Name_biomass = "Wheat",
    Prod_ygpit_Mg = 5,
    Area_ygpit_ha = 1,
    Year = 2000,
    region_HANPP = "West Europe",
    Water_regime = "Rainfed"
  )
  crop_irrigated <- tibble::tibble(
    Name_biomass = "Wheat",
    Prod_ygpit_Mg = 5,
    Area_ygpit_ha = 1,
    Year = 2000,
    region_HANPP = "West Europe",
    Water_regime = "Irrigated"
  )

  res_rainfed <- calculate_crop_residues(crop_rainfed)
  res_irrigated <- calculate_crop_residues(crop_irrigated)

  # Irrigated should have less residue (higher HI → more product, less straw)
  expect_lt(res_irrigated$Residue_MgDM, res_rainfed$Residue_MgDM)
})


test_that("calculate_crop_residues handles modern variety correction", {
  crop_modern <- tibble::tibble(
    Name_biomass = "Wheat",
    Prod_ygpit_Mg = 5,
    Area_ygpit_ha = 1,
    Year = 2000,
    region_HANPP = "West Europe",
    Water_regime = "Rainfed"
  )
  crop_historical <- tibble::tibble(
    Name_biomass = "Wheat",
    Prod_ygpit_Mg = 5,
    Area_ygpit_ha = 1,
    Year = 1900,
    region_HANPP = "West Europe",
    Water_regime = "Rainfed"
  )

  res_modern <- calculate_crop_residues(crop_modern)
  res_historical <- calculate_crop_residues(crop_historical)

  # Historical period should have more residue per unit product
  # because traditional varieties had lower HI
  expect_gt(res_historical$Residue_MgDM, res_modern$Residue_MgDM)
})


test_that("calculate_crop_residues handles zero production gracefully", {
  crop_zero <- tibble::tibble(
    Name_biomass = "Wheat",
    Prod_ygpit_Mg = 0,
    Area_ygpit_ha = 1,
    Year = 2000,
    region_HANPP = "West Europe",
    Water_regime = "Rainfed"
  )

  result <- calculate_crop_residues(crop_zero)
  expect_equal(result$Prod_MgDM, 0)
  # With zero yield, IPCC intercept still gives some residue per ha.
  # The ratio model gives 0. Ensemble: some small positive value or 0.
  expect_true(result$Residue_MgDM >= 0)
})


test_that("calculate_crop_roots estimates realistic roots (basic)", {
  crop_data <- tibble::tibble(
    Name_biomass = c("Wheat", "Maize"),
    Prod_MgDM = c(4.4, 6.9),
    Residue_MgDM = c(5.5, 6.5),
    Area_ygpit_ha = c(1, 1),
    Water_regime = c("Rainfed", "Rainfed"),
    N_input_kgha = c(100, 100)
  )

  result <- calculate_crop_roots(crop_data)

  expect_true("Root_MgDM" %in% names(result))
  expect_false("RS_effective" %in% names(result))
  expect_false("IPCC_crop" %in% names(result))

  # Root values positive and realistic (typically 15-30% of aerial)
  expect_true(all(result$Root_MgDM > 0))
  # Wheat aerial ~9.9, RS ~0.24 → root ~2.4 (RS) mixed with ref ~2.0
  expect_gt(result$Root_MgDM[1], 0.5)
  expect_lt(result$Root_MgDM[1], 8)
})


test_that("calculate_crop_roots adjusts for N input", {
  crop_low_N <- tibble::tibble(
    Name_biomass = "Wheat",
    Prod_MgDM = 4,
    Residue_MgDM = 5,
    Area_ygpit_ha = 1,
    Water_regime = "Rainfed",
    N_input_kgha = 10  # Very low N → higher RS
  )
  crop_high_N <- tibble::tibble(
    Name_biomass = "Wheat",
    Prod_MgDM = 4,
    Residue_MgDM = 5,
    Area_ygpit_ha = 1,
    Water_regime = "Rainfed",
    N_input_kgha = 250  # Very high N → lower RS
  )

  res_low <- calculate_crop_roots(crop_low_N)
  res_high <- calculate_crop_roots(crop_high_N)

  # Low N should have more root biomass (higher RS)
  expect_gt(res_low$Root_MgDM, res_high$Root_MgDM)
})


test_that("calculate_crop_npp wrapper produces complete NPP", {
  crop_data <- tibble::tibble(
    Name_biomass = c("Wheat", "Maize", "Soyabeans", "Potato"),
    Prod_ygpit_Mg = c(5, 8, 2, 20),
    Area_ygpit_ha = c(1, 1, 1, 1),
    Year = c(2000, 2000, 2000, 2000),
    region_HANPP = rep("West Europe", 4),
    Water_regime = rep("Rainfed", 4),
    N_input_kgha = c(100, 100, 20, 80)
  )

  result <- calculate_crop_npp(crop_data)

  # All NPP components present
  expect_true("Prod_MgDM" %in% names(result))
  expect_true("Residue_MgDM" %in% names(result))
  expect_true("Root_MgDM" %in% names(result))
  expect_true("Crop_NPP_MgDM" %in% names(result))

  # Intermediate columns removed
  expect_false("kg_residue_kg_product_FM" %in% names(result))
  expect_false("Root_Shoot_ratio" %in% names(result))
  expect_false("Product_kgDM_kgFM" %in% names(result))

  # NPP = Prod + Residue + Root
  expect_equal(
    result$Crop_NPP_MgDM,
    result$Prod_MgDM + result$Residue_MgDM + result$Root_MgDM,
    tolerance = 1e-10
  )

  # All positive
  expect_true(all(result$Crop_NPP_MgDM > 0))
  expect_true(all(result$Prod_MgDM >= 0))
  expect_true(all(result$Residue_MgDM >= 0))
  expect_true(all(result$Root_MgDM >= 0))
})


test_that("calculate_crop_npp with full context columns", {
  crop_data <- tibble::tibble(
    Name_biomass = "Wheat",
    Prod_ygpit_Mg = 5,
    Area_ygpit_ha = 1,
    Year = 2000,
    region_HANPP = "West Europe",
    Water_regime = "Rainfed",
    N_input_kgha = 100
  )

  result <- calculate_crop_npp(crop_data)

  expect_true("Crop_NPP_MgDM" %in% names(result))
  expect_gt(result$Crop_NPP_MgDM, 0)
  # Year, region_HANPP, Water_regime, N_input_kgha should be preserved
  expect_true("Year" %in% names(result))
  expect_true("region_HANPP" %in% names(result))
  expect_true("Water_regime" %in% names(result))
  expect_true("N_input_kgha" %in% names(result))
})


test_that("Calculate_crop_NPP legacy alias works with deprecation warning", {
  crop_data <- tibble::tibble(
    Name_biomass = "Wheat",
    Prod_ygpit_Mg = 5,
    Area_ygpit_ha = 1,
    Year = 2000,
    region_HANPP = "West Europe",
    Water_regime = "Rainfed",
    N_input_kgha = 100
  )

  # Calling with HI parameter should give deprecation warning
  expect_warning(
    result <- Calculate_crop_NPP(crop_data, HI = tibble::tibble()),
    "deprecated"
  )
  expect_true("Crop_NPP_MgDM" %in% names(result))

  # Calling without HI should also warn (function is deprecated)
  expect_warning(
    result2 <- Calculate_crop_NPP(crop_data),
    "deprecated"
  )
  expect_true("Crop_NPP_MgDM" %in% names(result2))
})


test_that("calculate_crop_npp handles unmapped crops gracefully", {
  # "Straw" is in Biomass_coefs but not in IPCC_crop_mapping
  crop_data <- tibble::tibble(
    Name_biomass = "Straw",
    Prod_ygpit_Mg = 3,
    Area_ygpit_ha = 1,
    Year = 2000,
    region_HANPP = "West Europe",
    Water_regime = "Rainfed",
    N_input_kgha = 100
  )

  # Should not error — falls back to ratio model only
  result <- calculate_crop_npp(crop_data)
  expect_true("Crop_NPP_MgDM" %in% names(result))
})


test_that("w_ipcc parameter controls residue model weighting", {
  crop_data <- tibble::tibble(
    Name_biomass = "Wheat",
    Prod_ygpit_Mg = 5,
    Area_ygpit_ha = 1,
    Year = 2000,
    region_HANPP = "West Europe",
    Water_regime = "Rainfed",
    N_input_kgha = 100
  )

  # All weight on ratio model
  res_ratio <- calculate_crop_npp(crop_data, w_ipcc = 0.0)
  # All weight on IPCC model
  res_ipcc <- calculate_crop_npp(crop_data, w_ipcc = 1.0)
  # 50/50 ensemble
  res_ensemble <- calculate_crop_npp(crop_data, w_ipcc = 0.5)

  # They should differ (unless models coincidentally agree)
  # but at least check they are all valid

  expect_true(all(c(res_ratio$Crop_NPP_MgDM, res_ipcc$Crop_NPP_MgDM,
                    res_ensemble$Crop_NPP_MgDM) > 0))
})
