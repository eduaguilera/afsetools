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
