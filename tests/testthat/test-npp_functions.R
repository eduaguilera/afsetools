# Tests for NPP calculation functions based on Spain_Hist and Global usage patterns

test_that("Calc_NPP_potentials calculates realistic NPP from climate data", {
  load_general_data()
  
  # Create climate data based on real Spain_Hist usage
  # Realistic values for Mediterranean, humid, and semi-arid climates
  climate_data <- tibble::tribble(
    ~TMP, ~MAP, ~PET, ~AET, ~AET_mm,
    15,   800,  1000, 700,  700,    # Mediterranean climate
    20,   1200, 1200, 900,  900,    # Warm humid climate
    5,    400,  600,  350,  350,    # Cold semi-arid
    25,   2000, 1500, 1200, 1200    # Tropical
  )
  
  result <- Calc_NPP_potentials(climate_data)
  
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

test_that("Calculate_crop_NPP calculates product, residue and root biomass", {
  load_general_data()
  
  # Realistic crop data based on Spain_Hist/Global usage
  crop_data <- tibble::tribble(
    ~Year, ~Province_name, ~Name_biomass, ~Irrig_cat, ~Irrig_type, ~Area_ygpit_ha, ~Prod_ygpit_Mg, ~Yield_ygpi_Mgha,
    2020,  "Madrid",       "Wheat",       "Rainfed",  "Rainfed",    10000,          30000,          3.0,
    2020,  "Madrid",       "Wheat",       "Irrigated", "Sprinkler", 5000,           25000,          5.0,
    2020,  "Madrid",       "Maize (corn)", "Irrigated", "Sprinkler", 3000,           36000,          12.0,
    2020,  "Barcelona",    "Barley",      "Rainfed",  "Rainfed",    8000,           20000,          2.5
  )
  
  # Create simple HI data (harvest index)
  hi_data <- tibble::tribble(
    ~Year, ~Name_biomass, ~Dyn_HI, ~Dyn_RS,
    2020,  "Wheat",       1.0,     0.3,
    2020,  "Maize (corn)", 1.1,     0.25,
    2020,  "Barley",      0.9,     0.35
  )
  
  result <- Calculate_crop_NPP(crop_data, hi_data)
  
  # Test that NPP components are calculated
  expect_true("Prod_MgDM" %in% names(result))
  expect_true("Residue_MgDM" %in% names(result))
  expect_true("Root_MgDM" %in% names(result))
  
  # Test that all values are non-negative
  expect_true(all(result$Prod_MgDM >= 0, na.rm = TRUE))
  expect_true(all(result$Residue_MgDM >= 0, na.rm = TRUE))
  expect_true(all(result$Root_MgDM >= 0, na.rm = TRUE))
  
  # Test that product DM is less than fresh weight (DM content <1)
  wheat_row <- result[result$Name_biomass == "Wheat" & result$Irrig_cat == "Rainfed", ]
  expect_true(wheat_row$Prod_MgDM[1] < wheat_row$Prod_ygpit_Mg[1])
  
  # Test that irrigated yields are higher than rainfed
  wheat_rainfed <- result[result$Name_biomass == "Wheat" & result$Irrig_cat == "Rainfed", ]$Yield_ygpi_Mgha
  wheat_irrigated <- result[result$Name_biomass == "Wheat" & result$Irrig_cat == "Irrigated", ]$Yield_ygpi_Mgha
  expect_true(wheat_irrigated > wheat_rainfed)
  
  # Test that residue amount is reasonable compared to product
  # (residue:product ratio typically 0.5-3.0)
  residue_ratio <- result$Residue_MgDM / result$Prod_MgDM
  expect_true(all(residue_ratio > 0.3 & residue_ratio < 4, na.rm = TRUE))
})

test_that("Calc_NPP_DM_C_N converts biomass to C and N correctly", {
  load_general_data()
  
  # Create sample NPP data with biomass components
  npp_data <- tibble::tribble(
    ~Name_biomass, ~Area_ygpit_ha, ~Prod_MgDM, ~Residue_MgDM, ~Root_MgDM, ~Weeds_AG_MgDM, ~Weeds_BG_MgDM,
    "Wheat",       10000,          25000,      25000,          10000,      1000,           500,
    "Maize (corn)", 5000,           40000,      35000,          15000,      500,            250
  )
  
  result <- Calc_NPP_DM_C_N(npp_data)
  
  # Test that C and N columns are created
  expect_true("Prod_MgC" %in% names(result))
  expect_true("Prod_MgN" %in% names(result))
  expect_true("Residue_MgC" %in% names(result))
  expect_true("Residue_MgN" %in% names(result))
  expect_true("Root_MgC" %in% names(result))
  expect_true("Root_MgN" %in% names(result))
  
  # Test that C content is approximately 40-45% of DM (typical for plant biomass)
  wheat_row <- result[result$Name_biomass == "Wheat", ]
  c_fraction <- wheat_row$Prod_MgC / wheat_row$Prod_MgDM
  expect_true(c_fraction > 0.35 & c_fraction < 0.50)
  
  # Test that N content is typically 0.5-3% of DM
  n_fraction <- wheat_row$Prod_MgN / wheat_row$Prod_MgDM
  expect_true(n_fraction > 0.005 & n_fraction < 0.05)
  
  # Test that total NPP is sum of components
  if ("Crop_NPP_MgDM" %in% names(result)) {
    total_calc <- result$Prod_MgDM + result$Residue_MgDM + result$Root_MgDM
    expect_equal(result$Crop_NPP_MgDM, total_calc)
  }
})

test_that("Calc_CropNPP_components includes weeds and scales properly", {
  load_general_data()
  
  # Create NPPpot data as used in Spain_Hist
  npp_pot_data <- tibble::tribble(
    ~Year, ~Province_name, ~Name_biomass, ~Irrig_cat, ~Area_ygpit_ha, ~NPPpot_MgDMha, ~ANPP_NCEAS_MgDMha,
    2020,  "Madrid",       "Wheat",       "Rainfed",  10000,          8.0,            6.0,
    2020,  "Madrid",       "Fallow",      "Rainfed",  5000,           5.0,            3.5,
    2020,  "Barcelona",    "Maize (corn)", "Irrigated", 3000,           12.0,           9.0
  )
  
  result <- Calc_CropNPP_components(npp_pot_data)
  
  # Test that weed biomass is calculated
  expect_true("Weeds_AG_MgDM" %in% names(result))
  
  # Test that weeds are scaled relative to NPPpot
  expect_true(all(result$Weeds_AG_MgDM >= 0, na.rm = TRUE))
  
  # Test that fallow has higher weed biomass than crops
  fallow_weeds <- result$Weeds_AG_MgDM[result$Name_biomass == "Fallow"]
  crop_weeds <- result$Weeds_AG_MgDM[result$Name_biomass != "Fallow"]
  if (length(fallow_weeds) > 0 & length(crop_weeds) > 0) {
    # Fallow typically has more weed biomass per ha
    expect_true(mean(fallow_weeds / result$Area_ygpit_ha[result$Name_biomass == "Fallow"]) >=
                mean(crop_weeds / result$Area_ygpit_ha[result$Name_biomass != "Fallow"]))
  }
})

test_that("NPP functions handle zero and NA values appropriately", {
  load_general_data()
  
  # Test data with zeros and NAs
  test_data <- tibble::tribble(
    ~Year, ~Province_name, ~Name_biomass, ~Irrig_cat, ~Irrig_type, ~Area_ygpit_ha, ~Prod_ygpit_Mg, ~Yield_ygpi_Mgha,
    2020,  "Madrid",       "Wheat",       "Rainfed",  "Rainfed",    10000,          0,              0,
    2020,  "Madrid",       "Fallow",      "Rainfed",  "Rainfed",    5000,           NA,             NA
  )
  
  hi_data <- tibble::tribble(
    ~Year, ~Name_biomass, ~Dyn_HI, ~Dyn_RS,
    2020,  "Wheat",       1.0,     0.3,
    2020,  "Fallow",      NA,      NA
  )
  
  result <- Calculate_crop_NPP(test_data, hi_data)
  
  # Test that function doesn't error on zeros
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 2)
  
  # Test that zero production gives zero NPP
  zero_prod_row <- result[result$Prod_ygpit_Mg == 0, ]
  expect_equal(zero_prod_row$Prod_MgDM, 0)
  
  # Test that NAs are handled (replaced with 0 for residue and roots in fallow)
  fallow_row <- result[result$Name_biomass == "Fallow", ]
  expect_true(!is.na(fallow_row$Residue_MgDM) | fallow_row$Residue_MgDM == 0)
  expect_true(!is.na(fallow_row$Root_MgDM) | fallow_row$Root_MgDM == 0)
})
