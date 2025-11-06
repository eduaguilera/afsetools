# Tests for NPP calculation functions based on Spain_Hist and Global usage patterns

test_that("Calc_NPP_potentials calculates realistic NPP from climate data", {
  
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
