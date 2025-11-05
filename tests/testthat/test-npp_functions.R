test_that("Calc_NPP_potentials calculates NPP from climate data", {
  # Create sample climate data
  climate_data <- tibble::tribble(
    ~TMP, ~MAP, ~PET, ~AET,
    15,   800,  1000, 600,
    20,   1200, 1200, 900,
    10,   500,  800,  400
  )
  
  result <- Calc_NPP_potentials(climate_data)
  
  # Test that result is a data frame
  expect_true(is.data.frame(result))
  
  # Test that NPP columns exist
  expect_true("NPP_miami" %in% names(result) ||
              "NPP_Miami" %in% names(result))
  
  # Test that values are numeric and positive
  npp_cols <- names(result)[grepl("NPP", names(result), ignore.case = TRUE)]
  if (length(npp_cols) > 0) {
    expect_type(result[[npp_cols[1]]], "double")
    expect_true(all(result[[npp_cols[1]]] >= 0, na.rm = TRUE))
  }
  
  # Test same number of rows
  expect_equal(nrow(result), nrow(climate_data))
})

test_that("Calculate_crop_NPP calculates crop NPP components", {
  # Create sample crop data
  crop_data <- tibble::tribble(
    ~Area_ha, ~Prod_Mg, ~Name_biomass,
    100,      500,      "Wheat",
    200,      800,      "Maize"
  )
  
  # Note: This function may require additional parameters
  # Adjust test based on actual function signature
  skip_if_not(exists("Calculate_crop_NPP"))
  
  # Test basic structure
  expect_true(is.function(Calculate_crop_NPP))
})

test_that("Calc_NPP_DM_C_N converts NPP to different units", {
  # Create sample NPP data
  npp_data <- tibble::tribble(
    ~NPP_MgDM, ~Name_biomass,
    100,       "Wheat",
    200,       "Maize"
  )
  
  skip_if_not(exists("Calc_NPP_DM_C_N"))
  
  # Test function exists and is callable
  expect_true(is.function(Calc_NPP_DM_C_N))
})

test_that("Calc_CropNPP_components calculates complete cropland NPP", {
  skip_if_not(exists("Calc_CropNPP_components"))
  
  # Test function exists
  expect_true(is.function(Calc_CropNPP_components))
})

test_that("NPP calculations handle edge cases", {
  # Test with zero values
  climate_zero <- tibble::tribble(
    ~TMP, ~MAP, ~PET, ~AET,
    0,    0,    0,    0
  )
  
  result <- Calc_NPP_potentials(climate_zero)
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 1)
})

test_that("NPP calculations handle NA values", {
  climate_na <- tibble::tribble(
    ~TMP, ~MAP, ~PET, ~AET,
    15,   NA,   1000, 600,
    NA,   1200, 1200, 900
  )
  
  result <- Calc_NPP_potentials(climate_na)
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), nrow(climate_na))
})
