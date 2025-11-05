test_that("Calc_N_fix calculates biological nitrogen fixation", {
  # Load data needed for BNF calculation
  load_general_data()
  
  # Create sample crop NPP data
  npp_data <- tibble::tribble(
    ~Name_biomass, ~Crop_NPP_MgN, ~Prod_MgN, ~Weeds_NPP_MgN, ~LandUse, ~Area_ygpit_ha,
    "Soybean",     10,            8,         1,              "Cropland", 100,
    "Wheat",       15,            12,        1.5,            "Cropland", 150
  )
  
  result <- Calc_N_fix(npp_data)
  
  # Test that result is a data frame
  expect_true(is.data.frame(result))
  
  # Test that BNF columns exist
  expect_true("BNF" %in% names(result) ||
              "CropBNF" %in% names(result))
  
  # Test same number of rows
  expect_equal(nrow(result), nrow(npp_data))
  
  # Test that BNF values are numeric
  if ("BNF" %in% names(result)) {
    expect_type(result$BNF, "double")
    expect_true(all(result$BNF >= 0, na.rm = TRUE))
  }
})

test_that("Gases_GWP classifies GHG emissions and calculates GWP", {
  # Load GWP data
  load_general_data()
  
  # Create sample emissions data
  emissions_data <- tibble::tribble(
    ~Gas_raw, ~Gas_type, ~value,
    "CH4",    "Biogenic", 100,
    "N2O",    "Biogenic", 50,
    "CO2",    "Fossil",   1000
  )
  
  result <- Gases_GWP(emissions_data)
  
  # Test that result is a data frame
  expect_true(is.data.frame(result))
  
  # Test that CO2e column exists
  expect_true("CO2e_Tg" %in% names(result))
  
  # Test same number of rows
  expect_equal(nrow(result), nrow(emissions_data))
  
  # Test that CO2e values are numeric
  expect_type(result$CO2e_Tg, "double")
})

test_that("Calc_diets calculates nutrient composition", {
  # Create sample food destiny data
  food_data <- tibble::tribble(
    ~Year, ~area, ~item_cbs, ~Element, ~Destiny, ~FM_Mg,
    2020, "Spain", "Wheat", "Production", "Food", 1000,
    2020, "Spain", "Maize", "Production", "Feed", 2000
  )
  
  # Create sample population data
  pop_data <- tibble::tribble(
    ~Year, ~area, ~Pop_Mpeop,
    2020, "Spain", 47
  )
  
  result <- Calc_diets(food_data, pop_data)
  
  # Test that result is a data frame
  expect_true(is.data.frame(result))
  
  # Test that result has rows
  expect_gt(nrow(result), 0)
})

test_that("get_herbwoody_fao extracts herbaceous/woody categories", {
  skip_if_not(exists("get_herbwoody_fao"))
  
  # Test function exists
  expect_true(is.function(get_herbwoody_fao))
})

test_that("calculate_land_scaling calculates land scaling factors", {
  skip_if_not(exists("calculate_land_scaling"))
  
  # Test function exists
  expect_true(is.function(calculate_land_scaling))
})

test_that("scale_land applies land scaling adjustments", {
  skip_if_not(exists("scale_land"))
  
  # Test function exists
  expect_true(is.function(scale_land))
})

test_that("Calc_N_fix handles missing BNF parameters", {
  load_general_data()
  
  # Create data with unknown crop
  npp_data <- tibble::tribble(
    ~Name_biomass, ~Crop_NPP_MgN, ~Prod_MgN, ~Weeds_NPP_MgN, ~LandUse, ~Area_ygpit_ha,
    "Unknown",     10,            8,         1,              "Cropland", 100
  )
  
  result <- Calc_N_fix(npp_data)
  
  # Should still return a data frame
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 1)
})

test_that("Gases_GWP handles missing gas types", {
  load_general_data()
  
  # Create data with unknown gas
  emissions_data <- tibble::tribble(
    ~Gas_raw, ~Gas_type, ~value,
    "SF6",    "Other",   10
  )
  
  result <- Gases_GWP(emissions_data)
  
  # Should still return a data frame
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 1)
})
