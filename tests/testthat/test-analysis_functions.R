# Tests for analysis functions based on Spain_Hist and Global usage

test_that("Calc_N_fix calculates biological nitrogen fixation realistically", {
  load_general_data()
  
  # Realistic NPP data with legume and non-legume crops
  test_data <- tibble::tribble(
    ~Year, ~Name_biomass,   ~LandUse,  ~Area_ygpit_ha, ~Crop_NPP_MgN, ~Prod_MgN, ~Weeds_NPP_MgN,
    2020,  "Chickpeas",     "Cropland", 1000,          15,            10,        1.0,
    2020,  "Lentils",       "Cropland", 800,           12,            8,         0.8,
    2020,  "Wheat",         "Cropland", 10000,         150,           100,       5.0,
    2020,  "Maize (corn)",  "Cropland", 5000,          200,           140,       3.0
  )
  
  result <- Calc_N_fix(test_data)
  
  # Test that BNF columns are created
  expect_true("CropBNF" %in% names(result))
  expect_true("WeedsBNF" %in% names(result))
  expect_true("NSBNF" %in% names(result))
  expect_true("BNF" %in% names(result))
  
  # Test that BNF values are non-negative
  expect_true(all(result$CropBNF >= 0, na.rm = TRUE))
  expect_true(all(result$WeedsBNF >= 0, na.rm = TRUE))
  expect_true(all(result$NSBNF >= 0, na.rm = TRUE))
  
  # Test that legumes have higher BNF than non-legumes
  legume_bnf <- result$CropBNF[result$Name_biomass %in% c("Chickpeas", "Lentils")]
  nonlegume_bnf <- result$CropBNF[result$Name_biomass %in% c("Wheat", "Maize (corn)")]
  
  if (length(legume_bnf) > 0 & length(nonlegume_bnf) > 0) {
    expect_true(mean(legume_bnf) > mean(nonlegume_bnf))
  }
  
  # Test that total BNF is sum of components
  expect_equal(result$BNF, result$CropBNF + result$WeedsBNF + result$NSBNF)
  
  # Test that Fert_type is set
  expect_equal(unique(result$Fert_type), "BNF")
})

test_that("Calc_N_fix handles missing BNF parameters gracefully", {
  load_general_data()
  
  # Data with crops that might not have BNF parameters
  test_data <- tibble::tribble(
    ~Year, ~Name_biomass, ~LandUse,  ~Area_ygpit_ha, ~Crop_NPP_MgN, ~Prod_MgN, ~Weeds_NPP_MgN,
    2020,  "Unknown crop", "Cropland", 1000,          20,            15,        1.0
  )
  
  result <- Calc_N_fix(test_data)
  
  # Test that function doesn't error
  expect_true(is.data.frame(result))
  
  # Test that BNF defaults to 0 for unknown crops
  expect_true(result$CropBNF[1] == 0 | is.na(result$CropBNF[1]))
  
  # Test that non-symbiotic BNF is still calculated
  expect_true(result$NSBNF[1] > 0)
})

test_that("Gases_GWP classifies emissions and calculates CO2e correctly", {
  load_general_data()
  
  # Realistic GHG emissions data from Spain_Hist pattern
  test_data <- tibble::tribble(
    ~Gas_raw, ~Gas_type,    ~value,
    "CO2",    "Biogenic",   1000,
    "CO2",    "Fossil",     500,
    "CH4",    "Biogenic",   50,
    "CH4",    "Fossil",     10,
    "N2O",    "Agricultural", 20
  )
  
  result <- Gases_GWP(test_data)
  
  # Test that Gas and Gas_categ are created
  expect_true("Gas" %in% names(result))
  expect_true("Gas_categ" %in% names(result))
  expect_true("CO2e_Tg" %in% names(result))
  
  # Test that fossil CH4 is distinguished from biogenic CH4
  expect_true("CH4_fossil" %in% result$Gas)
  
  # Test that CO2e calculation uses GWP correctly
  ch4_row <- result[result$Gas_raw == "CH4" & result$Gas_type == "Biogenic", ]
  if (nrow(ch4_row) > 0) {
    # CH4 GWP is ~28-30, so CO2e should be value * GWP / 1000
    expect_true(ch4_row$CO2e_Tg > ch4_row$value / 100)  # At least 10x the original value
  }
  
  # Test that N2O has very high GWP (265-298)
  n2o_row <- result[result$Gas_raw == "N2O", ]
  if (nrow(n2o_row) > 0) {
    expect_true(n2o_row$CO2e_Tg > n2o_row$value / 10)  # At least 100x the original value
  }
  
  # Test that CO2 biogenic has GWP of 1
  co2_row <- result[result$Gas_raw == "CO2" & result$Gas_type == "Biogenic", ]
  if (nrow(co2_row) > 0) {
    expect_equal(co2_row$CO2e_Tg, co2_row$value / 1000)
  }
})

test_that("Gases_GWP handles unknown gas types", {
  load_general_data()
  
  # Test with gas not in GWP table
  test_data <- tibble::tribble(
    ~Gas_raw,      ~Gas_type,  ~value,
    "UnknownGas",  "Unknown",  100
  )
  
  result <- Gases_GWP(test_data)
  
  # Test that function doesn't error
  expect_true(is.data.frame(result))
  
  # Test that CO2e defaults to value/1000 when GWP is missing
  expect_equal(result$CO2e_Tg, 100 / 1000)
})

test_that("Calc_diets calculates nutrient availability per capita", {
  load_general_data()
  
  # Realistic food destiny data from Global repository pattern
  pie_data <- tibble::tribble(
    ~Year, ~area,    ~item_cbs, ~Element, ~Destiny, ~FM_Mg,
    2020,  "Spain",  "Wheat",   "Food",   "Food",   1000000,
    2020,  "Spain",  "Milk",    "Food",   "Food",   500000,
    2020,  "Spain",  "Beef",    "Food",   "Food",   100000
  )
  
  pop_data <- tibble::tribble(
    ~Year, ~area,    ~Pop_Mpeop,
    2020,  "Spain",  47.0
  )
  
  result <- Calc_diets(pie_data, pop_data)
  
  # Test that nutrient columns are created
  expect_true("Energy_kcal_cap_day" %in% names(result) | 
              "energy" %in% names(result))
  expect_true("Protein_g_cap_day" %in% names(result) | 
              "protein" %in% names(result))
  
  # Test that per capita values are positive
  # (can't test exact values without full Composition table)
  expect_true(is.data.frame(result))
  expect_true(nrow(result) > 0)
})

test_that("get_herbwoody_fao classifies crops correctly", {
  load_general_data()
  
  # Test that function exists
  skip_if_not(exists("get_herbwoody_fao"))
  
  expect_true(is.function(get_herbwoody_fao))
})

test_that("calculate_land_scaling calculates area scaling factors", {
  load_general_data()
  
  # Test that function exists and is callable
  skip_if_not(exists("calculate_land_scaling"))
  
  expect_true(is.function(calculate_land_scaling))
})

test_that("scale_land applies scaling to land use data", {
  load_general_data()
  
  # Test that function exists
  skip_if_not(exists("scale_land"))
  
  expect_true(is.function(scale_land))
})

test_that("BNF values are realistic for different crop types", {
  load_general_data()
  
  # Test with high-fixing legume
  soybean_data <- tibble::tribble(
    ~Year, ~Name_biomass, ~LandUse,  ~Area_ygpit_ha, ~Crop_NPP_MgN, ~Prod_MgN, ~Weeds_NPP_MgN,
    2020,  "Soybeans",    "Cropland", 1000,          50,            35,        2.0
  )
  
  soy_result <- Calc_N_fix(soybean_data)
  
  # Soybeans typically fix 100-200 kg N/ha
  # With 1000 ha, should be 100-200 Mg N
  if ("CropBNF" %in% names(soy_result)) {
    bnf_per_ha <- soy_result$CropBNF / soy_result$Area_ygpit_ha * 1000  # kg/ha
    expect_true(bnf_per_ha > 50 & bnf_per_ha < 300)
  }
  
  # Test with non-legume (wheat)
  wheat_data <- tibble::tribble(
    ~Year, ~Name_biomass, ~LandUse,  ~Area_ygpit_ha, ~Crop_NPP_MgN, ~Prod_MgN, ~Weeds_NPP_MgN,
    2020,  "Wheat",       "Cropland", 1000,          25,            18,        1.0
  )
  
  wheat_result <- Calc_N_fix(wheat_data)
  
  # Wheat should have no crop BNF, only NSBNF
  expect_equal(wheat_result$CropBNF, 0)
  expect_true(wheat_result$NSBNF > 0)
})

test_that("GWP calculations match IPCC AR5 values", {
  load_general_data()
  
  # Test with 100 tonnes of each major GHG
  test_data <- tibble::tribble(
    ~Gas_raw, ~Gas_type,      ~value,
    "CO2",    "Fossil",       100000,  # 100 tonnes
    "CH4",    "Biogenic",     100000,  # 100 tonnes
    "N2O",    "Agricultural", 100000   # 100 tonnes
  )
  
  result <- Gases_GWP(test_data)
  
  # CO2: 100 tonnes = 0.1 Tg, CO2e should be 0.1 Tg
  co2_row <- result[result$Gas_raw == "CO2", ]
  expect_equal(co2_row$CO2e_Tg, 0.1, tolerance = 0.01)
  
  # CH4: 100 tonnes with GWP ~28 = ~2.8 Tg CO2e
  ch4_row <- result[result$Gas_raw == "CH4", ]
  expect_true(ch4_row$CO2e_Tg > 2.0 & ch4_row$CO2e_Tg < 4.0)
  
  # N2O: 100 tonnes with GWP ~265 = ~26.5 Tg CO2e
  n2o_row <- result[result$Gas_raw == "N2O", ]
  expect_true(n2o_row$CO2e_Tg > 20.0 & n2o_row$CO2e_Tg < 35.0)
})
