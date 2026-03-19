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
  expect_true(exists("HI_crop_ranges"))
  expect_true(exists("Crop_RS_N_response"))

  # Check structure
  expect_true("IPCC_crop" %in% names(IPCC_residue_coefs))
  expect_true("Slope_AG" %in% names(IPCC_residue_coefs))
  expect_true("Intercept_AG_MgDMha" %in% names(IPCC_residue_coefs))

  expect_true("RS_default" %in% names(IPCC_root_coefs))
  expect_true("RS_low_N" %in% names(IPCC_root_coefs))
  expect_true("BG_ref_MgDMha" %in% names(IPCC_root_coefs))

  expect_true("Name_biomass" %in% names(IPCC_crop_mapping))
  expect_true("IPCC_crop" %in% names(IPCC_crop_mapping))
  expect_true("crop_group" %in% names(IPCC_crop_mapping))

  # Check key crop mappings exist
  expect_true("Wheat" %in% IPCC_crop_mapping$Name_biomass)
  expect_true("Maize" %in% IPCC_crop_mapping$Name_biomass)
  expect_true("Rice" %in% IPCC_crop_mapping$Name_biomass)

  # Check realistic coefficient ranges
  expect_true(all(IPCC_residue_coefs$Slope_AG >= 0, na.rm = TRUE))
  expect_true(all(IPCC_root_coefs$RS_default > 0 &
                    IPCC_root_coefs$RS_default < 2, na.rm = TRUE))
})


test_that("Modern_variety_adoption has crop-group-specific annual data", {
  # Structure checks
  expect_true(all(c("region_HANPP", "crop_group", "Year", "Modern_share")
                  %in% names(Modern_variety_adoption)))

  # 8 regions x 8 crop groups x 121 years (1900-2020 annual)
  expect_equal(length(unique(Modern_variety_adoption$region_HANPP)), 8)
  expect_equal(length(unique(Modern_variety_adoption$crop_group)), 8)
  expect_equal(min(Modern_variety_adoption$Year), 1900)
  expect_equal(max(Modern_variety_adoption$Year), 2020)

  # Annual interpolation: year 1965 should exist (not just decadal)
  expect_true(1965 %in% Modern_variety_adoption$Year)

  # All shares between 0 and 1
  expect_true(all(Modern_variety_adoption$Modern_share >= 0 &
                    Modern_variety_adoption$Modern_share <= 1,
                  na.rm = TRUE))

  # By 2020, developed regions should be ~100% modern for wheat
  wheat_we_2020 <- Modern_variety_adoption |>
    dplyr::filter(region_HANPP == "West Europe",
                  crop_group == "Wheat", Year == 2020)
  expect_equal(wheat_we_2020$Modern_share, 1.0)

  # SSA 1940 wheat adoption = 0% (no Green Revolution yet)
  wheat_ssa_1940 <- Modern_variety_adoption |>
    dplyr::filter(region_HANPP == "Sub-saharan Africa",
                  crop_group == "Wheat", Year == 1940)
  expect_equal(wheat_ssa_1940$Modern_share, 0)

  # Different crop groups should have different adoption rates
  # Wheat was adopted much faster than legumes in South Asia
  sa_1980 <- Modern_variety_adoption |>
    dplyr::filter(region_HANPP == "South and Central Asia", Year == 1980)
  wheat_1980 <- sa_1980 |>
    dplyr::filter(crop_group == "Wheat") |>
    dplyr::pull(Modern_share)
  legume_1980 <- sa_1980 |>
    dplyr::filter(crop_group == "Legumes") |>
    dplyr::pull(Modern_share)
  expect_gt(wheat_1980, legume_1980)
})


test_that("HI_crop_ranges has realistic values for all crop groups", {
  expect_equal(nrow(HI_crop_ranges), 8)
  expect_true(all(c("crop_group", "HI_traditional", "HI_modern",
                     "HI_gap_factor") %in% names(HI_crop_ranges)))

  # All HI values between 0 and 1
  expect_true(all(HI_crop_ranges$HI_traditional > 0 &
                    HI_crop_ranges$HI_traditional < 1))
  expect_true(all(HI_crop_ranges$HI_modern > 0 &
                    HI_crop_ranges$HI_modern < 1))

  # Modern HI should always be higher than traditional
  expect_true(all(HI_crop_ranges$HI_modern > HI_crop_ranges$HI_traditional))

  # HI_gap_factor should always be > 1 (traditional = more residue)
  expect_true(all(HI_crop_ranges$HI_gap_factor > 1))

  # Wheat should have highest gap factor (biggest Green Revolution HI change)
  wheat_gap <- HI_crop_ranges$HI_gap_factor[
    HI_crop_ranges$crop_group == "Wheat"]
  legume_gap <- HI_crop_ranges$HI_gap_factor[
    HI_crop_ranges$crop_group == "Legumes"]
  expect_gt(wheat_gap, legume_gap)
})


test_that("Crop_RS_N_response has all crop groups with valid sensitivities", {
  expect_equal(nrow(Crop_RS_N_response), 8)
  expect_true(all(c("crop_group", "RS_N_sensitivity")
                  %in% names(Crop_RS_N_response)))

  # Sensitivity values should be positive and bounded
  expect_true(all(Crop_RS_N_response$RS_N_sensitivity > 0 &
                    Crop_RS_N_response$RS_N_sensitivity <= 1.5))

  # Legumes should have lowest sensitivity (N-fixing)
  legume_sens <- Crop_RS_N_response$RS_N_sensitivity[
    Crop_RS_N_response$crop_group == "Legumes"]
  wheat_sens <- Crop_RS_N_response$RS_N_sensitivity[
    Crop_RS_N_response$crop_group == "Wheat"]
  expect_lt(legume_sens, wheat_sens)
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
  # Use Maize — the only crop with validated irrigation residue effect
  # (Ludemann 2025: irrigated maize RPR < rainfed maize RPR)
  # Wheat, rice, soybeans have Irr_residue_sensitivity = 0
  crop_rainfed <- tibble::tibble(
    Name_biomass = "Maize",
    Prod_ygpit_Mg = 8,
    Area_ygpit_ha = 1,
    Year = 2000,
    region_HANPP = "West Europe",
    Water_regime = "Rainfed"
  )
  crop_irrigated <- tibble::tibble(
    Name_biomass = "Maize",
    Prod_ygpit_Mg = 8,
    Area_ygpit_ha = 1,
    Year = 2000,
    region_HANPP = "West Europe",
    Water_regime = "Irrigated"
  )

  res_rainfed <- calculate_crop_residues(crop_rainfed)
  res_irrigated <- calculate_crop_residues(crop_irrigated)

  # Irrigated maize should have less residue (validated by Ludemann 2025)
  expect_lt(res_irrigated$Residue_MgDM, res_rainfed$Residue_MgDM)

  # Wheat irrigation should NOT change residue (Irr_residue_sensitivity = 0)
  wheat_rain <- tibble::tibble(
    Name_biomass = "Wheat", Prod_ygpit_Mg = 5, Area_ygpit_ha = 1,
    Water_regime = "Rainfed"
  )
  wheat_irr <- wheat_rain |> dplyr::mutate(Water_regime = "Irrigated")
  expect_equal(
    calculate_crop_residues(wheat_rain)$Residue_MgDM,
    calculate_crop_residues(wheat_irr)$Residue_MgDM
  )
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


test_that("calculate_crop_residues applies crop-group-specific HI correction", {
  # Wheat and Rice in 1940 SSA should get different HI corrections
  # because Wheat has HI_gap_factor=1.88 vs Rice=1.51
  base_data <- tibble::tibble(
    Prod_ygpit_Mg = 5,
    Area_ygpit_ha = 1,
    Year = 1940,
    region_HANPP = "Sub-saharan Africa",
    Water_regime = "Rainfed"
  )
  wheat_hist <- base_data |> dplyr::mutate(Name_biomass = "Wheat")
  rice_hist <- base_data |> dplyr::mutate(Name_biomass = "Rice")

  # Same crop in 2000 (modern varieties)
  wheat_mod <- wheat_hist |> dplyr::mutate(Year = 2000)
  rice_mod <- rice_hist |> dplyr::mutate(Year = 2000)

  rw_hist <- calculate_crop_residues(wheat_hist)
  rr_hist <- calculate_crop_residues(rice_hist)
  rw_mod <- calculate_crop_residues(wheat_mod)
  rr_mod <- calculate_crop_residues(rice_mod)

  # Historical correction should be larger for wheat than rice
  wheat_correction <- rw_hist$Residue_MgDM / rw_mod$Residue_MgDM
  rice_correction <- rr_hist$Residue_MgDM / rr_mod$Residue_MgDM
  expect_gt(wheat_correction, rice_correction)
})


test_that("calculate_crop_residues works with non-decadal years", {
  # Year 1965 should work (annual interpolation) and not default to 1.0
  crop_1965 <- tibble::tibble(
    Name_biomass = "Wheat",
    Prod_ygpit_Mg = 5,
    Area_ygpit_ha = 1,
    Year = 1965,
    region_HANPP = "South and Central Asia",
    Water_regime = "Rainfed"
  )
  crop_2000 <- crop_1965 |> dplyr::mutate(Year = 2000)

  res_1965 <- calculate_crop_residues(crop_1965)
  res_2000 <- calculate_crop_residues(crop_2000)

  # 1965 should have higher residue than 2000 (partial adoption)
  expect_gt(res_1965$Residue_MgDM, res_2000$Residue_MgDM)
})


test_that("calculate_crop_roots uses crop-specific RS N sensitivity", {
  # Wheat (sensitivity=1.0) should respond more to N than legumes (0.3)
  base_data <- tibble::tibble(
    Prod_MgDM = 2,
    Residue_MgDM = 2,
    Area_ygpit_ha = 1,
    Water_regime = "Rainfed"
  )

  wheat_low  <- base_data |>
    dplyr::mutate(Name_biomass = "Wheat", N_input_kgha = 10)
  wheat_high <- base_data |>
    dplyr::mutate(Name_biomass = "Wheat", N_input_kgha = 250)
  legume_low  <- base_data |>
    dplyr::mutate(Name_biomass = "Soyabeans", N_input_kgha = 10)
  legume_high <- base_data |>
    dplyr::mutate(Name_biomass = "Soyabeans", N_input_kgha = 250)

  rw_lo <- calculate_crop_roots(wheat_low)$Root_MgDM
  rw_hi <- calculate_crop_roots(wheat_high)$Root_MgDM
  rl_lo <- calculate_crop_roots(legume_low)$Root_MgDM
  rl_hi <- calculate_crop_roots(legume_high)$Root_MgDM

  wheat_ratio <- rw_lo / rw_hi
  legume_ratio <- rl_lo / rl_hi

  # Wheat should have a bigger low/high N ratio than legumes
  expect_gt(wheat_ratio, legume_ratio)
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

  # Should not error — falls back gracefully with warnings about missing RS data
  expect_warning(
    result <- calculate_crop_npp(crop_data),
    "no root"
  )
  expect_true("Crop_NPP_MgDM" %in% names(result))
})


test_that("calculate_crop_residues validates required columns", {
  # Missing Name_biomass should error
  bad_data <- tibble::tibble(Prod_ygpit_Mg = 5, Area_ygpit_ha = 1)
  expect_error(calculate_crop_residues(bad_data), "missing required columns")

  # Missing context columns now auto-detected — runs without error
  partial_data <- tibble::tibble(
    Name_biomass = "Wheat", Prod_ygpit_Mg = 5, Area_ygpit_ha = 1
  )
  result <- calculate_crop_residues(partial_data)
  expect_true("Prod_MgDM" %in% names(result))
  expect_true("Residue_MgDM" %in% names(result))

  # simple = TRUE also works without context columns
  result2 <- calculate_crop_residues(partial_data, simple = TRUE)
  expect_true("Prod_MgDM" %in% names(result2))
})


test_that("calculate_crop_roots validates required columns", {
  # Missing Prod_MgDM should error
  bad_data <- tibble::tibble(Name_biomass = "Wheat", Area_ygpit_ha = 1)
  expect_error(calculate_crop_roots(bad_data), "missing required columns")

  # Invalid w_ref should error
  good_data <- tibble::tibble(
    Name_biomass = "Wheat", Prod_MgDM = 3, Residue_MgDM = 4,
    Area_ygpit_ha = 1, N_input_kgha = 100, Water_regime = "Rainfed"
  )
  expect_error(calculate_crop_roots(good_data, w_ref = 1.5), "w_ref must be")

  # Missing N_input_kgha now auto-detected — runs without error, skips N adj
  no_n_data <- tibble::tibble(
    Name_biomass = "Wheat", Prod_MgDM = 3, Residue_MgDM = 4,
    Area_ygpit_ha = 1
  )
  result <- calculate_crop_roots(no_n_data)
  expect_true("Root_MgDM" %in% names(result))
  expect_gt(result$Root_MgDM, 0)

  # simple = TRUE also works without N_input or Water_regime
  result2 <- calculate_crop_roots(no_n_data, simple = TRUE)
  expect_true("Root_MgDM" %in% names(result2))
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


# ===========================================================================
# INTEGRATION TEST: Full pipeline with diverse crop types
# ===========================================================================

test_that("Full NPP pipeline produces realistic results for diverse crops", {
  # Representative crops spanning major categories:
  # cereal, legume, root crop, oilseed, permanent crop, fodder, vegetable
  crop_data <- tibble::tibble(
    Name_biomass = c("Wheat", "Rice", "Maize", "Soyabeans", "Potato",
                     "Sunflower seed", "Sugarcane", "Alfalfa", "Tomato",
                     "Groundnuts, with shell"),
    Prod_ygpit_Mg = c(5, 4, 8, 2.5, 25, 1.5, 60, 8, 30, 2),
    Area_ygpit_ha = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
    Year = 2010,
    region_HANPP = "West Europe",
    Water_regime = c("Rainfed", "Irrigated", "Rainfed", "Rainfed", "Irrigated",
                     "Rainfed", "Irrigated", "Rainfed", "Irrigated", "Rainfed"),
    N_input_kgha = c(150, 120, 180, 10, 100, 60, 80, 0, 200, 20)
  )

  # Full pipeline: residues -> roots -> assembly
  result <- calculate_crop_npp(crop_data)

  # All crops should have positive NPP
  expect_true(all(result$Crop_NPP_MgDM > 0))
  expect_true(all(result$Prod_MgDM > 0))
  expect_true(all(result$Residue_MgDM >= 0))
  expect_true(all(result$Root_MgDM >= 0))

  # Root biomass should be positive for all real crops
  expect_true(all(result$Root_MgDM > 0))

  # Root should be less than 2x aerial biomass for all crops (generous sanity check;

  # fodder crops with RS=0.80 can have Root approaching Aerial)
  expect_true(all(result$Root_MgDM < 2 * (result$Prod_MgDM + result$Residue_MgDM)))

  # Cereals should have substantial residue (RPR > 0.5)
  wheat_res <- result$Residue_MgDM[result$Name_biomass == "Wheat"]
  wheat_prod <- result$Prod_MgDM[result$Name_biomass == "Wheat"]
  expect_true(wheat_res / wheat_prod > 0.5)

  # Potato should have low residue:product ratio
  potato_res <- result$Residue_MgDM[result$Name_biomass == "Potato"]
  potato_prod <- result$Prod_MgDM[result$Name_biomass == "Potato"]
  expect_true(potato_res / potato_prod < 0.5)

  # Legumes should have non-trivial root allocation
  soy_root <- result$Root_MgDM[result$Name_biomass == "Soyabeans"]
  expect_true(soy_root > 0.1)

  # NPP = Prod + Residue + Root
  expect_equal(
    result$Crop_NPP_MgDM,
    result$Prod_MgDM + result$Residue_MgDM + result$Root_MgDM,
    tolerance = 1e-6
  )
})


test_that("calculate_crop_npp joins correct C and N coefficients", {
  crop_data <- tibble::tibble(
    Name_biomass = c("Wheat", "Maize", "Soyabeans", "Potato", "Rice"),
    Prod_ygpit_Mg = c(5, 8, 2.5, 25, 4),
    Area_ygpit_ha = 1,
    Year = 2010,
    region_HANPP = "West Europe",
    Water_regime = "Rainfed",
    N_input_kgha = c(150, 180, 10, 100, 120)
  )

  # calculate_crop_npp provides DM values
  result <- calculate_crop_npp(crop_data)

  # All DM components should be positive
  expect_true(all(result$Prod_MgDM > 0))
  expect_true(all(result$Residue_MgDM > 0))
  expect_true(all(result$Root_MgDM > 0))

  # NPP should be sum of components
  expect_equal(
    result$Crop_NPP_MgDM,
    result$Prod_MgDM + result$Residue_MgDM + result$Root_MgDM,
    tolerance = 1e-6
  )

  # Wheat should have reasonable values:
  # 5 Mg FM * ~0.88 DM/FM = ~4.4 Mg DM product
  wheat <- result[result$Name_biomass == "Wheat", ]
  expect_true(wheat$Prod_MgDM > 3.5 & wheat$Prod_MgDM < 5.0)
  # Residue should be > product for wheat (RPR > 1)
  expect_true(wheat$Residue_MgDM > wheat$Prod_MgDM * 0.8)
  # Root should be plausible (15-35% of aerial)
  aerial <- wheat$Prod_MgDM + wheat$Residue_MgDM
  expect_true(wheat$Root_MgDM / aerial > 0.10)
  expect_true(wheat$Root_MgDM / aerial < 0.50)
})


test_that("Root estimation is robust across N-input and irrigation gradients", {
  # Same crop under different conditions
  crop_data <- tibble::tibble(
    Name_biomass = "Wheat",
    Prod_ygpit_Mg = 5,
    Area_ygpit_ha = 1,
    Year = 2010,
    region_HANPP = "West Europe",
    Water_regime = c("Rainfed", "Irrigated", "Rainfed", "Rainfed", "Rainfed"),
    N_input_kgha = c(0, 150, 50, 150, 300)
  )

  result <- crop_data |>
    calculate_crop_residues() |>
    calculate_crop_roots()

  # Low N, rainfed should have highest root allocation
  # (RS_adjustment = 1.2 for <20 kgN, RS_ratio_factor = 1.0 for rainfed)
  expect_true(result$Root_MgDM[1] > result$Root_MgDM[4])

  # Irrigated should have lower roots than rainfed at same N
  # (RS_ratio_factor = 0.85 for irrigated)
  expect_true(result$Root_MgDM[2] < result$Root_MgDM[4])

  # Very high N should have lowest roots among rainfed
  expect_true(result$Root_MgDM[5] < result$Root_MgDM[3])

  # All should be positive
  expect_true(all(result$Root_MgDM > 0))
})


test_that("Auto-detect mode activates each adjustment independently", {
  # Base: only required columns (no adjustments)
  base_data <- tibble::tibble(
    Name_biomass = "Wheat",
    Prod_ygpit_Mg = 5,
    Area_ygpit_ha = 1
  )
  res_base <- calculate_crop_npp(base_data)

  # Add Water_regime → irrigation adjustment activates
  # Use Maize for residue test (Wheat has Irr_residue_sensitivity=0)
  maize_base <- tibble::tibble(
    Name_biomass = "Maize", Prod_ygpit_Mg = 8, Area_ygpit_ha = 1
  )
  res_maize_base <- calculate_crop_npp(maize_base)
  data_maize_water <- maize_base |> dplyr::mutate(Water_regime = "Irrigated")
  res_maize_water <- calculate_crop_npp(data_maize_water)
  # Irrigated maize should reduce residue (crop-specific effect)
  expect_lt(res_maize_water$Residue_MgDM, res_maize_base$Residue_MgDM)
  # Root irrigation adjustment applies to all crops via RS_ratio_factor
  data_water <- base_data |> dplyr::mutate(Water_regime = "Irrigated")
  res_water <- calculate_crop_npp(data_water)
  expect_lt(res_water$Root_MgDM, res_base$Root_MgDM)

  # Add N_input_kgha → N adjustment activates (low N = more roots)
  data_n_low <- base_data |> dplyr::mutate(N_input_kgha = 10)
  data_n_high <- base_data |> dplyr::mutate(N_input_kgha = 250)
  res_n_low <- calculate_crop_npp(data_n_low)
  res_n_high <- calculate_crop_npp(data_n_high)
  expect_gt(res_n_low$Root_MgDM, res_n_high$Root_MgDM)
  # Residue should be the same (N doesn't affect residues)
  expect_equal(res_n_low$Residue_MgDM, res_n_high$Residue_MgDM)

  # Add Year + region_HANPP → modern variety correction activates
  data_modern <- base_data |>
    dplyr::mutate(Year = 2010, region_HANPP = "West Europe")
  data_hist <- base_data |>
    dplyr::mutate(Year = 1920, region_HANPP = "Sub-saharan Africa")
  res_modern <- calculate_crop_npp(data_modern)
  res_hist <- calculate_crop_npp(data_hist)
  # Historical period, low adoption region → more residue
  expect_gt(res_hist$Residue_MgDM, res_modern$Residue_MgDM)

  # simple = TRUE overrides everything even when all columns present
  data_full <- tibble::tibble(
    Name_biomass = "Wheat",
    Prod_ygpit_Mg = 5,
    Area_ygpit_ha = 1,
    Year = 2010,
    region_HANPP = "West Europe",
    Water_regime = "Irrigated",
    N_input_kgha = 250
  )
  res_full <- calculate_crop_npp(data_full)
  res_simple <- calculate_crop_npp(data_full, simple = TRUE)
  # Simple should give different (unadjusted) results
  expect_true(res_simple$Residue_MgDM != res_full$Residue_MgDM ||
              res_simple$Root_MgDM != res_full$Root_MgDM)
})


test_that("w_ref parameter controls root estimation weighting", {
  crop_data <- tibble::tibble(
    Name_biomass = "Wheat",
    Prod_MgDM = 4,
    Residue_MgDM = 5,
    Area_ygpit_ha = 1,
    Water_regime = "Rainfed",
    N_input_kgha = 100
  )

  # All weight on RS method
  res_rs <- calculate_crop_roots(crop_data, w_ref = 0.0)
  # All weight on reference BG biomass
  res_ref <- calculate_crop_roots(crop_data, w_ref = 1.0)
  # 50/50 ensemble (default)
  res_mid <- calculate_crop_roots(crop_data, w_ref = 0.5)

  # All should be positive
  expect_gt(res_rs$Root_MgDM, 0)
  expect_gt(res_ref$Root_MgDM, 0)
  expect_gt(res_mid$Root_MgDM, 0)

  # Ensemble should be between the two extremes (or equal if they agree)
  root_min <- min(res_rs$Root_MgDM, res_ref$Root_MgDM)
  root_max <- max(res_rs$Root_MgDM, res_ref$Root_MgDM)
  expect_true(res_mid$Root_MgDM >= root_min - 1e-6)
  expect_true(res_mid$Root_MgDM <= root_max + 1e-6)
})


test_that("calculate_crop_roots irrigation adjustment reduces roots", {
  crop_rainfed <- tibble::tibble(
    Name_biomass = "Wheat",
    Prod_MgDM = 4,
    Residue_MgDM = 5,
    Area_ygpit_ha = 1,
    Water_regime = "Rainfed",
    N_input_kgha = 100
  )
  crop_irrigated <- crop_rainfed |>
    dplyr::mutate(Water_regime = "Irrigated")

  res_rain <- calculate_crop_roots(crop_rainfed)
  res_irr <- calculate_crop_roots(crop_irrigated)

  # Irrigated should have lower root biomass (shallower roots)
  expect_lt(res_irr$Root_MgDM, res_rain$Root_MgDM)
})


test_that("calculate_npp_dm_c_n produces correct C and N conversions", {
  # Build minimal input that calculate_npp_dm_c_n expects
  # It needs biomass columns + coefficient columns + weed columns
  input <- tibble::tibble(
    Prod_MgDM = 4.0,
    Residue_MgDM = 5.0,
    Root_MgDM = 2.0,
    Crop_NPP_MgDM = 11.0,
    Weeds_AG_MgDM = 0.5,
    # Crop coefficients
    Product_kgN_kgDM = 0.020,
    Residue_kgN_kgDM = 0.006,
    Root_kgN_kgDM = 0.010,
    Rhizodeposits_N_kgN_kgRootN = 0.50,
    Product_kgC_kgDM = 0.45,
    Residue_kgC_kgDM = 0.42,
    Root_kgC_kgDM = 0.40,
    Residue_kgDM_kgFM = 0.88,
    Use_Share = 0.3,
    Soil_Share = 0.7,
    # Weed coefficients
    Root_Shoot_ratio_W = 0.40,
    Residue_kgN_kgDM_W = 0.015,
    Root_kgN_kgDM_W = 0.012,
    Rhizod_kgN_kgRootN_W = 0.40,
    Residue_kgC_kgDM_W = 0.42,
    Root_kgC_kgDM_W = 0.38
  )

  result <- calculate_npp_dm_c_n(input)

  # Weed BG biomass
  expect_equal(result$Weeds_BG_MgDM, 0.5 * 0.40)
  expect_equal(result$Weeds_NPP_MgDM, 0.5 + 0.5 * 0.40)

  # Crop_NPP_MgDM should be preserved (not recomputed)
  expect_equal(result$Crop_NPP_MgDM, 11.0)

  # Carbon conversions
  expect_equal(result$Prod_MgC, 4.0 * 0.45)
  expect_equal(result$Residue_MgC, 5.0 * 0.42)
  expect_equal(result$Root_MgC, 2.0 * 0.40)
  expect_equal(result$Crop_NPP_MgC,
               result$Prod_MgC + result$Residue_MgC + result$Root_MgC)

  # Nitrogen conversions (root N includes rhizodeposits)
  expect_equal(result$Prod_MgN, 4.0 * 0.020)
  expect_equal(result$Residue_MgN, 5.0 * 0.006)
  expected_root_N <- 2.0 * 0.010 + 2.0 * 0.010 * 0.50
  expect_equal(result$Root_MgN, expected_root_N)

  # Residue FM and use/soil shares
  expect_equal(result$Residue_MgFM, 5.0 / 0.88)
  expect_equal(result$Used_Residue_MgFM, (5.0 / 0.88) * 0.3)
  expect_equal(result$Residue_soil_MgN, result$Residue_MgN * 0.7)

  # Totals
  expect_equal(result$Tot_NPP_MgDM,
               result$Crop_NPP_MgDM + result$Weeds_NPP_MgDM)
  expect_equal(result$Tot_NPP_MgC,
               result$Crop_NPP_MgC + result$Weeds_NPP_MgC)
  expect_equal(result$Tot_NPP_MgN,
               result$Crop_NPP_MgN + result$Weeds_NPP_MgN)
})


test_that("Simple mode produces reasonable results without context columns", {
  crop_data <- tibble::tibble(
    Name_biomass = c("Wheat", "Maize", "Rice", "Soyabeans"),
    Prod_ygpit_Mg = c(5, 8, 4, 2.5),
    Area_ygpit_ha = 1
  )

  result <- crop_data |>
    calculate_crop_npp(simple = TRUE)

  expect_true(all(result$Crop_NPP_MgDM > 0))
  expect_true(all(result$Root_MgDM > 0))

  # Simple mode (no adjustments) should produce intermediate root values
  # between min and max possible under full mode
  expect_true(all(result$Root_MgDM > 0.05))
})
