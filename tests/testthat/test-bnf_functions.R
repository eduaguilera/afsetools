# Tests for BNF functions (bnf_functions.R)
# Covers: calc_crop_bnf, calc_weed_bnf, calc_nonsymbiotic_bnf, calc_bnf
# and internal environmental modifier functions.

# ============================================================================
# Internal helper tests
# ============================================================================

test_that(".bnf_f_n_symbiotic returns correct N inhibition", {
  # At 0 N (both sources): no inhibition
  expect_equal(
    afsetools:::.bnf_f_n_symbiotic(0, 0), 1.0
  )
  # At 200 kg synthetic N/ha with k_synth=0.0035: ~0.497
  f200 <- afsetools:::.bnf_f_n_symbiotic(200, 0, k_synth = 0.0035)
  expect_true(f200 > 0.45 & f200 < 0.55)
  # Monotonically decreasing with synthetic N
  expect_true(
    afsetools:::.bnf_f_n_symbiotic(50, 0) >
    afsetools:::.bnf_f_n_symbiotic(100, 0)
  )
  # Always positive
  expect_true(afsetools:::.bnf_f_n_symbiotic(1000, 0) > 0)
  # Organic N inhibits less than synthetic N at same rate
  f_synth <- afsetools:::.bnf_f_n_symbiotic(100, 0)
  f_org <- afsetools:::.bnf_f_n_symbiotic(0, 100)
  expect_true(f_org > f_synth)
})

test_that(".bnf_f_n_nonsymbiotic has stronger inhibition than symbiotic", {
  f_symb <- afsetools:::.bnf_f_n_symbiotic(100, 0)
  f_ns <- afsetools:::.bnf_f_n_nonsymbiotic(100, 0)
  expect_true(f_ns < f_symb)
})

test_that(".bnf_f_temperature peaks at optimum", {
  f_opt <- afsetools:::.bnf_f_temperature(25, t_opt = 25)
  expect_equal(f_opt, 1.0)

  # Symmetric decline
  f_low <- afsetools:::.bnf_f_temperature(15, t_opt = 25, sigma = 8)
  f_high <- afsetools:::.bnf_f_temperature(35, t_opt = 25, sigma = 8)
  expect_equal(f_low, f_high, tolerance = 1e-10)

  # At 5C, should be substantially reduced
  f_cold <- afsetools:::.bnf_f_temperature(5, t_opt = 25, sigma = 8)
  expect_true(f_cold < 0.5)
})

test_that(".bnf_f_water saturates above humid threshold", {
  # Above AI 0.65 -> factor = 1
  expect_equal(
    afsetools:::.bnf_f_water(1000, 1000), 1.0
  )
  # Dry conditions
  f_dry <- afsetools:::.bnf_f_water(200, 1000)
  expect_true(f_dry < 0.5)
  # Very wet: capped at 1
  expect_equal(
    afsetools:::.bnf_f_water(2000, 1000), 1.0
  )
})

test_that(".bnf_f_som is normalized at reference SOM", {
  # At reference SOM (2.5%), factor should be 1.0
  f_ref <- afsetools:::.bnf_f_som(2.5, k_som = 2.0, som_ref = 2.5)
  expect_equal(f_ref, 1.0, tolerance = 1e-10)

  # Higher SOM -> factor > 1 (enhanced fixation)
  f_high <- afsetools:::.bnf_f_som(5.0, k_som = 2.0, som_ref = 2.5)
  expect_true(f_high > 1.0)

  # Lower SOM -> factor < 1 (reduced fixation)
  f_low <- afsetools:::.bnf_f_som(1.0, k_som = 2.0, som_ref = 2.5)
  expect_true(f_low < 1.0)
})

test_that(".bnf_f_ph peaks near optimal pH", {
  f_opt <- afsetools:::.bnf_f_ph(6.8, ph_opt = 6.8, sigma = 1.5)
  expect_equal(f_opt, 1.0)

  # Very acidic: substantially reduced
  f_acid <- afsetools:::.bnf_f_ph(4.0, ph_opt = 6.8, sigma = 1.5)
  expect_true(f_acid < 0.2)

  # Alkaline: moderately reduced
  f_alk <- afsetools:::.bnf_f_ph(8.5, ph_opt = 6.8, sigma = 1.5)
  expect_true(f_alk > 0.3 & f_alk < 0.7)
})

test_that(".bnf_f_clay is normalized at reference clay", {
  # At reference clay (25%), factor should be 1.0
  f_ref <- afsetools:::.bnf_f_clay(25, k_clay = 20, clay_ref = 25)
  expect_equal(f_ref, 1.0, tolerance = 1e-10)

  # Higher clay -> factor > 1
  f_high <- afsetools:::.bnf_f_clay(50, k_clay = 20, clay_ref = 25)
  expect_true(f_high > 1.0)

  # Sandy soil (low clay) -> factor < 1
  f_low <- afsetools:::.bnf_f_clay(5, k_clay = 20, clay_ref = 25)
  expect_true(f_low < 1.0)
})

test_that(".bnf_validate_input catches bad inputs", {
  # Not a data frame

  expect_error(
    afsetools:::.bnf_validate_input("not_df", "col", "test_fn"),
    "input must be a data frame"
  )
  # Missing columns
  df <- data.frame(a = 1)
  expect_error(
    afsetools:::.bnf_validate_input(df, c("a", "b"), "test_fn"),
    "missing required columns.*b"
  )
  # Zero rows: warning
  df0 <- data.frame(a = numeric(0))
  expect_warning(
    afsetools:::.bnf_validate_input(df0, "a", "test_fn"),
    "zero rows"
  )
  # Valid input: no error
  expect_true(
    afsetools:::.bnf_validate_input(
      data.frame(a = 1, b = 2), c("a", "b"), "test_fn"
    )
  )
})

# ============================================================================
# .bnf_ensure_env_cols tests
# ============================================================================

test_that(".bnf_ensure_env_cols adds missing columns with defaults", {
  df <- data.frame(x = 1:3)
  result <- afsetools:::.bnf_ensure_env_cols(df)

  expect_true("N_synth_kgha" %in% names(result))
  expect_true("N_org_kgha" %in% names(result))
  expect_true("TMP" %in% names(result))
  expect_true("PET_mm" %in% names(result))
  expect_true("WaterInput_mm" %in% names(result))
  expect_true("SOM_pct" %in% names(result))
  expect_true("soil_pH" %in% names(result))
  expect_true("clay_pct" %in% names(result))

  # N defaults to 0
  expect_equal(result$N_synth_kgha, c(0, 0, 0))
  # Climate defaults to NA
  expect_true(all(is.na(result$TMP)))
})

test_that(".bnf_ensure_env_cols computes WaterInput from components", {
  df <- data.frame(precip_mm = 500, irrig_mm = 100)
  result <- afsetools:::.bnf_ensure_env_cols(df)
  expect_equal(result$WaterInput_mm, 600)

  # Without irrigation
  df2 <- data.frame(precip_mm = 500)
  result2 <- afsetools:::.bnf_ensure_env_cols(df2)
  expect_equal(result2$WaterInput_mm, 500)
})

# ============================================================================
# calc_crop_bnf tests
# ============================================================================

test_that("calc_crop_bnf calculates crop BNF correctly", {
  test_data <- tibble::tribble(
    ~Name_biomass, ~Crop_NPP_MgN, ~Prod_MgN,
    "Lentils",     12,            8,
    "Wheat",       150,           100,
    "Soyabeans",   50,            30
  )

  result <- calc_crop_bnf(test_data)

  # Output has required columns
  expect_true(all(c("CropBNF", "CropBNF2", "Ndfa_adj",
                     "f_N_symb", "f_env_symb") %in% names(result)))

  # Legumes have positive CropBNF
  lentil_bnf <- result$CropBNF[result$Name_biomass == "Lentils"]
  expect_true(lentil_bnf > 0)

  # Non-legumes have zero CropBNF
  wheat_bnf <- result$CropBNF[result$Name_biomass == "Wheat"]
  expect_equal(wheat_bnf, 0)

  # Without env columns, f_env_symb = 1 (only N factor, and N=0)
  expect_equal(result$f_N_symb, c(1, 1, 1))
  expect_equal(result$f_temp_symb, c(1, 1, 1))
  expect_equal(result$f_water_symb, c(1, 1, 1))
})

test_that("calc_crop_bnf N inhibition reduces Ndfa", {
  base_data <- tibble::tribble(
    ~Name_biomass, ~Crop_NPP_MgN, ~Prod_MgN,
    "Soyabeans",   50,            30
  )

  # No N input
  result_0 <- calc_crop_bnf(base_data)

  # High N input
  data_highN <- base_data |>
    dplyr::mutate(N_synth_kgha = 200)
  result_200 <- calc_crop_bnf(data_highN)

  # BNF should be lower with high N
  expect_true(result_200$CropBNF < result_0$CropBNF)
  # f_N_symb should be < 1
  expect_true(result_200$f_N_symb < 1)
})

test_that("calc_crop_bnf temperature reduces BNF in cold", {
  base_data <- tibble::tribble(
    ~Name_biomass, ~Crop_NPP_MgN, ~Prod_MgN, ~TMP,
    "Soyabeans",   50,            30,        25,
    "Soyabeans",   50,            30,        5
  )

  result <- calc_crop_bnf(base_data)

  # Warm row should have higher BNF than cold
  expect_true(result$CropBNF[1] > result$CropBNF[2])
  # Cold temperature factor < 1
  expect_true(result$f_temp_symb[2] < 0.5)
})

test_that("calc_crop_bnf water stress reduces BNF", {
  base_data <- tibble::tribble(
    ~Name_biomass, ~Crop_NPP_MgN, ~Prod_MgN,
    ~precip_mm, ~PET_mm,
    "Soyabeans", 50, 30, 800, 800,
    "Soyabeans", 50, 30, 200, 800
  )

  result <- calc_crop_bnf(base_data)

  # Wet row should have higher BNF
  expect_true(result$CropBNF[1] > result$CropBNF[2])
  # Dry water factor < 1
  expect_true(result$f_water_symb[2] < 0.5)
})

# ============================================================================
# calc_weed_bnf tests
# ============================================================================

test_that("calc_weed_bnf calculates weed BNF correctly", {
  test_data <- tibble::tribble(
    ~Weeds_NPP_MgN, ~LandUse,   ~Legs_Seeded, ~Seeded_CC_share,
    1.0,            "Cropland",  0.5,          0.2,
    0.5,            "Grassland", 0.0,          0.0
  )

  result <- calc_weed_bnf(test_data)

  expect_true("WeedsBNF" %in% names(result))
  expect_true("Weeds_leg_share" %in% names(result))

  # BNF is non-negative
  expect_true(all(result$WeedsBNF >= 0))

  # Grassland has no seeded CC, uses spontaneous only
  expect_equal(result$Seeded_CC_share[2], 0)
})

test_that("calc_weed_bnf handles NA Weeds_NPP_MgN", {
  test_data <- tibble::tribble(
    ~Weeds_NPP_MgN, ~LandUse,   ~Legs_Seeded, ~Seeded_CC_share,
    NA_real_,       "Cropland",  0.3,          0.1
  )

  result <- calc_weed_bnf(test_data)
  expect_equal(result$WeedsBNF, 0)
})

# ============================================================================
# calc_nonsymbiotic_bnf tests
# ============================================================================

test_that("calc_nonsymbiotic_bnf uses default base rate", {
  test_data <- tibble::tribble(
    ~Area_ygpit_ha,
    1000,
    500
  )

  result <- calc_nonsymbiotic_bnf(test_data)

  expect_true("NSBNF" %in% names(result))
  expect_true("NSBNF_base_kgha" %in% names(result))

  # With no env adjustments: NSBNF = 5 * 1 * Area / 1000
  expect_equal(result$NSBNF_base_kgha, c(5, 5))
  expect_equal(result$NSBNF, c(5 * 1000 / 1000, 5 * 500 / 1000))
})

test_that("calc_nonsymbiotic_bnf uses kgNha from BNF table", {
  test_data <- tibble::tribble(
    ~Name_biomass, ~Area_ygpit_ha,
    "Rice",        1000,
    "Wheat",       1000
  )

  result <- calc_nonsymbiotic_bnf(test_data)

  # Rice should use 33 from BNF table
  rice_base <- result$NSBNF_base_kgha[
    result$Name_biomass == "Rice"
  ]
  expect_equal(rice_base, 33)

  # Wheat uses default (not in BNF kgNha)
  wheat_base <- result$NSBNF_base_kgha[
    result$Name_biomass == "Wheat"
  ]
  expect_equal(wheat_base, 5)
})

test_that("calc_nonsymbiotic_bnf N inhibition reduces NSBNF", {
  test_data <- tibble::tribble(
    ~Area_ygpit_ha, ~N_synth_kgha,
    1000,           0,
    1000,           200
  )

  result <- calc_nonsymbiotic_bnf(test_data)

  # Higher N -> lower NSBNF
  expect_true(result$NSBNF[1] > result$NSBNF[2])
  # f_N_ns for high N < 1
  expect_true(result$f_N_ns[2] < 0.5)
})

test_that("calc_nonsymbiotic_bnf SOM enhances fixation", {
  test_data <- tibble::tribble(
    ~Area_ygpit_ha, ~SOM_pct,
    1000,           1.0,
    1000,           5.0
  )

  result <- calc_nonsymbiotic_bnf(test_data)

  # Higher SOM -> higher NSBNF
  expect_true(result$NSBNF[2] > result$NSBNF[1])
  # SOM factor at 5% > 1 (above reference)
  expect_true(result$f_SOM_ns[2] > 1.0)
  # SOM factor at 1% < 1 (below reference)
  expect_true(result$f_SOM_ns[1] < 1.0)
})

test_that("calc_nonsymbiotic_bnf pH affects fixation", {
  test_data <- tibble::tribble(
    ~Area_ygpit_ha, ~soil_pH,
    1000,           6.8,
    1000,           4.0
  )

  result <- calc_nonsymbiotic_bnf(test_data)

  # Optimal pH -> more NSBNF than very acidic
  expect_true(result$NSBNF[1] > result$NSBNF[2])
  # Acidic pH factor should be very low
  expect_true(result$f_pH_ns[2] < 0.2)
})

test_that("calc_nonsymbiotic_bnf full env stack works", {
  test_data <- tibble::tribble(
    ~Area_ygpit_ha, ~N_synth_kgha, ~N_org_kgha,
    ~TMP, ~precip_mm, ~irrig_mm, ~PET_mm,
    ~SOM_pct, ~soil_pH,
    1000, 50, 20, 22, 300, 0, 900, 3.0, 6.5
  )

  result <- calc_nonsymbiotic_bnf(test_data)

  # All factors should be computed (not 1)
  expect_true(result$f_N_ns != 1)
  expect_true(result$f_temp_ns != 1)
  expect_true(result$f_water_ns != 1)
  expect_true(result$f_SOM_ns != 1)
  expect_true(result$f_pH_ns != 1)

  # NSBNF should be positive
  expect_true(result$NSBNF > 0)
})

# ============================================================================
# calc_bnf master function tests
# ============================================================================

test_that("calc_bnf produces all expected output columns", {
  test_data <- tibble::tribble(
    ~Year, ~Name_biomass, ~LandUse,   ~Area_ygpit_ha,
    ~Crop_NPP_MgN, ~Prod_MgN, ~Weeds_NPP_MgN,
    ~Legs_Seeded, ~Seeded_CC_share,
    2020, "Lentils", "Cropland", 800,
    12, 8, 0.8, 0.5, 0.2,
    2020, "Wheat", "Cropland", 10000,
    150, 100, 5.0, 0.3, 0.1
  )

  result <- calc_bnf(test_data)

  expected_cols <- c(
    "Fert_type", "CropBNF", "CropBNF2", "WeedsBNF", "NSBNF",
    "BNF", "f_N_symb", "f_temp_symb", "f_water_symb",
    "f_env_symb", "Ndfa_adj", "f_N_ns", "f_temp_ns",
    "f_water_ns", "f_SOM_ns", "f_pH_ns", "f_clay_ns",
    "f_env_ns", "Alpha1", "Alpha2", "NSBNF_base_kgha"
  )
  for (col in expected_cols) {
    expect_true(col %in% names(result),
                info = paste("Missing column:", col))
  }
})

test_that("calc_bnf total BNF is sum of components", {
  test_data <- tibble::tribble(
    ~Year, ~Name_biomass, ~LandUse,   ~Area_ygpit_ha,
    ~Crop_NPP_MgN, ~Prod_MgN, ~Weeds_NPP_MgN,
    ~Legs_Seeded, ~Seeded_CC_share,
    2020, "Lentils", "Cropland", 800,
    12, 8, 0.8, 0.5, 0.2,
    2020, "Wheat", "Cropland", 10000,
    150, 100, 5.0, 0.3, 0.1
  )

  result <- calc_bnf(test_data)

  expect_equal(
    result$BNF,
    result$CropBNF + result$WeedsBNF + result$NSBNF
  )
})

test_that("calc_bnf legumes have higher total BNF than cereals", {
  test_data <- tibble::tribble(
    ~Year, ~Name_biomass, ~LandUse,   ~Area_ygpit_ha,
    ~Crop_NPP_MgN, ~Prod_MgN, ~Weeds_NPP_MgN,
    ~Legs_Seeded, ~Seeded_CC_share,
    2020, "Lentils",  "Cropland", 1000,
    12, 8, 1.0, 0.5, 0.2,
    2020, "Wheat",    "Cropland", 1000,
    12, 8, 1.0, 0.3, 0.1
  )

  result <- calc_bnf(test_data)

  lentil_bnf <- result$BNF[result$Name_biomass == "Lentils"]
  wheat_bnf <- result$BNF[result$Name_biomass == "Wheat"]
  expect_true(lentil_bnf > wheat_bnf)
})

test_that("calc_bnf BNF values are non-negative", {
  test_data <- tibble::tribble(
    ~Year, ~Name_biomass, ~LandUse,  ~Area_ygpit_ha,
    ~Crop_NPP_MgN, ~Prod_MgN, ~Weeds_NPP_MgN,
    ~Legs_Seeded, ~Seeded_CC_share,
    2020, "Soyabeans", "Cropland", 5000,
    50, 30, 2.0, 0.3, 0.1,
    2020, "Rice", "Cropland", 3000,
    60, 40, 1.5, 0.0, 0.0
  )

  result <- calc_bnf(test_data)

  expect_true(all(result$CropBNF >= 0))
  expect_true(all(result$WeedsBNF >= 0))
  expect_true(all(result$NSBNF >= 0))
  expect_true(all(result$BNF >= 0))
})

test_that("calc_bnf with env data produces adjusted BNF", {
  base_data <- tibble::tribble(
    ~Year, ~Name_biomass, ~LandUse,   ~Area_ygpit_ha,
    ~Crop_NPP_MgN, ~Prod_MgN, ~Weeds_NPP_MgN,
    ~Legs_Seeded, ~Seeded_CC_share,
    2020, "Soyabeans", "Cropland", 5000,
    50, 30, 2.0, 0.3, 0.1
  )

  # Without env data
  result_base <- calc_bnf(base_data)

  # With high N, cold, dry conditions
  result_stressed <- calc_bnf(
    base_data |>
      dplyr::mutate(
        N_synth_kgha = 200, TMP = 8,
        precip_mm = 200, PET_mm = 900
      )
  )

  # Stressed conditions -> lower BNF
  expect_true(result_stressed$BNF < result_base$BNF)
  expect_true(result_stressed$CropBNF < result_base$CropBNF)
  expect_true(result_stressed$NSBNF < result_base$NSBNF)
})

test_that("calc_bnf Fert_type is BNF", {
  test_data <- tibble::tribble(
    ~Year, ~Name_biomass, ~LandUse,   ~Area_ygpit_ha,
    ~Crop_NPP_MgN, ~Prod_MgN, ~Weeds_NPP_MgN,
    ~Legs_Seeded, ~Seeded_CC_share,
    2020, "Wheat", "Cropland", 1000,
    10, 5, 0.5, 0.0, 0.0
  )

  result <- calc_bnf(test_data)
  expect_equal(unique(result$Fert_type), "BNF")
})

test_that("calc_bnf rice has high NSBNF base", {
  test_data <- tibble::tribble(
    ~Year, ~Name_biomass, ~LandUse,   ~Area_ygpit_ha,
    ~Crop_NPP_MgN, ~Prod_MgN, ~Weeds_NPP_MgN,
    ~Legs_Seeded, ~Seeded_CC_share,
    2020, "Rice", "Cropland", 1000,
    20, 12, 1.0, 0.0, 0.0
  )

  result <- calc_bnf(test_data)

  # Rice NSBNF base = 33 from BNF table
  expect_equal(result$NSBNF_base_kgha, 33)
  # Rice NSBNF = 33 * 1000 / 1000 = 33 MgN (with f_env=1)
  expect_equal(result$NSBNF, 33 * 1000 / 1000)
})

# ============================================================================
# Backward compatibility: Calc_N_fix wrapper
# ============================================================================

test_that("Calc_N_fix wrapper calls calc_bnf successfully", {
  test_data <- tibble::tribble(
    ~Year, ~Name_biomass, ~LandUse,   ~Area_ygpit_ha,
    ~Crop_NPP_MgN, ~Prod_MgN, ~Weeds_NPP_MgN,
    ~Legs_Seeded, ~Seeded_CC_share,
    2020, "Lentils", "Cropland", 800,
    12, 8, 0.8, 0.5, 0.2,
    2020, "Wheat", "Cropland", 10000,
    150, 100, 5.0, 0.3, 0.1
  )

  result <- Calc_N_fix(test_data)

  # Same output columns as calc_bnf
  expect_true("CropBNF" %in% names(result))
  expect_true("WeedsBNF" %in% names(result))
  expect_true("NSBNF" %in% names(result))
  expect_true("BNF" %in% names(result))
  expect_true("Fert_type" %in% names(result))

  # Values are realistic
  expect_true(all(result$BNF >= 0))
  expect_equal(result$BNF,
               result$CropBNF + result$WeedsBNF + result$NSBNF)
})

# ============================================================================
# calc_nonsymbiotic_bnf clay modifier tests
# ============================================================================

test_that("calc_nonsymbiotic_bnf clay enhances NSBNF", {
  test_data <- tibble::tribble(
    ~Area_ygpit_ha, ~clay_pct,
    1000,           5,
    1000,           50
  )

  result <- calc_nonsymbiotic_bnf(test_data)

  # Higher clay -> higher NSBNF
  expect_true(result$NSBNF[2] > result$NSBNF[1])
  # Clay factor at 50% > 1 (above reference 25%)
  expect_true(result$f_clay_ns[2] > 1.0)
  # Clay factor at 5% < 1 (below reference)
  expect_true(result$f_clay_ns[1] < 1.0)
})

# ============================================================================
# summarize_bnf tests
# ============================================================================

test_that("summarize_bnf produces correct summary", {
  test_data <- tibble::tribble(
    ~Year, ~Name_biomass, ~LandUse,   ~Area_ygpit_ha,
    ~Crop_NPP_MgN, ~Prod_MgN, ~Weeds_NPP_MgN,
    ~Legs_Seeded, ~Seeded_CC_share,
    2020, "Lentils", "Cropland", 800,
    12, 8, 0.8, 0.5, 0.2,
    2020, "Wheat", "Cropland", 10000,
    150, 100, 5.0, 0.3, 0.1
  )

  bnf_result <- calc_bnf(test_data)
  summary <- summarize_bnf(bnf_result)

  expect_true("total_BNF_MgN" %in% names(summary))
  expect_true("pct_CropBNF" %in% names(summary))
  expect_true("pct_NSBNF" %in% names(summary))
  expect_true("Cat_leg" %in% names(summary))
  expect_equal(nrow(summary), 2)  # Grouped by Name_biomass

  # Lentils should be classified as Grain legume
  lentils_row <- summary[summary$Name_biomass == "Lentils", ]
  expect_equal(lentils_row$Cat_leg, "Grain")

  # Wheat should have NA Cat_leg (not a legume)
  wheat_row <- summary[summary$Name_biomass == "Wheat", ]
  expect_true(is.na(wheat_row$Cat_leg))

  # Percentages should sum to ~100 per group
  for (i in seq_len(nrow(summary))) {
    pct_sum <- sum(
      summary$pct_CropBNF[i],
      summary$pct_WeedsBNF[i],
      summary$pct_NSBNF[i],
      na.rm = TRUE
    )
    if (!is.na(pct_sum)) {
      expect_equal(pct_sum, 100, tolerance = 0.01)
    }
  }
})

test_that("summarize_bnf with NULL group gives overall", {
  test_data <- tibble::tribble(
    ~Year, ~Name_biomass, ~LandUse,   ~Area_ygpit_ha,
    ~Crop_NPP_MgN, ~Prod_MgN, ~Weeds_NPP_MgN,
    ~Legs_Seeded, ~Seeded_CC_share,
    2020, "Lentils", "Cropland", 800,
    12, 8, 0.8, 0.5, 0.2
  )

  bnf_result <- calc_bnf(test_data)
  summary <- summarize_bnf(bnf_result, group_by = NULL)

  expect_equal(nrow(summary), 1)
})

test_that("summarize_bnf errors without BNF columns", {
  df <- data.frame(x = 1:3)
  expect_error(
    summarize_bnf(df),
    "missing BNF result columns"
  )
})

# ============================================================================
# Data integrity: BNF tables join validation
# ============================================================================

test_that("All Name_BNF in Names_BNF exist in BNF table", {
  bnf_categories <- unique(BNF$Name_BNF)
  mapping_categories <- unique(Names_BNF$Name_BNF)
  missing <- setdiff(mapping_categories, bnf_categories)
  expect_equal(
    length(missing), 0,
    info = paste("Names_BNF references missing BNF rows:",
                 paste(missing, collapse = ", "))
  )
})

test_that("All Pure_legs Name_BNF exist in BNF table", {
  bnf_categories <- unique(BNF$Name_BNF)
  pl_categories <- unique(Pure_legs$Name_BNF)
  missing <- setdiff(pl_categories, bnf_categories)
  expect_equal(
    length(missing), 0,
    info = paste("Pure_legs references missing BNF rows:",
                 paste(missing, collapse = ", "))
  )
})

test_that("Names_BNF has expected dimensions", {
  expect_true(nrow(Names_BNF) >= 38)
  expect_true(all(c("Name_biomass", "Name_BNF") %in% names(Names_BNF)))
})

test_that("BNF table has expected structure", {
  expect_equal(nrow(BNF), 17)
  required_cols <- c("Name_BNF", "Ndfa", "NHI", "BGN", "kgNha",
                     "Leguminous_share", "Source")
  expect_true(all(required_cols %in% names(BNF)))
})

test_that("BNF Ndfa values are in valid range", {
  valid_ndfa <- BNF$Ndfa[!is.na(BNF$Ndfa)]
  expect_true(all(valid_ndfa >= 0 & valid_ndfa <= 1))
})

test_that("BNF Leguminous_share values are in valid range", {
  ls_vals <- BNF$Leguminous_share[!is.na(BNF$Leguminous_share)]
  expect_true(all(ls_vals >= 0 & ls_vals <= 1))
})

test_that("BNF kgNha values are positive where set", {
  kgn <- BNF$kgNha[!is.na(BNF$kgNha)]
  expect_true(all(kgn > 0))
  # Rice = 33, Sugarcane = 25
  expect_equal(BNF$kgNha[BNF$Name_BNF == "Rice"], 33)
  expect_equal(BNF$kgNha[BNF$Name_BNF == "Sugarcane"], 25)
})

test_that("Fodder types have NHI/BGN filled", {
  # Fodder, other, Mixed swards, and Meadows should now have NHI/BGN
  fodder_rows <- BNF[BNF$Name_BNF %in%
    c("Fodder, other", "Mixed swards", "Meadows"), ]
  expect_true(all(!is.na(fodder_rows$NHI)))
  expect_true(all(!is.na(fodder_rows$BGN)))

  # Fallow and Weeds should remain NA (not production systems)
  non_prod <- BNF[BNF$Name_BNF %in% c("Fallow", "Weeds"), ]
  expect_true(all(is.na(non_prod$NHI)))
  expect_true(all(is.na(non_prod$BGN)))
})

test_that("New leguminous crops are mapped in Names_BNF", {
  new_crops <- c(
    "Bard vetch/Oneflower vetch, green",
    "Bitter vetch, green",
    "Cereal-legume mixture",
    "Common sainfoin",
    "Fenugreek, green",
    "Subterranean clover",
    "Tree medick",
    "Legume brans",
    "Other monospecific swards"
  )
  for (crop in new_crops) {
    expect_true(
      crop %in% Names_BNF$Name_biomass,
      info = paste("Missing mapping for:", crop)
    )
  }
})

test_that("Pure_legs covers only pure legumes", {
  # All Pure_legs categories should have Leguminous_share = 1
  for (i in seq_len(nrow(Pure_legs))) {
    bnf_row <- BNF[BNF$Name_BNF == Pure_legs$Name_BNF[i], ]
    expect_equal(
      bnf_row$Leguminous_share, 1,
      info = paste(Pure_legs$Name_BNF[i], "should have share=1")
    )
  }
})