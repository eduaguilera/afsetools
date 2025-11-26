library(testthat)

test_that("integrate_fallow distributes fallow area to herbaceous crops", {
  # create minimal supporting objects in global env (mimics load_general_data behavior)
  Names_biomass <- tibble::tibble(
    Name_biomass = c("CropA", "Fallow"),
    Name = c("CropA_name", "Fallow_name"),
    Herb_Woody = c("Herbaceous", "Herbaceous")
  )
  Names_cats <- tibble::tibble(
    Name = c("CropA_name", "Fallow_name"),
    Cat_1 = c("Cereals", "Other")
  )

  assign("Names_biomass", Names_biomass, envir = .GlobalEnv)
  assign("Names_cats", Names_cats, envir = .GlobalEnv)

  df <- tibble::tibble(
    Year = c(2000, 2000),
    Name_biomass = c("CropA", "Fallow"),
    LandUse = c("Cropland", "Cropland"),
    Area_ygpit_ha = c(10, 5)
  )

  out <- integrate_fallow(df, .by = "Year")

  # CropA should receive all fallow (it is the only herbaceous crop)
  cropA_area <- out$Area_ygpit_ha[out$Name_biomass == "CropA"]
  expect_equal(cropA_area, 15)

  # cleanup
  rm(Names_biomass, Names_cats, envir = .GlobalEnv)
})
