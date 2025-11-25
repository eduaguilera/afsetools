#' Load Color Palettes, Factor Levels, and Categorical Vectors
#'
#' @description
#' Creates 300+ vector objects in the calling environment for data visualization
#' and analysis, including:
#' - Month-related vectors and dataframes
#' - Color palettes for different categories
#' - Factor level orderings
#' - Named color vectors for ggplot2 scale functions
#' - Categorical dataframes for NPP and C input categorization
#'
#' This function is called internally by `load_general_data()` when
#' `load_vectors = TRUE`.
#'
#' @param env Environment to load vectors into. Defaults to parent.frame().
#'
#' @return NULL (loads objects into specified environment)
#'
#' @details
#' All vectors are created using assign() to ensure they are available in the
#' specified environment without needing to source external files.
#'
#' @examples
#' \dontrun{
#' # Load vectors into current environment
#' load_vectors()
#'
#' # Check what was loaded
#' ls()
#' }
#'
#' @export
load_vectors <- function(env = parent.frame()) {
  
  # Dates ----
  assign("Month_names", c(
    "January", "February", "March", "April", "May", "June",
    "July", "August", "September", "October", "November", "December"
  ), envir = env)
  
  Month_names <- get("Month_names", envir = env)
  assign("Month_order", Month_names, envir = env)
  
  assign("Month_integer", c(0:11), envir = env)
  assign("Month_number", c(1:12), envir = env)
  
  Month_integer <- get("Month_integer", envir = env)
  Month_number <- get("Month_number", envir = env)
  
  Month_numbers <- data.frame(Month_names, Month_integer, Month_number) |>
    dplyr::mutate(
      Month = Month_integer / 12,
      Month_number = as.numeric(Month_number)
    ) |>
    dplyr::select(-Month_integer)
  assign("Month_numbers", Month_numbers, envir = env)
  
  # General colours ----
  assign("Total_color", "black", envir = env)
  assign("SOM_color", "peachpuff3", envir = env)
  
  Total_color <- get("Total_color", envir = env)
  SOM_color <- get("SOM_color", envir = env)
  
  assign("Total_colour", c("Total" = Total_color), envir = env)
  assign("SOM_colour", c("SOM" = SOM_color), envir = env)
  assign("SON_colour", c("SON" = SOM_color), envir = env)
  assign("SOC_colour", c("SOC" = SOM_color), envir = env)
  
  # Irrig and Irrig_cat levels and colours ----
  Temperate_Irrigated_color <- "cadetblue"
  Temperate_Rainfed_color <- "darkgoldenrod"
  Mediterranean_Irrigated_color <- "cadetblue2"
  Mediterranean_Rainfed_color <- "darkgoldenrod1"
  Greenhouse_color <- "plum4"
  Irrigated_color <- "skyblue2"
  Rainfed_color <- "sandybrown"
  
  assign("Temperate_Irrigated_color", Temperate_Irrigated_color, envir = env)
  assign("Temperate_Rainfed_color", Temperate_Rainfed_color, envir = env)
  assign("Mediterranean_Irrigated_color", Mediterranean_Irrigated_color, envir = env)
  assign("Mediterranean_Rainfed_color", Mediterranean_Rainfed_color, envir = env)
  assign("Greenhouse_color", Greenhouse_color, envir = env)
  assign("Irrigated_color", Irrigated_color, envir = env)
  assign("Rainfed_color", Rainfed_color, envir = env)
  
  assign("Temperate_Irrigated_colour", c("Temperate Irrigated" = Temperate_Irrigated_color), envir = env)
  assign("Temperate_Rainfed_colour", c("Temperate Rainfed" = Temperate_Rainfed_color), envir = env)
  assign("Mediterranean_Irrigated_colour", c("Mediterranean Irrigated" = Mediterranean_Irrigated_color), envir = env)
  assign("Mediterranean_Rainfed_colour", c("Mediterranean Rainfed" = Mediterranean_Rainfed_color), envir = env)
  assign("Greenhouse_colour", c("Greenhouse" = Greenhouse_color), envir = env)
  assign("Irrigated_colour", c("Irrigated" = Irrigated_color), envir = env)
  assign("Rainfed_colour", c("Rainfed" = Rainfed_color), envir = env)
  
  Temperate_Irrigated_colour <- get("Temperate_Irrigated_colour", envir = env)
  Temperate_Rainfed_colour <- get("Temperate_Rainfed_colour", envir = env)
  Mediterranean_Irrigated_colour <- get("Mediterranean_Irrigated_colour", envir = env)
  Mediterranean_Rainfed_colour <- get("Mediterranean_Rainfed_colour", envir = env)
  Greenhouse_colour <- get("Greenhouse_colour", envir = env)
  Irrigated_colour <- get("Irrigated_colour", envir = env)
  Rainfed_colour <- get("Rainfed_colour", envir = env)
  Total_colour <- get("Total_colour", envir = env)
  
  assign("Irrig_Climate_levels", c("Temperate Irrigated", "Temperate Rainfed", "Mediterranean Irrigated", "Mediterranean Rainfed"), envir = env)
  assign("Irrig_Climate_colours", c(Temperate_Irrigated_colour, Temperate_Rainfed_colour, Mediterranean_Irrigated_colour, Mediterranean_Rainfed_colour), envir = env)
  
  Irrig_Climate_levels <- get("Irrig_Climate_levels", envir = env)
  Irrig_Climate_colours <- get("Irrig_Climate_colours", envir = env)
  
  assign("Irrig_Climate_levels_all", c(Irrig_Climate_levels, "Total Spain"), envir = env)
  assign("Irrig_Climate_colours_all", c(Irrig_Climate_colours, Total_colour), envir = env)
  
  assign("Irrig_levels", c("Irrigated", "Rainfed"), envir = env)
  assign("Irrig_colours_simple", c(Irrigated_colour, Rainfed_colour), envir = env)
  
  Irrig_levels <- get("Irrig_levels", envir = env)
  Irrig_colours_simple <- get("Irrig_colours_simple", envir = env)
  
  assign("Irrig_tot_levels", c("Total", Irrig_levels), envir = env)
  assign("Irrig_tot_colours", c(Total_colour, Irrig_colours_simple), envir = env)
  
  assign("Irrig_cat_levels", c("Greenhouse", "Irrigated", "Rainfed"), envir = env)
  assign("Irrig_colours", c(Greenhouse_colour, Irrig_colours_simple), envir = env)
  
  # NPP levels and colours ----
  Crop_color <- "darkgoldenrod3"
  Crop_prod_color <- "darkorange4"
  Crop_residue_color <- "darkgoldenrod1"
  Crop_BG_color <- "darkgoldenrod3"
  Crop_RD_color <- "lightgoldenrod"
  Shade_color <- "olivedrab3"
  Shade_products_color <- Shade_color
  Shade_fruit_color <- "green4"
  Shade_roundwood_color <- "green3"
  Shade_firewood_color <- "green1"
  Shade_AG_color <- "darkolivegreen"
  Shade_BG_color <- "olivedrab3"
  Shade_RD_color <- "olivedrab1"
  Weeds_color <- "skyblue3"
  Weeds_AG_color <- "skyblue4"
  Weeds_BG_color <- "skyblue3"
  Weeds_RD_color <- "skyblue1"
  
  assign("Crop_color", Crop_color, envir = env)
  assign("Crop_prod_color", Crop_prod_color, envir = env)
  assign("Crop_residue_color", Crop_residue_color, envir = env)
  assign("Crop_BG_color", Crop_BG_color, envir = env)
  assign("Crop_RD_color", Crop_RD_color, envir = env)
  assign("Shade_color", Shade_color, envir = env)
  assign("Shade_products_color", Shade_products_color, envir = env)
  assign("Shade_fruit_color", Shade_fruit_color, envir = env)
  assign("Shade_roundwood_color", Shade_roundwood_color, envir = env)
  assign("Shade_firewood_color", Shade_firewood_color, envir = env)
  assign("Shade_AG_color", Shade_AG_color, envir = env)
  assign("Shade_BG_color", Shade_BG_color, envir = env)
  assign("Shade_RD_color", Shade_RD_color, envir = env)
  assign("Weeds_color", Weeds_color, envir = env)
  assign("Weeds_AG_color", Weeds_AG_color, envir = env)
  assign("Weeds_BG_color", Weeds_BG_color, envir = env)
  assign("Weeds_RD_color", Weeds_RD_color, envir = env)
  
  assign("Crop_colour", c("Crop" = Crop_color), envir = env)
  assign("Crop_Biomass_colour", c("Crop Biomass" = Crop_color), envir = env)
  assign("Crop_prod_colour", c("Crop product" = Crop_prod_color), envir = env)
  assign("Crop_residue_colour", c("Crop residue" = Crop_residue_color), envir = env)
  assign("Crop_BG_colour", c("Crop BG" = Crop_BG_color), envir = env)
  assign("Crop_RD_colour", c("Crop RD" = Crop_RD_color), envir = env)
  assign("Shade_colour", c("Shade" = Shade_color), envir = env)
  assign("Shade_Biomass_colour", c("Shade Biomass" = Shade_color), envir = env)
  assign("Shade_products_colour", c("Shade products" = Shade_products_color), envir = env)
  assign("Shade_fruit_colour", c("Shade fruit" = Shade_fruit_color), envir = env)
  assign("Shade_roundwood_colour", c("Shade roundwood" = Shade_roundwood_color), envir = env)
  assign("Shade_firewood_colour", c("Shade firewood" = Shade_firewood_color), envir = env)
  assign("Shade_AG_colour", c("Shade AG" = Shade_AG_color), envir = env)
  assign("Shade_BG_colour", c("Shade BG" = Shade_BG_color), envir = env)
  assign("Shade_RD_colour", c("Shade RD" = Shade_RD_color), envir = env)
  assign("Weeds_colour", c("Weeds" = Weeds_color), envir = env)
  assign("Weeds_AG_colour", c("Weeds AG" = Weeds_AG_color), envir = env)
  assign("Weeds_BG_colour", c("Weeds BG" = Weeds_BG_color), envir = env)
  assign("Weeds_RD_colour", c("Weeds RD" = Weeds_RD_color), envir = env)
  
  Crop_colour <- get("Crop_colour", envir = env)
  Crop_Biomass_colour <- get("Crop_Biomass_colour", envir = env)
  Crop_prod_colour <- get("Crop_prod_colour", envir = env)
  Crop_residue_colour <- get("Crop_residue_colour", envir = env)
  Crop_BG_colour <- get("Crop_BG_colour", envir = env)
  Crop_RD_colour <- get("Crop_RD_colour", envir = env)
  Shade_colour <- get("Shade_colour", envir = env)
  Shade_Biomass_colour <- get("Shade_Biomass_colour", envir = env)
  Shade_products_colour <- get("Shade_products_colour", envir = env)
  Shade_fruit_colour <- get("Shade_fruit_colour", envir = env)
  Shade_roundwood_colour <- get("Shade_roundwood_colour", envir = env)
  Shade_firewood_colour <- get("Shade_firewood_colour", envir = env)
  Shade_AG_colour <- get("Shade_AG_colour", envir = env)
  Shade_BG_colour <- get("Shade_BG_colour", envir = env)
  Shade_RD_colour <- get("Shade_RD_colour", envir = env)
  Weeds_colour <- get("Weeds_colour", envir = env)
  Weeds_AG_colour <- get("Weeds_AG_colour", envir = env)
  Weeds_BG_colour <- get("Weeds_BG_colour", envir = env)
  Weeds_RD_colour <- get("Weeds_RD_colour", envir = env)
  SOC_colour <- get("SOC_colour", envir = env)
  
  assign("Harvest_levels", c("Crop product", "Crop residue", "Shade fruit", "Shade roundwood", "Shade firewood"), envir = env)
  assign("Harvest_colours", c(Crop_prod_colour, Crop_residue_colour, Shade_fruit_colour, Shade_roundwood_colour, Shade_firewood_colour), envir = env)
  assign("Recycled_levels", c("Crop residue", "Crop BG", "Crop RD", "Shade AG", "Shade BG", "Shade RD", "Weeds AG", "Weeds BG", "Weeds RD"), envir = env)
  assign("Recycled_colours", c(Crop_residue_colour, Crop_BG_colour, Crop_RD_colour, Shade_AG_colour, Shade_BG_colour, Shade_RD_colour, Weeds_AG_colour, Weeds_BG_colour, Weeds_RD_colour), envir = env)
  assign("PlantType_levels", c("Crop", "Shade", "Weeds"), envir = env)
  assign("PlantType_colours", c(Crop_colour, Shade_colour, Weeds_colour), envir = env)
  assign("Stock_levels", c("Crop Biomass", "Shade Biomass", "SOC"), envir = env)
  assign("Stock_colours", c(Crop_Biomass_colour, Shade_Biomass_colour, SOC_colour), envir = env)
  
  # NPP component levels and colours ----
  Harvest_color <- "purple4"
  Recycled_AG_color <- "orchid4"
  Recycled_BG_color <- "orchid"
  Recycled_RD_color <- "orchid1"
  Accumulated_color <- "burlywood4"
  
  assign("Harvest_color", Harvest_color, envir = env)
  assign("Recycled_AG_color", Recycled_AG_color, envir = env)
  assign("Recycled_BG_color", Recycled_BG_color, envir = env)
  assign("Recycled_RD_color", Recycled_RD_color, envir = env)
  assign("Accumulated_color", Accumulated_color, envir = env)
  
  assign("Harvest_colour", c("Harvest" = Harvest_color), envir = env)
  assign("Recycled_AG_colour", c("Recycled AG" = Recycled_AG_color), envir = env)
  assign("Recycled_BG_colour", c("Recycled BG" = Recycled_BG_color), envir = env)
  assign("Recycled_RD_colour", c("Recycled RD" = Recycled_RD_color), envir = env)
  assign("Accumulated_colour", c("Accumulated" = Accumulated_color), envir = env)
  
  Harvest_colour <- get("Harvest_colour", envir = env)
  Recycled_AG_colour <- get("Recycled_AG_colour", envir = env)
  Recycled_BG_colour <- get("Recycled_BG_colour", envir = env)
  Recycled_RD_colour <- get("Recycled_RD_colour", envir = env)
  Accumulated_colour <- get("Accumulated_colour", envir = env)
  
  assign("Component_levels", c("Harvest", "Recycled AG", "Recycled BG", "Recycled RD", "Accumulated"), envir = env)
  assign("Component_colours", c(Harvest_colour, Recycled_AG_colour, Recycled_BG_colour, Recycled_RD_colour, Accumulated_colour), envir = env)
  
  # GHG levels and colours ----
  CH4_color <- "cadetblue"
  N2O_color <- "darkolivegreen3"
  CO2_color <- "gray19"
  
  assign("CH4_color", CH4_color, envir = env)
  assign("N2O_color", N2O_color, envir = env)
  assign("CO2_color", CO2_color, envir = env)
  
  assign("CH4_colour", c("CH4" = CH4_color), envir = env)
  assign("N2O_colour", c("N2O" = N2O_color), envir = env)
  assign("CO2_colour", c("CO2" = CO2_color), envir = env)
  
  CH4_colour <- get("CH4_colour", envir = env)
  N2O_colour <- get("N2O_colour", envir = env)
  CO2_colour <- get("CO2_colour", envir = env)
  
  assign("GHG_levels", c("CH4", "N2O", "CO2"), envir = env)
  assign("GHG_colours", c(CH4_colour, N2O_colour, CO2_colour), envir = env)
  
  # SOC Groups levels and colours ----
  Non_cropland_color <- "darkolivegreen4"
  Fallow_color <- "darkolivegreen3"
  Herbaceous_Rainfed_color <- "darkgoldenrod1"
  Woody_Rainfed_color <- "darkorange2"
  Herbaceous_Irrigated_color <- "cyan3"
  Woody_Irrigated_color <- "cyan4"
  Cropland_color <- "darkkhaki"
  
  assign("Non_cropland_color", Non_cropland_color, envir = env)
  assign("Fallow_color", Fallow_color, envir = env)
  assign("Herbaceous_Rainfed_color", Herbaceous_Rainfed_color, envir = env)
  assign("Woody_Rainfed_color", Woody_Rainfed_color, envir = env)
  assign("Herbaceous_Irrigated_color", Herbaceous_Irrigated_color, envir = env)
  assign("Woody_Irrigated_color", Woody_Irrigated_color, envir = env)
  assign("Cropland_color", Cropland_color, envir = env)
  
  assign("Non_cropland_colour", c("Non-Cropland" = Non_cropland_color), envir = env)
  assign("Fallow_colour", c("Fallow" = Fallow_color), envir = env)
  assign("Herbaceous_Rainfed_colour", c("Herbaceous Rainfed" = Herbaceous_Rainfed_color), envir = env)
  assign("Woody_Rainfed_colour", c("Woody Rainfed" = Woody_Rainfed_color), envir = env)
  assign("Herbaceous_Irrigated_colour", c("Herbaceous Irrigated" = Herbaceous_Irrigated_color), envir = env)
  assign("Woody_Irrigated_colour", c("Woody Irrigated" = Woody_Irrigated_color), envir = env)
  assign("Cropland_colour", c("Cropland" = Cropland_color), envir = env)
  
  Non_cropland_colour <- get("Non_cropland_colour", envir = env)
  Fallow_colour <- get("Fallow_colour", envir = env)
  Herbaceous_Rainfed_colour <- get("Herbaceous_Rainfed_colour", envir = env)
  Woody_Rainfed_colour <- get("Woody_Rainfed_colour", envir = env)
  Herbaceous_Irrigated_colour <- get("Herbaceous_Irrigated_colour", envir = env)
  Woody_Irrigated_colour <- get("Woody_Irrigated_colour", envir = env)
  Cropland_colour <- get("Cropland_colour", envir = env)
  
  assign("SOCgroups_levels", c("Non-cropland", "Fallow", "Herbaceous Rainfed", "Woody Rainfed", "Herbaceous Irrigated", "Woody Irrigated"), envir = env)
  assign("SOCgroups_colours", c(Non_cropland_colour, Fallow_colour, Herbaceous_Rainfed_colour, Woody_Rainfed_colour, Herbaceous_Irrigated_colour, Woody_Irrigated_colour), envir = env)
  assign("SOCgroups_levels_all", c(get("SOCgroups_levels", envir = env), "Cropland", "Total"), envir = env)
  assign("SOCgroups_colours_all", c(get("SOCgroups_colours", envir = env), "darkorchid4", Total_colour), envir = env)
  
  # Fertilizer type levels and colours ----
  Recycling_color <- "darkseagreen"
  Manure_color <- "darkorange3"
  Liquid_color <- "goldenrod3"
  Solid_color <- "goldenrod4"
  Excreta_color <- "darkorange"
  Urban_color <- "darkorange4"
  BNF_color <- "olivedrab4"
  Agro_industry_color <- "plum3"
  Synthetic_color <- "red4"
  External_color <- "darkorange"
  Shade_BNF_color <- Shade_color
  Weeds_BNF_color <- Weeds_color
  ASBNF_color <- "skyblue1"
  Deposition_color <- "gray40"
  
  assign("Recycling_color", Recycling_color, envir = env)
  assign("Manure_color", Manure_color, envir = env)
  assign("Liquid_color", Liquid_color, envir = env)
  assign("Solid_color", Solid_color, envir = env)
  assign("Excreta_color", Excreta_color, envir = env)
  assign("Urban_color", Urban_color, envir = env)
  assign("BNF_color", BNF_color, envir = env)
  assign("Agro_industry_color", Agro_industry_color, envir = env)
  assign("Synthetic_color", Synthetic_color, envir = env)
  assign("External_color", External_color, envir = env)
  assign("Shade_BNF_color", Shade_BNF_color, envir = env)
  assign("Weeds_BNF_color", Weeds_BNF_color, envir = env)
  assign("ASBNF_color", ASBNF_color, envir = env)
  assign("Deposition_color", Deposition_color, envir = env)
  
  assign("Recycling_colour", c("Recycling" = Recycling_color), envir = env)
  assign("Manure_colour", c("Manure" = Manure_color), envir = env)
  assign("Liquid_colour", c("Liquid" = Liquid_color), envir = env)
  assign("Solid_colour", c("Solid" = Solid_color), envir = env)
  assign("Excreta_colour", c("Excreta" = Excreta_color), envir = env)
  assign("Excreta_deposition_colour", c("Excreta_deposition" = Excreta_color), envir = env)
  assign("Urban_colour", c("Urban" = Urban_color), envir = env)
  assign("BNF_colour", c("BNF" = BNF_color), envir = env)
  assign("Agro_industry_colour", c("Agro_industry" = Agro_industry_color), envir = env)
  assign("Agro__industry_colour", c("Agro-industry" = Agro_industry_color), envir = env)
  assign("Synthetic_colour", c("Synthetic" = Synthetic_color), envir = env)
  assign("External_colour", c("External" = External_color), envir = env)
  assign("Organic_colour", c("Organic" = External_color), envir = env)
  assign("Shade_BNF_colour", c("Shade BNF" = Shade_BNF_color), envir = env)
  assign("Weeds_BNF_colour", c("Weeds BNF" = Weeds_BNF_color), envir = env)
  assign("ASBNF_colour", c("ASBNF" = ASBNF_color), envir = env)
  assign("Deposition_colour", c("Deposition" = Deposition_color), envir = env)
  assign("Net_SOM_colour", c("Net_SOM" = SOM_color), envir = env)
  
  N2O_color <- "gray25"
  NH3_color <- "green4"
  NO3_color <- "cyan4"
  Denitrif_color <- "lightgray"
  Unexplained_color <- "lightgray"
  Surplus_color <- "slategray"
  Burning_color <- "gold2"
  Gaseous_color <- "darkslategray3"
  Grazed_weeds_color <- "darkseagreen2"
  Residue_color <- Crop_residue_color
  Production_color <- "orange3"
  
  assign("N2O_color", N2O_color, envir = env)
  assign("NH3_color", NH3_color, envir = env)
  assign("NO3_color", NO3_color, envir = env)
  assign("Denitrif_color", Denitrif_color, envir = env)
  assign("Unexplained_color", Unexplained_color, envir = env)
  assign("Surplus_color", Surplus_color, envir = env)
  assign("Burning_color", Burning_color, envir = env)
  assign("Gaseous_color", Gaseous_color, envir = env)
  assign("Grazed_weeds_color", Grazed_weeds_color, envir = env)
  assign("Residue_color", Residue_color, envir = env)
  assign("Production_color", Production_color, envir = env)
  
  assign("N2O_colour", c("N2O" = N2O_color), envir = env)
  assign("NH3_colour", c("NH3" = NH3_color), envir = env)
  assign("Ammonia_colour", c("Ammonia" = NH3_color), envir = env)
  assign("NO3_colour", c("NO3" = NO3_color), envir = env)
  assign("Gaseous_colour", c("Gaseous_losses" = Gaseous_color), envir = env)
  assign("Denitrif_colour", c("Denitrif" = Denitrif_color), envir = env)
  assign("UsedResidue_colour", c("UsedResidue" = Crop_residue_color), envir = env)
  assign("Used_Residue_colour", c("Used Residue" = Crop_residue_color), envir = env)
  assign("Prod_colour", c("Prod" = Crop_prod_color), envir = env)
  # assign("Production_colour", c("Production" = Crop_prod_color), envir = env)
  assign("Surplus_colour", c("Surplus" = Surplus_color), envir = env)
  assign("Burning_colour", c("Burning" = Burning_color), envir = env)
  assign("Grazed_weeds_colour", c("Grazed_weeds" = Grazed_weeds_color), envir = env)
  assign("Residue_colour", c("Residue" = Residue_color), envir = env)
  assign("Production_colour", c("Production" = Production_color), envir = env)
  assign("Unexplained_colour", c("Unexplained" = Unexplained_color), envir = env)
  
  Synthetic_colour <- get("Synthetic_colour", envir = env)
  Liquid_colour <- get("Liquid_colour", envir = env)
  Solid_colour <- get("Solid_colour", envir = env)
  Excreta_colour <- get("Excreta_colour", envir = env)
  Urban_colour <- get("Urban_colour", envir = env)
  BNF_colour <- get("BNF_colour", envir = env)
  Deposition_colour <- get("Deposition_colour", envir = env)
  Agro_industry_colour <- get("Agro_industry_colour", envir = env)
  SOM_colour <- get("SOM_colour", envir = env)
  Manure_colour <- get("Manure_colour", envir = env)
  Excreta_deposition_colour <- get("Excreta_deposition_colour", envir = env)
  Recycling_colour <- get("Recycling_colour", envir = env)
  Agro__industry_colour <- get("Agro__industry_colour", envir = env)
  External_colour <- get("External_colour", envir = env)
  Shade_BNF_colour <- get("Shade_BNF_colour", envir = env)
  Weeds_BNF_colour <- get("Weeds_BNF_colour", envir = env)
  ASBNF_colour <- get("ASBNF_colour", envir = env)
  SON_colour <- get("SON_colour", envir = env)
  PlantType_levels <- get("PlantType_levels", envir = env)
  PlantType_colours <- get("PlantType_colours", envir = env)
  Surplus_colour <- get("Surplus_colour", envir = env)
  Ammonia_colour <- get("Ammonia_colour", envir = env)
  Used_Residue_colour <- get("Used_Residue_colour", envir = env)
  N2O_colour <- get("N2O_colour", envir = env)
  NH3_colour <- get("NH3_colour", envir = env)
  NO3_colour <- get("NO3_colour", envir = env)
  Denitrif_colour <- get("Denitrif_colour", envir = env)
  UsedResidue_colour <- get("UsedResidue_colour", envir = env)
  Prod_colour <- get("Prod_colour", envir = env)
  Burning_colour <- get("Burning_colour", envir = env)
  Grazed_weeds_colour <- get("Grazed_weeds_colour", envir = env)
  Gaseous_colour <- get("Gaseous_colour", envir = env)
  Net_SOM_colour <- get("Net_SOM_colour", envir = env)
  Unexplained_colour <- get("Unexplained_colour", envir = env)
  
  assign("Fert_type_ext_levels", c("Synthetic", "Liquid", "Solid", "Excreta", "Urban", "BNF", "Deposition"), envir = env)
  assign("Fert_type_ext_colours", c(Synthetic_colour, Liquid_colour, Solid_colour, Excreta_colour, Urban_colour, BNF_colour, Deposition_colour), envir = env)
  assign("Fert_type_extSOM_levels", c("Synthetic", "Liquid", "Solid", "Excreta", "Agro_industry", "Urban", "BNF", "SOM", "Accum_loss", "Deposition"), envir = env)
  assign("Fert_type_extSOM_colours", c(Synthetic_colour, Liquid_colour, Solid_colour, Excreta_colour, Agro_industry_colour, Urban_colour, BNF_colour, SOM_colour, Accumulated_colour, Deposition_colour), envir = env)
  assign("Fertilizers_levels", c("Synthetic", "Manure", "Excreta deposition", "Recycling", "Agro-industry", "Urban", "SOM"), envir = env)
  assign("Fertilizers_colours", c(Synthetic_colour, Manure_colour, Excreta_deposition_colour, Recycling_colour, Agro__industry_colour, Urban_colour, SOM_colour), envir = env)
  assign("Fert_res_levels", c("Synthetic", "Manure", "Urban", "BNF", "SOM", "Deposition"), envir = env)
  assign("Fertilizers_colours_all", c(get("Fertilizers_colours", envir = env), Total_colour), envir = env)
  assign("SoilInputC_levels", c("External", PlantType_levels), envir = env)
  assign("SoilInputC_colours", c(External_colour, PlantType_colours), envir = env)
  assign("SoilInputN_levels", c("SON", "Synthetic", get("SoilInputC_levels", envir = env), "Deposition"), envir = env)
  assign("SoilInputN_colours", c(SON_colour, Synthetic_colour, get("SoilInputC_colours", envir = env), Deposition_colour), envir = env)
  assign("Net_InputN_levels", c("SON", "Synthetic", "External", "Shade BNF", "Weeds BNF", "ASBNF", "Deposition"), envir = env)
  assign("Net_InputN_colours", c(SON_colour, Synthetic_colour, External_colour, Shade_BNF_colour, Weeds_BNF_colour, ASBNF_colour, Deposition_colour), envir = env)
  assign("Net_InputN_levels_short", c("SON", "Synthetic", "External", "BNF", "Deposition"), envir = env)
  assign("Net_InputN_colours_short", c(SON_colour, Synthetic_colour, External_colour, BNF_colour, Deposition_colour), envir = env)
  assign("N_IO_levels", c("Surplus", "Shade products", "Crop residue", "Crop product", "SON", "Synthetic", "External", "BNF", "Deposition"), envir = env)
  assign("N_IO_colours", c(Surplus_colour, Shade_products_colour, Crop_residue_colour, Crop_prod_colour, SON_colour, Synthetic_colour, External_colour, BNF_colour, Deposition_colour), envir = env)
  assign("Output_levels", c("N2O", "NH3", "NO3", "Denitrif", "Burning", "SOM", "Accumulated", "UsedResidue", "Prod"), envir = env)
  assign("Output_color_vector", c(N2O_colour, NH3_colour, NO3_colour, Denitrif_colour, Burning_colour, SOM_colour, Accumulated_colour, UsedResidue_colour, Prod_colour), envir = env)
  assign("Output_levels_res", c("Ammonia", "Surplus", "SOM", "Used Residue", "Production"), envir = env)
  assign("Output_colours_res", c(Ammonia_colour, Surplus_colour, SOM_colour, Used_Residue_colour, Production_colour), envir = env)
  assign("N_outputs_levels", c("Surplus", "SON", "Shade products", "Crop residue", "Crop product"), envir = env)
  assign("N_outputs_colours", c(Surplus_colour, SON_colour, Shade_products_colour, Crop_residue_colour, Crop_prod_colour), envir = env)
  assign("N_bal_levels", c("Unexplained", "Gaseous_losses", "NO3", "NH3", "Burning", "Accumulated", "Net_SOM", "Grazed_weeds", "Residue", "Production", "Synthetic", "Urban", "Manure", "Excreta", "Agro_industry", "BNF", "Deposition"), envir = env)
  assign("N_bal_colours", c(Unexplained_colour, Gaseous_colour, NO3_colour, NH3_colour, Burning_colour, Accumulated_colour, Net_SOM_colour, Grazed_weeds_colour, Grazed_weeds_colour, Residue_colour, Production_colour, Synthetic_colour, Urban_colour, Manure_colour, Excreta_colour, Agro_industry_colour, BNF_colour, Deposition_colour), envir = env)
  
  # Cat_1 vectors and levels ----
  Fallow_color <- "bisque2"
  Cereals_color <- "darkgoldenrod1"
  Pulses_color <- "darkgoldenrod3"
  Roots_tubers_color <- "khaki"
  Oilseeds_color <- "lightpink"
  Vegetables_color <- "mediumorchid"
  Fibres_color <- "ivory1"
  Spices_color <- "grey40"
  Sugar_crops_color <- "peachpuff"
  Fodder_green_color <- "olivedrab2"
  Olives_color <- "darkolivegreen4"
  Grapes_color <- "deeppink4"
  Treenuts_color <- "orange3"
  Fruits_color <- "palevioletred1"
  Citrus_color <- "sienna2"
  Other_color <- "grey"
  Grass_color <- "darkolivegreen"
  
  assign("Fallow_color", Fallow_color, envir = env)
  assign("Cereals_color", Cereals_color, envir = env)
  assign("Pulses_color", Pulses_color, envir = env)
  assign("Roots_tubers_color", Roots_tubers_color, envir = env)
  assign("Oilseeds_color", Oilseeds_color, envir = env)
  assign("Vegetables_color", Vegetables_color, envir = env)
  assign("Fibres_color", Fibres_color, envir = env)
  assign("Spices_color", Spices_color, envir = env)
  assign("Sugar_crops_color", Sugar_crops_color, envir = env)
  assign("Fodder_green_color", Fodder_green_color, envir = env)
  assign("Olives_color", Olives_color, envir = env)
  assign("Grapes_color", Grapes_color, envir = env)
  assign("Treenuts_color", Treenuts_color, envir = env)
  assign("Fruits_color", Fruits_color, envir = env)
  assign("Citrus_color", Citrus_color, envir = env)
  assign("Other_color", Other_color, envir = env)
  assign("Grass_color", Grass_color, envir = env)
  
  assign("Fallow_colour", c("Fallow" = Fallow_color), envir = env)
  assign("Cereals_colour", c("Cereals" = Cereals_color), envir = env)
  assign("Pulses_colour", c("Pulses" = Pulses_color), envir = env)
  assign("Roots_tubers_colour", c("Roots_tubers" = Roots_tubers_color), envir = env)
  assign("Oilseeds_colour", c("Oilseeds" = Oilseeds_color), envir = env)
  assign("Vegetables_colour", c("Vegetables" = Vegetables_color), envir = env)
  assign("Fibres_colour", c("Fibres" = Fibres_color), envir = env)
  assign("Spices_colour", c("Spices" = Spices_color), envir = env)
  assign("Sugar_crops_colour", c("Sugar_crops" = Sugar_crops_color), envir = env)
  assign("Fodder_green_colour", c("Fodder_green" = Fodder_green_color), envir = env)
  assign("Olives_colour", c("Olives" = Olives_color), envir = env)
  assign("Grapes_colour", c("Grapes" = Grapes_color), envir = env)
  assign("Treenuts_colour", c("Treenuts" = Treenuts_color), envir = env)
  assign("Fruits_colour", c("Fruits" = Fruits_color), envir = env)
  assign("Citrus_colour", c("Citrus" = Citrus_color), envir = env)
  assign("Other_colour", c("Other" = Other_color), envir = env)
  assign("Grass_colour", c("Grass" = Grass_color), envir = env)
  
  Fallow_colour <- get("Fallow_colour", envir = env)
  Cereals_colour <- get("Cereals_colour", envir = env)
  Pulses_colour <- get("Pulses_colour", envir = env)
  Roots_tubers_colour <- get("Roots_tubers_colour", envir = env)
  Vegetables_colour <- get("Vegetables_colour", envir = env)
  Fibres_colour <- get("Fibres_colour", envir = env)
  Spices_colour <- get("Spices_colour", envir = env)
  Sugar_crops_colour <- get("Sugar_crops_colour", envir = env)
  Fodder_green_colour <- get("Fodder_green_colour", envir = env)
  Olives_colour <- get("Olives_colour", envir = env)
  Grapes_colour <- get("Grapes_colour", envir = env)
  Treenuts_colour <- get("Treenuts_colour", envir = env)
  Fruits_colour <- get("Fruits_colour", envir = env)
  Citrus_colour <- get("Citrus_colour", envir = env)
  Other_colour <- get("Other_colour", envir = env)
  
  assign("Cat_1_levels", c("Fallow", "Cereals", "Pulses", "Roots_tubers", "Oilseeds", "Vegetables", "Fibres", "Spices", "Sugar_crops", "Fodder_green", "Olives", "Grapes", "Treenuts", "Fruits", "Citrus", "Other"), envir = env)
  assign("Crop_colours_NoFallow", c(Cereals_colour, Pulses_colour, Roots_tubers_colour, Oilseeds_colour, Vegetables_colour, Fibres_colour, Spices_colour, Sugar_crops_colour, Fodder_green_colour, Olives_colour, Grapes_colour, Treenuts_colour, Fruits_colour, Citrus_colour, Other_colour), envir = env)
  assign("Crop_colours", c(Fallow_colour, get("Crop_colours_NoFallow", envir = env)), envir = env)
  
  # Cat01 levels and colours ----
  Meat_color <- "brown3"
  Milk_color <- "azure2"
  Other_animal_products_color <- "coral2"
  Fish_color <- "cadetblue3"
  Other_processed_color <- "azure3"
  Cakes_color <- "darkgoldenrod4"
  Oils_color <- "gold"
  Other_plant_products_color <- "darkolivegreen1"
  Fruits_and_vegetables_color <- "darkorchid4"
  Grains_color <- "darkgoldenrod3"
  
  assign("Meat_color", Meat_color, envir = env)
  assign("Milk_color", Milk_color, envir = env)
  assign("Other_animal_products_color", Other_animal_products_color, envir = env)
  assign("Fish_color", Fish_color, envir = env)
  assign("Other_processed_color", Other_processed_color, envir = env)
  assign("Cakes_color", Cakes_color, envir = env)
  assign("Oils_color", Oils_color, envir = env)
  assign("Other_plant_products_color", Other_plant_products_color, envir = env)
  assign("Fruits_and_vegetables_color", Fruits_and_vegetables_color, envir = env)
  assign("Grains_color", Grains_color, envir = env)
  
  assign("Meat_colour", c("Meat" = Meat_color), envir = env)
  assign("Milk_colour", c("Milk" = Milk_color), envir = env)
  assign("Other_animal_products_colour", c("Other animal products" = Other_animal_products_color), envir = env)
  assign("Other_animal_colour", c("Other animal" = Other_animal_products_color), envir = env)
  assign("Fish_colour", c("Fish" = Fish_color), envir = env)
  assign("Fish_seafood_colour", c("Fish and seafood" = Fish_color), envir = env)
  assign("Other_processed_colour", c("Other processed" = Other_processed_color), envir = env)
  assign("Cakes_colour", c("Cakes" = Cakes_color), envir = env)
  assign("Oils_colour", c("Oils" = Oils_color), envir = env)
  assign("Oil_colour", c("Oil" = Oils_color), envir = env)
  assign("Other_plant_products_colour", c("Other plant products" = Other_plant_products_color), envir = env)
  assign("Other_vegetal_colour", c("Other vegetal" = Other_plant_products_color), envir = env)
  assign("Fruits_and_vegetables_colour", c("Fruits and vegetables" = Fruits_and_vegetables_color), envir = env)
  assign("Grains_colour", c("Grains" = Grains_color), envir = env)
  assign("Sugar_alcohol_colour", c("Sugar and alcohol" = Other_processed_color), envir = env)
  assign("Legumes_colour", c("Legumes" = Pulses_color), envir = env)
  assign("Roots_and_tubers_colour", c("Roots and tubers" = Roots_tubers_color), envir = env)
  
  Meat_colour <- get("Meat_colour", envir = env)
  Milk_colour <- get("Milk_colour", envir = env)
  Other_animal_products_colour <- get("Other_animal_products_colour", envir = env)
  Other_animal_colour <- get("Other_animal_colour", envir = env)
  Fish_colour <- get("Fish_colour", envir = env)
  Fish_seafood_colour <- get("Fish_seafood_colour", envir = env)
  Other_processed_colour <- get("Other_processed_colour", envir = env)
  Cakes_colour <- get("Cakes_colour", envir = env)
  Oils_colour <- get("Oils_colour", envir = env)
  Oil_colour <- get("Oil_colour", envir = env)
  Other_plant_products_colour <- get("Other_plant_products_colour", envir = env)
  Other_vegetal_colour <- get("Other_vegetal_colour", envir = env)
  Fruits_and_vegetables_colour <- get("Fruits_and_vegetables_colour", envir = env)
  Sugar_alcohol_colour <- get("Sugar_alcohol_colour", envir = env)
  Legumes_colour <- get("Legumes_colour", envir = env)
  Roots_and_tubers_colour <- get("Roots_and_tubers_colour", envir = env)
  
  assign("Cat_01_Levels", c("Meat", "Milk", "Other animal products", "Fish", "Other processed", "Cakes", "Oils", "Other plant products", "Fruits and vegetables", "Oilseeds", "Grains"), envir = env)
  assign("Cat_01_colours", c(Meat_colour, Milk_colour, Other_animal_products_colour, Fish_colour, Other_processed_colour, Cakes_colour, Oils_colour, Other_plant_products_colour, Fruits_and_vegetables_colour, Oilseeds_colour, Grains_colour), envir = env)
  assign("Cat_diet_agg_Levels", c("Meat", "Milk", "Other animal", "Fish and seafood", "Other vegetal", "Sugar and alcohol", "Oil", "Treenuts", "Fruits", "Vegetables", "Roots and tubers", "Legumes", "Cereals"), envir = env)
  assign("Cat_diet_agg_colours", c(Meat_colour, Milk_colour, Other_animal_colour, Fish_seafood_colour, Other_vegetal_colour, Sugar_alcohol_colour, Oil_colour, Treenuts_colour, Fruits_colour, Vegetables_colour, Roots_and_tubers_colour, Legumes_colour, Cereals_colour), envir = env)
  assign("Cat_01_Levels_prod", c("Meat", "Milk", "Other animal products", "Fish", "Other plant products", "Fruits and vegetables", "Oilseeds", "Grains"), envir = env)
  assign("Cat_01_colours_prod", c(Meat_colour, Milk_colour, Other_animal_products_colour, Fish_colour, Other_plant_products_colour, Fruits_and_vegetables_colour, Oilseeds_colour, Grains_colour), envir = env)
  
  # Miscellaneous levels ----
  assign("Livestock_Species", c("Cattle", "Sheep", "Goats", "Pigs", "Horses", "Donkeys_mules", "Poultry", "Rabbits"), envir = env)
  assign("LandUse_levels", c("Unproductive", "Forest_high", "Forest_low", "Pasture_Shrubland", "Dehesa", "Fallow", "Harvested"), envir = env)
  assign("Synthetic_ItemName_levels", c("NPK", "U", "CAN", "AN", "AS", "Saltpeter", "Guano"), envir = env)
  assign("Synthetic_ItemName_items", c("Other", get("Synthetic_ItemName_levels", envir = env)), envir = env)
  assign("Synthetic_ItemName_tot", c(get("Synthetic_ItemName_levels", envir = env), "N fertilizers"), envir = env)
  assign("FertilizersGWP_levels", c("Upstream emissions", "N2O indirect NO3", "N2O indirect NH3", "N2O direct", "Total fertilizer emissions"), envir = env)
  assign("Function_levels", c("Total", "Traction", "Fertilization", "Irrigation", "Protection", "Land", "Carbon cycle"), envir = env)
  assign("Continents_levels", c("RoW", "Africa", "Asia and Pacific", "America Other", "North America", "Europe"), envir = env)
  assign("Impact_levels", c("Carbon cycle", "Fertilization", "Irrigation", "Protection", "Traction", "Total"), envir = env)
  assign("Input_cats", c("Traction", "Fertilization", "Irrigation", "Protection"), envir = env)
  assign("Input_cats_tot", c("Traction", "Fertilization", "Irrigation", "Protection", "Total"), envir = env)
  
  # NPP categories ----
  NPP_comp <- c("Prod_MgC", "Burn_MgC", "Used_MgC", "Unharvested_MgC", "Root_MgC", "Weeds_AG_MgC", "Weeds_BG_MgC")
  NPP_names <- c("Production", "Burnt residues", "Used residues", "Unharvested residues", "Crop belowground", "Weeds aboveground", "Weeds belowground")
  NPP_cat <- tibble::tibble(NPP_comp, NPP_names) |>
    dplyr::mutate(NPP_names = factor(NPP_names, levels = c("Weeds belowground", "Crop belowground", "Weeds aboveground", "Unharvested residues", "Burnt residues", "Used residues", "Production")))
  assign("NPP_comp", NPP_comp, envir = env)
  assign("NPP_names", NPP_names, envir = env)
  assign("NPP_cat", NPP_cat, envir = env)
  
  # C input categories ----
  C_input_comp <- c("Residue", "Root", "Weeds_AG", "Weeds_BG", "Agro_industry", "Manure", "Urban")
  C_input_names <- c("Unharvested residues", "Crop belowground", "Weeds aboveground", "Weeds belowground", "Agro-industry", "Manure", "Urban")
  C_input_cat <- tibble::tibble(C_input_comp, C_input_names) |>
    dplyr::rename(Fert_type = C_input_comp, C_input_name = C_input_names) |>
    dplyr::mutate(C_input_names = factor(C_input_name, levels = c("Urban", "Manure", "Agro-industry", "Weeds belowground", "Crop belowground", "Weeds aboveground", "Unharvested residues")))
  assign("C_input_comp", C_input_comp, envir = env)
  assign("C_input_names", C_input_names, envir = env)
  assign("C_input_cat", C_input_cat, envir = env)
  
  # Water variables ----
  assign("Water_variables", c("PET", "AET", "ETc", "WaterInput", "BW_tot", "BW_net", "FloodWater", "PRE", "PREe"), envir = env)
  
  # Figure parameters and color codes ----
  assign("YellowRedPalette", c("white", "yellow", "gold", "orange", "orangered", "red", "darkred", "black"), envir = env)
  assign("Synthetic_ItemName_itemsshort_colours", c("firebrick2", "darksalmon", "darkred", "indianred", "indianred1", "lightgoldenrod", "lightgoldenrod4"), envir = env)
  assign("Synthetic_ItemName_items_colours", c("darkgrey", get("Synthetic_ItemName_itemsshort_colours", envir = env)), envir = env)
  assign("Synthetic_ItemName_all_colours", c("darkgrey", get("Synthetic_ItemName_itemsshort_colours", envir = env), "black"), envir = env)
  assign("Synthetic_ItemName_tot_colours", c(get("Synthetic_ItemName_itemsshort_colours", envir = env), "black"), envir = env)
  assign("FertilizersGWP_colours", c("firebrick", "azure3", "darkolivegreen2", "chartreuse4", "black"), envir = env)
  assign("ResidueWeed_colours", c("gray50", "gray80", "palegreen3"), envir = env)
  assign("NPP_colours", c(get("ResidueWeed_colours", envir = env), "orange2", "goldenrod1", "steelblue1", "steelblue"), envir = env)
  assign("C_input_colours", c("darkorange4", "darkorange3", "plum3", get("ResidueWeed_colours", envir = env), "goldenrod1"), envir = env)
  assign("Water_colours", c("coral4", "darkorange3", "darkorange", "black", "darkblue", "cornflowerblue", "cadetblue3", "darkolivegreen4", "darkolivegreen3"), envir = env)
  assign("Impact_colours", c("bisque3", "darkolivegreen4", "cadetblue2", "brown", "darkorchid1", "azure4", "black"), envir = env)
  assign("Continent_colours", c("gray", "chocolate2", "darkolivegreen4", "chocolate4", "cadetblue3", "dodgerblue4"), envir = env)
  
  # GL levels and colours ----
  Animal_color <- "coral3"
  Sea_color <- "cornflowerblue"
  assign("Animal_color", Animal_color, envir = env)
  assign("Sea_color", Sea_color, envir = env)
  assign("Animal_colour", c("Animal" = Animal_color), envir = env)
  assign("Sea_colour", c("Sea" = Sea_color), envir = env)
  
  Animal_colour <- get("Animal_colour", envir = env)
  Sea_colour <- get("Sea_colour", envir = env)
  
  assign("LU_simple_levels", c("Animal", "Cropland", "Cropland SOC", "Non-Cropland", "Non-Cropland SOC", "LULUCF", "Industry", "Transport", "Sea"), envir = env)
  assign("LU_type_levels", c("Animal", "Cropland", "Non-Cropland", "Sea"), envir = env)
  assign("LU_type_colours", c(Animal_colour, Cropland_colour, Non_cropland_colour, Sea_colour), envir = env)
  assign("Colours_AniCroGra", c("coral3", "darkkhaki", "darkolivegreen4"), envir = env)
  assign("Colours_GL", c("coral3", "darkkhaki", "darkgoldenrod2", "darkolivegreen4", "darkgoldenrod", "chartreuse3", "azure3", "azure4", "cornflowerblue"), envir = env)
  assign("Colours_NRE_GL", c("coral3", "darkolivegreen4", "darkkhaki", "azure3", "azure4", "cornflowerblue"), envir = env)
  assign("Colours_Land_GL", c("darkkhaki", "darkolivegreen4", "azure3"), envir = env)
  
  # SJOS Levels and colours ----
  assign("Nour_levels", c("Over", "Adequate", "Under"), envir = env)
  assign("SJOS_levels", rev(c("Within_boundary Under", "Within_boundary Adequate", "Within_boundary Over", "Exceedance Under", "Exceedance Adequate", "Exceedance Over")), envir = env)
  assign("SJOS_colours", c("Within_boundary Under" = "lightseagreen", "Within_boundary Adequate" = "lightgreen", "Within_boundary Over" = "burlywood3", "Exceedance Under" = "mediumpurple", "Exceedance Adequate" = "salmon1", "Exceedance Over" = "indianred3"), envir = env)
  assign("Nourish_colours", c("Under" = "blue", "Adequate" = "green", "Over" = "red"), envir = env)
  assign("Nourish_colours_tot", c(get("Nourish_colours", envir = env), "Total" = "black"), envir = env)
  assign("Nourish_levels", c("Under", "Adequate", "Over"), envir = env)
  assign("Nourish_levels_tot", c(get("Nourish_levels", envir = env), "Total"), envir = env)
  
  invisible(NULL)
}
