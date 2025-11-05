# Dates ----
Month_names <- c(
  "January",
  "February",
  "March",
  "April",
  "May",
  "June",
  "July",
  "August",
  "September",
  "October",
  "November",
  "December"
)
Month_order <- Month_names

Month_integer <- c(0:11)
Month_number <- c(1:12)
Month_numbers <- data.frame(
  Month_names,
  Month_integer,
  Month_number
) |>
  dplyr::mutate(
    Month = Month_integer / 12,
    Month_number = as.numeric(Month_number)
  ) |>
  dplyr::select(-Month_integer)

# General colours ----
Total_color <- "black"
SOM_color <- "peachpuff3"

Total_colour <- c("Total" = Total_color)
SOM_colour <- c("SOM" = SOM_color)
SON_colour <- c("SON" = SOM_color)
SOC_colour <- c("SOC" = SOM_color)

# Irrig and Irrig_cat levels and colours ----

# Individual category and color elements
Temperate_Irrigated_color <- "cadetblue"
Temperate_Rainfed_color <- "darkgoldenrod"
Mediterranean_Irrigated_color <- "cadetblue2"
Mediterranean_Rainfed_color <- "darkgoldenrod1"
Greenhouse_color <- "plum4"
Irrigated_color <- "skyblue2"
Rainfed_color <- "sandybrown"

# Create vectors for each category and color pair
Temperate_Irrigated_colour <- c("Temperate Irrigated" = Temperate_Irrigated_color)
Temperate_Rainfed_colour <- c("Temperate Rainfed" = Temperate_Rainfed_color)
Mediterranean_Irrigated_colour <- c("Mediterranean Irrigated" = Mediterranean_Irrigated_color)
Mediterranean_Rainfed_colour <- c("Mediterranean Rainfed" = Mediterranean_Rainfed_color)
Greenhouse_colour <- c("Greenhouse" = Greenhouse_color)
Irrigated_colour <- c("Irrigated" = Irrigated_color)
Rainfed_colour <- c("Rainfed" = Rainfed_color)


# Combine all category and color pairs into one vector
Irrig_Climate_levels <- c("Temperate Irrigated", "Temperate Rainfed", "Mediterranean Irrigated", "Mediterranean Rainfed")
Irrig_Climate_colours <- c(
  Temperate_Irrigated_colour,
  Temperate_Rainfed_colour,
  Mediterranean_Irrigated_colour,
  Mediterranean_Rainfed_colour
)

Irrig_Climate_levels_all <- c(Irrig_Climate_levels, "Total Spain")
Irrig_Climate_colours_all <- c(Irrig_Climate_colours, Total_colour)

Irrig_levels <- c("Irrigated", "Rainfed")
Irrig_colours_simple <- c(
  Irrigated_colour,
  Rainfed_colour
)

Irrig_tot_levels <- c("Total", Irrig_levels)
Irrig_tot_colours <- c(
  Total_colour,
  Irrig_colours_simple
)

Irrig_cat_levels <- c("Greenhouse", "Irrigated", "Rainfed")
Irrig_colours <- c(
  Greenhouse_colour,
  Irrig_colours_simple
)


# NPP levels and colours ----

# Individual category and color elements
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

# Create vectors for each category and color pair
Crop_colour <- c("Crop" = Crop_color)
Crop_Biomass_colour <- c("Crop Biomass" = Crop_color)
Crop_prod_colour <- c("Crop product" = Crop_prod_color)
Crop_residue_colour <- c("Crop residue" = Crop_residue_color)
Crop_BG_colour <- c("Crop BG" = Crop_BG_color)
Crop_RD_colour <- c("Crop RD" = Crop_RD_color)

Shade_colour <- c("Shade" = Shade_color)
Shade_Biomass_colour <- c("Shade Biomass" = Shade_color)
Shade_products_colour <- c("Shade products" = Shade_products_color)
Shade_fruit_colour <- c("Shade fruit" = Shade_fruit_color)
Shade_roundwood_colour <- c("Shade roundwood" = Shade_roundwood_color)
Shade_firewood_colour <- c("Shade firewood" = Shade_firewood_color)
Shade_AG_colour <- c("Shade AG" = Shade_AG_color)
Shade_BG_colour <- c("Shade BG" = Shade_BG_color)
Shade_RD_colour <- c("Shade RD" = Shade_RD_color)

Weeds_colour <- c("Weeds" = Weeds_color)
Weeds_AG_colour <- c("Weeds AG" = Weeds_AG_color)
Weeds_BG_colour <- c("Weeds BG" = Weeds_BG_color)
Weeds_RD_colour <- c("Weeds RD" = Weeds_RD_color)

# Combine all category and color pairs into one vector
Harvest_levels <- c("Crop product", "Crop residue", "Shade fruit", "Shade roundwood", "Shade firewood")
Harvest_colours <- c(
  Crop_prod_colour,
  Crop_residue_colour,
  Shade_fruit_colour,
  Shade_roundwood_colour,
  Shade_firewood_colour
)

Recycled_levels <- c("Crop residue", "Crop BG", "Crop RD", "Shade AG", "Shade BG", "Shade RD", "Weeds AG", "Weeds BG", "Weeds RD")
Recycled_colours <- c(
  Crop_residue_colour,
  Crop_BG_colour,
  Crop_RD_colour,
  Shade_AG_colour,
  Shade_BG_colour,
  Shade_RD_colour,
  Weeds_AG_colour,
  Weeds_BG_colour,
  Weeds_RD_colour
)

PlantType_levels <- c("Crop", "Shade", "Weeds")
PlantType_colours <- c(
  Crop_colour,
  Shade_colour,
  Weeds_colour
)

Stock_levels <- c("Crop Biomass", "Shade Biomass", "SOC")
Stock_colours <- c(Crop_Biomass_colour, Shade_Biomass_colour, SOC_colour)


# NPP component levels and colours ----

# Individual category and color elements
Harvest_color <- "purple4"
Recycled_AG_color <- "orchid4"
Recycled_BG_color <- "orchid"
Recycled_RD_color <- "orchid1"
Accumulated_color <- "burlywood4"

# Create vectors for each category and color pair
Harvest_colour <- c("Harvest" = Harvest_color)
Recycled_AG_colour <- c("Recycled AG" = Recycled_AG_color)
Recycled_BG_colour <- c("Recycled BG" = Recycled_BG_color)
Recycled_RD_colour <- c("Recycled RD" = Recycled_RD_color)
Accumulated_colour <- c("Accumulated" = Accumulated_color)

# Combine all category and color pairs into one vector
Component_levels <- c("Harvest", "Recycled AG", "Recycled BG", "Recycled RD", "Accumulated")
Component_colours <- c(
  Harvest_colour,
  Recycled_AG_colour,
  Recycled_BG_colour,
  Recycled_RD_colour,
  Accumulated_colour
)

# GHG levels and colours ----

# Individual category and color elements
CH4_color <- "cadetblue"
N2O_color <- "darkolivegreen3"
CO2_color <- "gray19"

# Create vectors for each category and color pair
CH4_colour <- c("CH4" = CH4_color)
N2O_colour <- c("N2O" = N2O_color)
CO2_colour <- c("CO2" = CO2_color)

# Combine all category and color pairs into one vector
GHG_levels <- c(
  "CH4",
  "N2O",
  "CO2"
)
GHG_colours <- c(
  CH4_colour,
  N2O_colour,
  CO2_colour
)

# SOC Groups levels and colours ----

# Individual category and color elements
Non_cropland_color <- "darkolivegreen4"
Fallow_color <- "darkolivegreen3"
Herbaceous_Rainfed_color <- "darkgoldenrod1"
Woody_Rainfed_color <- "darkorange2"
Herbaceous_Irrigated_color <- "cyan3"
Woody_Irrigated_color <- "cyan4"
Cropland_color <- "darkkhaki"

# Create vectors for each category and color pair
Non_cropland_colour <- c("Non-Cropland" = Non_cropland_color)
Fallow_colour <- c("Fallow" = Fallow_color)
Herbaceous_Rainfed_colour <- c("Herbaceous Rainfed" = Herbaceous_Rainfed_color)
Woody_Rainfed_colour <- c("Woody Rainfed" = Woody_Rainfed_color)
Herbaceous_Irrigated_colour <- c("Herbaceous Irrigated" = Herbaceous_Irrigated_color)
Woody_Irrigated_colour <- c("Woody Irrigated" = Woody_Irrigated_color)
Cropland_colour <- c("Cropland" = Cropland_color)

# Combine all category and color pairs into one vector
SOCgroups_levels <- c(
  "Non-cropland",
  "Fallow",
  "Herbaceous Rainfed",
  "Woody Rainfed",
  "Herbaceous Irrigated",
  "Woody Irrigated"
)
SOCgroups_colours <- c(
  Non_cropland_colour,
  Fallow_colour,
  Herbaceous_Rainfed_colour,
  Woody_Rainfed_colour,
  Herbaceous_Irrigated_colour,
  Woody_Irrigated_colour
)

SOCgroups_levels_all <- c(
  SOCgroups_levels,
  "Cropland",
  "Total"
)
SOCgroups_colours_all <- c(
  SOCgroups_colours,
  "darkorchid4",
  Total_colour
)


# Fertilizer type levels and colours ----

# Individual category and color elements
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


# Create vectors for each category and color pair
Recycling_colour <- c("Recycling" = Recycling_color)
Manure_colour <- c("Manure" = Manure_color)
Liquid_colour <- c("Liquid" = Liquid_color)
Solid_colour <- c("Solid" = Solid_color)
Excreta_colour <- c("Excreta" = Excreta_color)
Excreta_deposition_colour <- c("Excreta_deposition" = Excreta_color)
Urban_colour <- c("Urban" = Urban_color)
BNF_colour <- c("BNF" = BNF_color)
Agro_industry_colour <- c("Agro_industry" = Agro_industry_color)
Agro__industry_colour <- c("Agro-industry" = Agro_industry_color)
Synthetic_colour <- c("Synthetic" = Synthetic_color)
External_colour <- c("External" = External_color)
Organic_colour <- c("Organic" = External_color)
Shade_BNF_colour <- c("Shade BNF" = Shade_BNF_color)
Weeds_BNF_colour <- c("Weeds BNF" = Weeds_BNF_color)
ASBNF_colour <- c("ASBNF" = ASBNF_color)
Deposition_colour <- c("Deposition" = Deposition_color)
Net_SOM_colour <- c("Net_SOM" = SOM_color)

# Individual category and color elements
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

# Create vectors for each category and color pair
N2O_colour <- c("N2O" = N2O_color)
NH3_colour <- c("NH3" = NH3_color)
Ammonia_colour <- c("Ammonia" = NH3_color)
NO3_colour <- c("NO3" = NO3_color)
Gaseous_colour <- c("Gaseous_losses" = Gaseous_color)
Denitrif_colour <- c("Denitrif" = Denitrif_color)
UsedResidue_colour <- c("UsedResidue" = Crop_residue_color)
Used_Residue_colour <- c("Used Residue" = Crop_residue_color)
Prod_colour <- c("Prod" = Crop_prod_color)
Production_colour <- c("Production" = Crop_prod_color)
Surplus_colour <- c("Surplus" = Surplus_color)
Burning_colour <- c("Burning" = Burning_color)
Grazed_weeds_colour <- c("Grazed_weeds" = Grazed_weeds_color)
Residue_colour <- c("Residue" = Residue_color)
Production_colour <- c("Production" = Production_color)
Unexplained_colour <- c("Unexplained" = Unexplained_color)

# Combine all category and color pairs into one vector
Fert_type_ext_levels <- c("Synthetic", "Liquid", "Solid", "Excreta", "Urban", "BNF", "Deposition")
Fert_type_ext_colours <- c(Synthetic_colour, Liquid_colour, Solid_colour, Excreta_colour, Urban_colour, BNF_colour, Deposition_colour)

Fert_type_extSOM_levels <- c("Synthetic", "Liquid", "Solid", "Excreta", "Agro_industry", "Urban", "BNF", "SOM", "Accum_loss", "Deposition")
Fert_type_extSOM_colours <- c(
  Synthetic_colour, Liquid_colour, Solid_colour, Excreta_colour, Agro_industry_colour,
  Urban_colour, BNF_colour, SOM_colour, Accumulated_colour, Deposition_colour
)

Fertilizers_levels <- c("Synthetic", "Manure", "Excreta deposition", "Recycling", "Agro-industry", "Urban", "SOM")
Fertilizers_colours <- c(Synthetic_colour, Manure_colour, Excreta_deposition_colour, Recycling_colour, Agro__industry_colour, Urban_colour, SOM_colour)

Fert_res_levels <- c("Synthetic", "Manure", "Urban", "BNF", "SOM", "Deposition")
Fertilizers_colours <- c(Synthetic_colour, Manure_colour, BNF_colour, SOM_colour, Deposition_colour)

Fertilizers_colours_all <- c(
  Fertilizers_colours,
  Total_colour
)

SoilInputC_levels <- c("External", PlantType_levels)
SoilInputC_colours <- c(External_colour, PlantType_colours)

SoilInputN_levels <- c("SON", "Synthetic", SoilInputC_levels, "Deposition")
SoilInputN_colours <- c(SON_colour, Synthetic_colour, SoilInputC_colours, Deposition_colour)

Net_InputN_levels <- c("SON", "Synthetic", "External", "Shade BNF", "Weeds BNF", "ASBNF", "Deposition")
Net_InputN_colours <- c(SON_colour, Synthetic_colour, External_colour, Shade_BNF_colour, Weeds_BNF_colour, ASBNF_colour, Deposition_colour)

Net_InputN_levels_short <- c("SON", "Synthetic", "External", "BNF", "Deposition")
Net_InputN_colours_short <- c(SON_colour, Synthetic_colour, External_colour, BNF_colour, Deposition_colour)

N_IO_levels <- c("Surplus", "Shade products", "Crop residue", "Crop product", "SON", "Synthetic", "External", "BNF", "Deposition")
N_IO_colours <- c(Surplus_colour, Shade_products_colour, Crop_residue_colour, Crop_prod_colour, SON_colour, Synthetic_colour, External_colour, BNF_colour, Deposition_colour)

# Output type levels and colours ----



# Combine all category and color pairs into one vector
Output_levels <- c("N2O", "NH3", "NO3", "Denitrif", "Burning", "SOM", "Accumulated", "UsedResidue", "Prod")
Output_color_vector <- c(
  N2O_colour, NH3_colour, NO3_colour, Denitrif_colour, Burning_colour, SOM_colour, Accumulated_colour,
  UsedResidue_colour, Prod_colour
)

Output_levels_res <- c("Ammonia", "Surplus", "SOM", "Used Residue", "Production")
Output_colours_res <- c(Ammonia_colour, Surplus_colour, SOM_colour, Used_Residue_colour, Production_colour)

N_outputs_levels <- c("Surplus", "SON", "Shade products", "Crop residue", "Crop product")
N_outputs_colours <- c(Surplus_colour, SON_colour, Shade_products_colour, Crop_residue_colour, Crop_prod_colour)

# Cat_1 vectors and levels -----
# Individual category and color elements
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

# Create vectors for each category and color pair
Fallow_colour <- c("Fallow" = Fallow_color)
Cereals_colour <- c("Cereals" = Cereals_color)
Pulses_colour <- c("Pulses" = Pulses_color)
Roots_tubers_colour <- c("Roots_tubers" = Roots_tubers_color)
Oilseeds_colour <- c("Oilseeds" = Oilseeds_color)
Vegetables_colour <- c("Vegetables" = Vegetables_color)
Fibres_colour <- c("Fibres" = Fibres_color)
Spices_colour <- c("Spices" = Spices_color)
Sugar_crops_colour <- c("Sugar_crops" = Sugar_crops_color)
Fodder_green_colour <- c("Fodder_green" = Fodder_green_color)
Olives_colour <- c("Olives" = Olives_color)
Grapes_colour <- c("Grapes" = Grapes_color)
Treenuts_colour <- c("Treenuts" = Treenuts_color)
Fruits_colour <- c("Fruits" = Fruits_color)
Citrus_colour <- c("Citrus" = Citrus_color)
Other_colour <- c("Other" = Other_color)
Grass_colour <- c("Grass" = Grass_color)

# Combine all category and color pairs into one vector
Cat_1_levels <- c(
  "Fallow", "Cereals", "Pulses", "Roots_tubers", "Oilseeds",
  "Vegetables", "Fibres", "Spices", "Sugar_crops", "Fodder_green",
  "Olives", "Grapes", "Treenuts", "Fruits", "Citrus", "Other"
)
Crop_colours_NoFallow <- c(
  Cereals_colour, Pulses_colour, Roots_tubers_colour, Oilseeds_colour,
  Vegetables_colour, Fibres_colour, Spices_colour, Sugar_crops_colour, Fodder_green_colour,
  Olives_colour, Grapes_colour, Treenuts_colour, Fruits_colour, Citrus_colour,
  Other_colour
)

Crop_colours <- c(
  Fallow_colour,
  Crop_colours_NoFallow
)

# Cat01 levels and colours ----

# Individual category and color elements
Meat_color <- "brown3"
Milk_color <- "azure2"
Other_animal_products_color <- "coral2"
Fish_color <- "cadetblue3"
Other_processed_color <- "azure3"
Cakes_color <- "darkgoldenrod4"
Oils_color <- "gold"
Other_plant_products_color <- "darkolivegreen1"
Fruits_and_vegetables_color <- "darkorchid4"
Oilseeds_color <- "darkkhaki"
Grains_color <- "darkgoldenrod3"

# Create vectors for each category and color pair
Meat_colour <- c("Meat" = Meat_color)
Milk_colour <- c("Milk" = Milk_color)
Other_animal_products_colour <- c("Other animal products" = Other_animal_products_color)
Other_animal_colour <- c("Other animal" = Other_animal_products_color)
Fish_colour <- c("Fish" = Fish_color)
Fish_seafood_colour <- c("Fish and seafood" = Fish_color)
Other_processed_colour <- c("Other processed" = Other_processed_color)
Cakes_colour <- c("Cakes" = Cakes_color)
Oils_colour <- c("Oils" = Oils_color)
Oil_colour <- c("Oil" = Oils_color)
Other_plant_products_colour <- c("Other plant products" = Other_plant_products_color)
Other_vegetal_colour <- c("Other vegetal" = Other_plant_products_color)
Fruits_and_vegetables_colour <- c("Fruits and vegetables" = Fruits_and_vegetables_color)
Oilseeds_colour <- c("Oilseeds" = Oilseeds_color)
Grains_colour <- c("Grains" = Grains_color)
Sugar_alcohol_colour <- c("Sugar and alcohol" = Other_processed_color)
Legumes_colour <- c("Legumes" = Pulses_color)
Roots_and_tubers_colour <- c("Roots and tubers" = Roots_tubers_color)

# Combine all category and color pairs into one vector
Cat_01_Levels <- c(
  "Meat", "Milk", "Other animal products", "Fish",
  "Other processed", "Cakes", "Oils",
  "Other plant products", "Fruits and vegetables", "Oilseeds", "Grains"
)
Cat_01_colours <- c(
  Meat_colour, Milk_colour, Other_animal_products_colour, Fish_colour,
  Other_processed_colour, Cakes_colour, Oils_colour,
  Other_plant_products_colour, Fruits_and_vegetables_colour, Oilseeds_colour, Grains_colour
)

Cat_diet_agg_Levels <- c(
  "Meat", "Milk", "Other animal", "Fish and seafood",
  "Other vegetal", "Sugar and alcohol", "Oil",
  "Treenuts", "Fruits", "Vegetables", "Roots and tubers", "Legumes", "Cereals"
)
Cat_diet_agg_colours <- c(
  Meat_colour, Milk_colour, Other_animal_colour, Fish_seafood_colour,
  Other_vegetal_colour, Sugar_alcohol_colour, Oil_colour,
  Treenuts_colour, Fruits_colour, Vegetables_colour, Roots_and_tubers_colour, Legumes_colour, Cereals_colour
)

Cat_01_Levels_prod <- c(
  "Meat", "Milk", "Other animal products", "Fish",
  "Other plant products", "Fruits and vegetables", "Oilseeds", "Grains"
)

Cat_01_colours_prod <- c(
  Meat_colour, Milk_colour, Other_animal_products_colour, Fish_colour,
  Other_plant_products_colour, Fruits_and_vegetables_colour, Oilseeds_colour, Grains_colour
)

# Miscelaneous levels -----

Livestock_Species <- c(
  "Cattle",
  "Sheep",
  "Goats",
  "Pigs",
  "Horses",
  "Donkeys_mules",
  "Poultry",
  "Rabbits"
)
LandUse_levels <- c(
  "Unproductive",
  "Forest_high",
  "Forest_low",
  "Pasture_Shrubland",
  "Dehesa",
  "Fallow",
  "Harvested"
)

Synthetic_ItemName_levels <- c(
  "NPK",
  "U",
  "CAN",
  "AN",
  "AS",
  "Saltpeter",
  "Guano"
)
Synthetic_ItemName_items <- c(
  "Other",
  Synthetic_ItemName_levels
)
Synthetic_ItemName_tot <- c(
  Synthetic_ItemName_levels,
  "N fertilizers"
)
FertilizersGWP_levels <- c(
  "Upstream emissions",
  "N2O indirect NO3",
  "N2O indirect NH3",
  "N2O direct",
  "Total fertilizer emissions"
)

Function_levels <- c(
  "Total",
  "Traction",
  "Fertilization",
  "Irrigation",
  "Protection",
  "Land",
  "Carbon cycle"
)

Continents_levels <- c("RoW", "Africa", "Asia and Pacific", "America Other", "North America", "Europe")

Impact_levels <- c("Carbon cycle", "Fertilization", "Irrigation", "Protection", "Traction", "Total")
Input_cats <- c(
  "Traction",
  "Fertilization",
  "Irrigation",
  "Protection"
)
Input_cats_tot <- c(
  "Traction",
  "Fertilization",
  "Irrigation",
  "Protection",
  "Total"
)


# NPP categories
NPP_comp <- c("Prod_MgC", "Burn_MgC", "Used_MgC", "Unharvested_MgC", "Root_MgC", "Weeds_AG_MgC", "Weeds_BG_MgC")
NPP_names <- c("Production", "Burnt residues", "Used residues", "Unharvested residues", "Crop belowground", "Weeds aboveground", "Weeds belowground")
NPP_cat <- tibble::tibble(
  NPP_comp,
  NPP_names
) |>
  dplyr::mutate(NPP_names = factor(NPP_names,
    levels = c(
      "Weeds belowground",
      "Crop belowground",
      "Weeds aboveground",
      "Unharvested residues",
      "Burnt residues",
      "Used residues",
      "Production"
    )
  ))
# C input categories
C_input_comp <- c("Residue", "Root", "Weeds_AG", "Weeds_BG", "Agro_industry", "Manure", "Urban")
C_input_names <- c("Unharvested residues", "Crop belowground", "Weeds aboveground", "Weeds belowground", "Agro-industry", "Manure", "Urban")
C_input_cat <- tibble::tibble(
  C_input_comp,
  C_input_names
) |>
  dplyr::rename(
    Fert_type = C_input_comp,
    C_input_name = C_input_names
  ) |>
  dplyr::mutate(C_input_names = factor(C_input_name,
    levels = c(
      "Urban",
      "Manure",
      "Agro-industry",
      "Weeds belowground",
      "Crop belowground",
      "Weeds aboveground",
      "Unharvested residues"
    )
  ))


# Water variables
Water_variables <- c("PET", "AET", "ETc", "WaterInput", "BW_tot", "BW_net", "FloodWater", "PRE", "PREe")

# Setting figure parameters ----


# Make colour codes
YellowRedPalette <- c(
  "white", "yellow", "gold", "orange",
  "orangered", "red", "darkred", "black"
)


Synthetic_ItemName_itemsshort_colours <- c(
  "firebrick2",
  "darksalmon",
  "darkred",
  "indianred",
  "indianred1",
  "lightgoldenrod",
  "lightgoldenrod4"
)
Synthetic_ItemName_items_colours <- c(
  "darkgrey",
  Synthetic_ItemName_itemsshort_colours
)
Synthetic_ItemName_all_colours <- c(
  "darkgrey",
  Synthetic_ItemName_itemsshort_colours,
  "black"
)
Synthetic_ItemName_tot_colours <- c(
  Synthetic_ItemName_itemsshort_colours,
  "black"
)
FertilizersGWP_colours <- c(
  "firebrick",
  "azure3",
  "darkolivegreen2",
  "chartreuse4",
  "black"
)


ResidueWeed_colours <- c(
  "gray50",
  "gray80",
  "palegreen3"
)

NPP_colours <- c(
  ResidueWeed_colours,
  "orange2",
  "goldenrod1",
  "steelblue1",
  "steelblue"
)

C_input_colours <- c(
  "darkorange4",
  "darkorange3",
  "plum3",
  ResidueWeed_colours,
  "goldenrod1"
)

N_bal_levels <- c("Unexplained", "Gaseous_losses", "NO3", "NH3", "Burning", "Accumulated", "Net_SOM", "Grazed_weeds", "Residue", "Production", "Synthetic", "Urban", "Manure", "Excreta", "Agro_industry", "BNF", "Deposition")
N_bal_colours <- c(
  Unexplained_colour,
  Gaseous_colour,
  NO3_colour,
  NH3_colour,
  Burning_colour,
  Accumulated_colour,
  Net_SOM_colour,
  Grazed_weeds_colour,
  Grazed_weeds_colour,
  Residue_colour,
  Production_colour,
  Synthetic_colour,
  Urban_colour,
  Manure_colour,
  Excreta_colour,
  Agro_industry_colour,
  BNF_colour,
  Deposition_colour
)

Water_colours <- c("coral4", "darkorange3", "darkorange", "black", "darkblue", "cornflowerblue", "cadetblue3", "darkolivegreen4", "darkolivegreen3")

Impact_colours <- c("bisque3", "darkolivegreen4", "cadetblue2", "brown", "darkorchid1", "azure4", "black")

Continent_colours <- c("gray", "chocolate2", "darkolivegreen4", "chocolate4", "cadetblue3", "dodgerblue4")

# GL levels and colours ----
Animal_color <- "coral3"
Sea_color <- "cornflowerblue"
Animal_colour <- c("Animal" = Animal_color)
Sea_colour <- c("Sea" = Sea_color)
LU_simple_levels <- c("Animal", "Cropland", "Cropland SOC", "Non-Cropland", "Non-Cropland SOC", "LULUCF", "Industry", "Transport", "Sea")
LU_type_levels <- c("Animal", "Cropland", "Non-Cropland", "Sea")
LU_type_colours <- c(Animal_colour, Cropland_colour, Non_cropland_colour, Sea_colour)
Colours_AniCroGra <- c("coral3", "darkkhaki", "darkolivegreen4")
Colours_GL <- c("coral3", "darkkhaki", "darkgoldenrod2", "darkolivegreen4", "darkgoldenrod", "chartreuse3", "azure3", "azure4", "cornflowerblue")
Colours_NRE_GL <- c("coral3", "darkolivegreen4", "darkkhaki", "azure3", "azure4", "cornflowerblue")
Colours_Land_GL <- c("darkkhaki", "darkolivegreen4", "azure3")

# SJOS Levels and colours ----
Nour_levels <- c("Over", "Adequate", "Under")

SJOS_levels <- rev(c(
  "Within_boundary Under",
  "Within_boundary Adequate",
  "Within_boundary Over",
  "Exceedance Under",
  "Exceedance Adequate",
  "Exceedance Over"
))

SJOS_colours <- c(
  "Within_boundary Under" = "lightseagreen",
  "Within_boundary Adequate" = "lightgreen",
  "Within_boundary Over" = "burlywood3",
  "Exceedance Under" = "mediumpurple",
  "Exceedance Adequate" = "salmon1",
  "Exceedance Over" = "indianred3"
)

Nourish_colours <- c(
  "Under" = "blue",
  "Adequate" = "green",
  "Over" = "red"
)

Nourish_colours_tot <- c(Nourish_colours,
  "Total" = "black"
)

Nourish_levels <- c(
  "Under",
  "Adequate",
  "Over"
)

Nourish_levels_tot <- c(
  Nourish_levels,
  "Total"
)
