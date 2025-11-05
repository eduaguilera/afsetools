#' Biological Nitrogen Fixation Functions
#'
#' Calculate biological nitrogen fixation from crops, weeds, and non-symbiotic sources.
#'
#' @param x A data frame with crop NPP data including columns: Crop_NPP_MgN, Prod_MgN, Weeds_NPP_MgN, LandUse, Area_ygpit_ha
#'
#' @return A data frame with BNF calculations: CropBNF, WeedsBNF, NSBNF, and total BNF
#' @export
#'
#' @examples
#' \dontrun{
#' bnf_results <- Calc_N_fix(npp_data)
#' }
Calc_N_fix <- function(x) {
  x |>
    dplyr::left_join(Names_BNF |>
      dplyr::left_join(BNF, by = c("Name_BNF")), by = c("Name_biomass" = "Name_BNF")) |>
    dplyr::mutate(
      Fert_type = "BNF",
      CropBNF = dplyr::if_else(!is.na(Ndfa), # Crop BNF using estimation of N in crop NPP
        Crop_NPP_MgN * Ndfa * Leguminous_share,
        0
      ),
      CropBNF2 = dplyr::if_else(!is.na(Ndfa), # Crop BNF using BGN and NHI from Anglade et al. and Lassaletta et al.
        Prod_MgN * Leguminous_share * Ndfa * BGN / NHI,
        0
      ),
      Alpha1 = CropBNF / Prod_MgN,
      Alpha2 = CropBNF2 / Prod_MgN,
      Weeds_Ndfa = as.numeric(BNF |>
        dplyr::filter(Name_BNF == "Weeds") |>
        dplyr::select(Ndfa)),
      Legs_SpontWeeds = as.numeric(BNF |>
        dplyr::filter(Name_BNF == "Weeds") |>
        dplyr::select(Leguminous_share)),
      Legs_Seeded = tidyr::replace_na(Legs_Seeded, 0),
      Seeded_CC_share = dplyr::if_else(LandUse == "Cropland",
        Seeded_CC_share,
        0
      ),
      Weeds_leg_share = (Legs_SpontWeeds * (1 - Seeded_CC_share)) + (Legs_Seeded * Seeded_CC_share), # Weighted average of legume share in spontaneous weeds and seeded cover crops
      WeedsBNF = Weeds_NPP_MgN * Weeds_Ndfa * Weeds_leg_share,
      WeedsBNF = tidyr::replace_na(WeedsBNF, 0),
      NSBNF = dplyr::if_else(!is.na(kgNha), # Non-symbiotic BNF
        kgNha * Area_ygpit_ha / 1000,
        13 * Area_ygpit_ha / (2 * 1000)
      ), # 13 value from wheat and maize in Ladha et al. (2016), DIVIDED BY 2 for conservativeness
      BNF = CropBNF + WeedsBNF + NSBNF
    )
}

#' Classify GHG Emissions and Calculate Global Warming Potential
#'
#' Classifies greenhouse gas emissions and calculates GWP100 CO2 equivalents.
#'
#' @param x A data frame with columns: Gas_raw, Gas_type, value
#'
#' @return A data frame with Gas, Gas_categ, and CO2e_Tg calculated
#' @export
#'
#' @examples
#' \dontrun{
#' ghg_with_gwp <- Gases_GWP(emissions_data)
#' }
Gases_GWP <- function(x) {
  x |>
    dplyr::mutate(
      Gas = dplyr::if_else(Gas_raw == "CH4" & Gas_type == "Fossil",
        "CH4_fossil",
        Gas_raw
      ),
      Gas_categ = dplyr::if_else(Gas == "F_gases",
        "F gases",
        paste(Gas_raw, Gas_type, sep = " ")
      )
    ) |>
    dplyr::left_join(GWP |>
      dplyr::select(Gas, GWP_100), by = c("Gas")) |>
    dplyr::mutate(CO2e_Tg = dplyr::if_else(is.na(GWP_100),
      value / 1000,
      value * GWP_100 / 1000
    ))
}

#' Calculate Nutrient Composition of Diets
#'
#' Calculates the nutrient composition of diets including energy, protein, lipids,
#' carbohydrates, calcium, and vitamin A.
#'
#' @param PIE_dest_df A data frame with food destiny data (columns: Year, area, item_cbs, Element, Destiny, FM_Mg)
#' @param Pop A data frame with population data (columns: Year, area, Pop_Mpeop)
#'
#' @return A data frame with dietary nutrient availability per capita per day
#' @export
#'
#' @examples
#' \dontrun{
#' diet_nutrients <- Calc_diets(food_destiny_data, population_data)
#' }
Calc_diets <- function(PIE_dest_df, Pop) {
  PIE_dest_df |>
    dplyr::left_join(items_full |>
      dplyr::select(item_cbs, Name_biomass, Cat_1), by = c("item_cbs")) |>
    dplyr::left_join(Biomass_coefs |>
      dplyr::select(
        Name_biomass, Product_kgDM_kgFM, Product_kgN_kgDM,
        GE_product_MJ_kgFM, Edible_portion, Lipids_g_kgFM,
        Carbohydrates_g_kgFM, Calcium_mg_kgFM, VitaminA_microg_kgFM
      ), by = c("Name_biomass")) |>
    dplyr::left_join(Pop, by = c("Year", "area")) |>
    dplyr::mutate(
      DM_Mg = FM_Mg * Product_kgDM_kgFM,
      Energy_TJ = FM_Mg * GE_product_MJ_kgFM,
      Energy_kcal = Energy_TJ * Kcal_MJ / 1000,
      N_MgN = DM_Mg * Product_kgN_kgDM,
      Protein_g = N_MgN * Protein_N,
      Lipids_g = FM_Mg * Lipids_g_kgFM / 1000,
      Carbohydrates_g = FM_Mg * Carbohydrates_g_kgFM / 1000,
      Calcium_mg = FM_Mg * Calcium_mg_kgFM / 1000,
      VitaminA_microg = FM_Mg * VitaminA_microg_kgFM / 1000
    ) |>
    dplyr::select(
      Year, area, item_cbs, Element, Destiny, Name_biomass, Cat_1, Pop_Mpeop, Edible_portion, DM_Mg, N_MgN, Energy_TJ,
      FM_Mg, Energy_kcal, Protein_g, Lipids_g, Carbohydrates_g, Calcium_mg, VitaminA_microg
    ) |>
    tidyr::pivot_longer(DM_Mg:VitaminA_microg,
      names_to = "Variable",
      values_to = "Availability"
    ) |>
    dplyr::mutate(Avail_edible = Availability * Edible_portion) |>
    tidyr::pivot_longer(Availability:Avail_edible,
      values_to = "Value_tot"
    ) |>
    dplyr::mutate(Value_cap_day = Value_tot * 1000 / (Pop_Mpeop * 365)) |>
    dplyr::left_join(Cats |>
      dplyr::select(Cat_1, Cat_0, Cat_diet_agg, Cat_animal), by = c("Cat_1"))
}

#' Get Herbaceous and Woody Land from FAO
#'
#' Extracts herbaceous (arable) and woody (permanent crops) land area from FAO land use data.
#'
#' @return A data frame with columns: Year, area, Herb_Woody, Land_tot
#' @export
#'
#' @examples
#' \dontrun{
#' land_data <- get_herbwoody_fao()
#' }
get_herbwoody_fao <- function() {
  LandUse_FAO |>
    tidyr::pivot_longer(Y1961:Y2022,
      names_to = "Year",
      values_to = "Value"
    ) |>
    dplyr::mutate(Year = as.numeric(gsub("Y", "", Year))) |>
    dplyr::rename(
      area_code = Area.Code,
      item_code = Item.Code,
      unit = Unit
    ) |>
    filter_areas(Item) |>
    dplyr::filter(
      Element == "Area",
      Item %in% c("Arable land", "Permanent crops")
    ) |>
    dplyr::mutate(Herb_Woody = as.character(dplyr::if_else(Item == "Arable land",
      "Herbaceous",
      "Woody"
    ))) |>
    dplyr::mutate(Land_tot = Value * 1000) |>
    dplyr::select(Year, area, Herb_Woody, Land_tot)
}

#' Calculate Land Scaling Factors
#'
#' Calculates scaling factors to account for cropping intensity (fallow, double cropping, etc.).
#'
#' @return A data frame with columns: Year, area, Herb_Woody, land_scaling
#' @export
#'
#' @examples
#' \dontrun{
#' scaling_factors <- calculate_land_scaling()
#' }
calculate_land_scaling <- function() {
  Primary_all |>
    dplyr::filter(unit == "ha") |>
    dplyr::left_join(items_prod_full |>
      dplyr::select(item_prod, Herb_Woody), by = c("item_prod")) |>
    dplyr::group_by(Year, area, Herb_Woody) |>
    dplyr::summarize(Land_Primary = sum(Value, na.rm = TRUE), .groups = "drop") |>
    dplyr::left_join(Land_HerbWoody, by = c("Year", "area", "Herb_Woody")) |>
    dplyr::mutate(
      land_scaling = Land_tot / Land_Primary,
      land_scaling = dplyr::if_else(is.infinite(land_scaling) | is.na(land_scaling) | is.nan(land_scaling),
        1,
        dplyr::if_else(land_scaling > 4,
          4,
          dplyr::if_else(land_scaling < 0.25,
            0.25,
            land_scaling
          )
        )
      )
    )
}

#' Scale Land Area by Cropping Intensity
#'
#' Applies scaling factors to land area to account for cropping intensity differences.
#'
#' @return A data frame with scaled land impact values
#' @export
#'
#' @examples
#' \dontrun{
#' scaled_land <- scale_land()
#' }
scale_land <- function() {
  Primary_all |>
    dplyr::mutate(Impact = "Land") |> # Preparing land impact dataset
    tidyr::pivot_wider(
      names_from = unit,
      values_from = Value
    ) |>
    dplyr::left_join(items_prod_full |>
      dplyr::select(item_prod, Herb_Woody), by = c("item_prod")) |>
    dplyr::left_join(Land_scaling |>
      dplyr::filter(Herb_Woody == "Herbaceous"), by = c("Year", "area", "Herb_Woody")) |> # Scaling only for herbaceous crops
    dplyr::mutate(
      land_scaling = dplyr::if_else(is.na(land_scaling),
        1,
        land_scaling
      ),
      Value = dplyr::if_else(is.na(ha),
        LU,
        ha * land_scaling
      ),
      u_FU = dplyr::if_else(is.na(ha),
        0,
        1
      )
    ) |>
    dplyr::select(Year, Impact, area, area_code, item_prod, item_code_prod, Value, u_FU)
}
