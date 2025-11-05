#' Calculate Potential Net Primary Production (NPP)
#'
#' Calculates potential NPP using various models including Miami, NCEAS, and Rosenzweig.
#' This function estimates ecosystem productivity based on temperature and precipitation.
#'
#' @param Dataset A data frame containing climate data with columns: TMP (temperature), MAP (precipitation), PET (potential evapotranspiration), AET (actual evapotranspiration)
#'
#' @return A data frame with calculated NPP values from different models in MgDM/ha
#' @export
#'
#' @examples
#' \dontrun{
#' climate_data <- data.frame(TMP = 15, MAP = 800, PET = 1000, AET = 700)
#' npp_results <- Calc_NPP_potentials(climate_data)
#' }
Calc_NPP_potentials <- function(Dataset) {
  Dataset |>
    dplyr::mutate(
      WaterInput_mm = MAP - PET + AET, # The water input is the sum of precipitation and soil moisture difference
      NPPT_Miami_MgDMha = 30 / (1 + exp(1.315 - (0.119 * TMP))) / (100 * Residue_kgC_kgDM_W), # Miami model, from Lieth (1975)
      NPPP_Miami_MgDMha = 30 * (1 - exp(-6.64 * 10^-4 * MAP)) / (100 * Residue_kgC_kgDM_W), # Miami model, from Lieth (1975)
      FMAP_TNPP_NCEAS_MgDMha = ((0.2163 * WaterInput_mm^1.125) / exp(0.000319 * WaterInput_mm)) / (100 * Residue_kgC_kgDM_Wo),
      FMAT_TNPP_NCEAS_MgDMha = 2540 / (1 + exp(1.584 - 0.0622 * TMP)) / (100 * Residue_kgC_kgDM_Wo),
      FMAP_ANPP_NCEAS_MgDMha = ((0.1665 * WaterInput_mm^1.185) / exp(0.000414 * WaterInput_mm)) / (100 * Residue_kgC_kgDM_Wo),
      FMAT_ANPP_NCEAS_MgDMha = 3139 / (1 + exp(2.2 - 0.0307 * TMP)) / (100 * Residue_kgC_kgDM_Wo),
      TNPP_NCEAS_MgDMha = 6116 * (1 - exp(-6.05 * 10^-5 * WaterInput_mm)) / (100 * Residue_kgC_kgDM_W), # NCEAS model for total non-tree vegetation, from Del Grosso et al. (2008)
      ANPP_NCEAS_MgDMha = 4000 * (1 - exp(-4.77 * 10^-5 * WaterInput_mm)) / (100 * Residue_kgC_kgDM_W), # NCEAS model for aboveground non-tree vegetation, from Del Grosso et al. (2008)
      TNPP_tree_NCEAS_MgDMha = pmin(FMAP_TNPP_NCEAS_MgDMha, FMAT_TNPP_NCEAS_MgDMha), # NCEAS model for total vegetation with trees
      ANPP_tree_NCEAS_MgDMha = pmin(FMAP_ANPP_NCEAS_MgDMha, FMAT_ANPP_NCEAS_MgDMha), # NCEAS model for aboveground vegetation with trees
      NPP_Miami_MgDMha = pmin(NPPT_Miami_MgDMha, NPPP_Miami_MgDMha), # Miami model, from Lieth (1975)
      NPP_Rosenzweig_MgDMha = 10^(1.66 * log10(AET_mm) - 1.66) / 100, # Rosenzweig (1968), based on AET
      RS_ratio_NCEAS = (TNPP_NCEAS_MgDMha - ANPP_NCEAS_MgDMha) / ANPP_NCEAS_MgDMha,
      RS_ratio_tree_NCEAS = (TNPP_tree_NCEAS_MgDMha - ANPP_tree_NCEAS_MgDMha) / ANPP_tree_NCEAS_MgDMha
    )
}

#' Calculate Crop Net Primary Production Components
#'
#' Calculates the components of crop NPP including product, residue, and root biomass
#' in dry matter units.
#'
#' @param Dataset A data frame with crop area and production data
#' @param HI A data frame with harvest index (HI) values
#' @param ... Additional grouping variables to preserve in output
#'
#' @return A data frame with NPP components (Prod_MgDM, Residue_MgDM, Root_MgDM)
#' @export
#'
#' @examples
#' \dontrun{
#' crop_npp <- Calculate_crop_NPP(crop_data, harvest_index)
#' }
Calculate_crop_NPP <- function(Dataset, HI, ...) {
  Dataset |> # Crop area and production
    dplyr::left_join(Biomass_coefs, by = c("Name_biomass")) |>
    dplyr::left_join(Root_ref, by = c("Name_biomass")) |>
    dplyr::left_join(HI, by = c("Year", "Name_biomass")) |>
    dplyr::mutate(
      Prod_ygpit_Mg = dplyr::if_else(is.na(Prod_ygpit_Mg),
        0,
        as.numeric(as.character(Prod_ygpit_Mg))
      ),
      Prod_MgDM = Prod_ygpit_Mg * Product_kgDM_kgFM,
      Residue_MgFM = dplyr::if_else(is.na(Dyn_HI),
        Prod_ygpit_Mg * kg_residue_kg_product_FM,
        Prod_ygpit_Mg * Dyn_HI
      ),
      Residue_MgDM = Residue_MgFM * Residue_kgDM_kgFM,
      Aerial_MgDM = Prod_MgDM + Residue_MgDM,
      Root_MgDM_RS = dplyr::if_else(is.na(Dyn_RS),
        Aerial_MgDM * Root_Shoot_ratio,
        Aerial_MgDM * Dyn_RS
      ),
      Root_MgDM_ref = Root_MghaDM_ref * Area_ygpit_ha,
      Root_MgDM = (Root_MgDM_RS + Root_MgDM_ref) * 0.5, # Crop root estimated as the mean between RS approach and fixed biomass approach
      Root_MgDM = dplyr::if_else(Root_MgDM / Aerial_MgDM < Root_Shoot_ratio * 3, # Limit to 3x the reference R:S ratio
        Root_MgDM,
        Aerial_MgDM * Root_Shoot_ratio * 3
      ),
      Residue_MgDM = tidyr::replace_na(Residue_MgDM, 0), # Set NA generated in fallow to zero
      Root_MgDM = tidyr::replace_na(Root_MgDM, 0), # Set NA generated in fallow to zero
      Crop_NPP_MgDM = Prod_MgDM + Residue_MgDM + Root_MgDM
    ) |>
    dplyr::select(
      ..., Year, Name_biomass, Province_name, Irrig_cat, Irrig_type, # Add scenario grouping if needed
      Area_ygpit_ha, Prod_ygpit_Mg, Yield_ygpi_Mgha,
      Prod_MgDM, Residue_MgDM, Root_MgDM
    )
}

#' Calculate NPP in Dry Matter, Carbon, and Nitrogen
#'
#' Converts NPP components to dry matter, carbon, and nitrogen units,
#' including crop and weed biomass.
#'
#' @param AreaNPP A data frame with NPP components and biomass coefficients
#'
#' @return A data frame with NPP expressed in DM, C, and N for all components
#' @export
#'
#' @examples
#' \dontrun{
#' npp_nutrients <- Calc_NPP_DM_C_N(area_npp_data)
#' }
Calc_NPP_DM_C_N <- function(AreaNPP) {
  AreaNPP |>
    dplyr::mutate(
      Weeds_BG_MgDM = Weeds_AG_MgDM * Root_Shoot_ratio_W,
      Weeds_NPP_MgDM = Weeds_AG_MgDM + Weeds_BG_MgDM,
      Crop_NPP_MgDM = Prod_MgDM + Residue_MgDM + Root_MgDM,
      Tot_NPP_MgDM = Crop_NPP_MgDM + Weeds_NPP_MgDM,
      Prod_MgN = Prod_MgDM * Product_kgN_kgDM,
      Residue_MgFM = Residue_MgDM / Residue_kgDM_kgFM,
      Used_Residue_MgFM = Residue_MgFM * Use_Share,
      Residue_MgN = Residue_MgDM * Residue_kgN_kgDM,
      Root_MgN = (Root_MgDM * Root_kgN_kgDM) + (Root_MgDM * Root_kgN_kgDM * Rhizodeposits_N_kgN_kgRootN), # Includes rhizodeposits
      Crop_NPP_MgN = Prod_MgN + Residue_MgN + Root_MgN,
      Weeds_AG_MgN = Weeds_AG_MgDM * Residue_kgN_kgDM_W,
      Weeds_BG_MgN = (Weeds_BG_MgDM * Root_kgN_kgDM_W) + (Weeds_BG_MgDM * Root_kgN_kgDM_W * Rhizod_kgN_kgRootN_W), # Includes rhizodeposits
      Weeds_NPP_MgN = Weeds_AG_MgN + Weeds_BG_MgN,
      Tot_NPP_MgN = Crop_NPP_MgN + Weeds_NPP_MgN,
      Residue_soil_MgN = Residue_MgN * Soil_Share,
      Prod_MgC = Prod_MgDM * Product_kgC_kgDM,
      Residue_MgC = Residue_MgDM * Residue_kgC_kgDM,
      Root_MgC = Root_MgDM * Root_kgC_kgDM, # Includes rhizodeposits
      Crop_NPP_MgC = Prod_MgC + Residue_MgC + Root_MgC,
      Weeds_AG_MgC = Weeds_AG_MgDM * Residue_kgC_kgDM_W,
      Weeds_BG_MgC = Weeds_BG_MgDM * Root_kgC_kgDM_W, # Includes rhizodeposits
      Weeds_NPP_MgC = Weeds_AG_MgC + Weeds_BG_MgC,
      Tot_NPP_MgC = Crop_NPP_MgC + Weeds_NPP_MgC,
      Residue_soil_MgC = Residue_MgC * Soil_Share
    )
}

#' Calculate Cropland NPP Components Including Weeds
#'
#' Calculates complete cropland NPP including weed biomass, scaled by potential NPP
#' and considering fallow periods.
#'
#' @param Crop_NPPpot A data frame with potential NPP values and crop data
#'
#' @return A data frame with complete cropland NPP components (crop + weeds) in DM, C, N
#' @export
#'
#' @examples
#' \dontrun{
#' cropland_npp <- Calc_CropNPP_components(crop_npp_potential)
#' }
Calc_CropNPP_components <- function(Crop_NPPpot) {
  Crop_NPPpot |>
    dplyr::left_join(Biomass_coefs |>
      dplyr::select(-Category, -Code, -Equiv), by = c("Name_biomass")) |>
    dplyr::left_join(Weed_NPP_Scaling, by = c("Year", "Name_biomass")) |>
    dplyr::left_join(Residue_Shares, by = c("Year", "Name_biomass")) |>
    dplyr::left_join(Fallow_cover, by = c("Year", "Name_biomass")) |>
    dplyr::group_by(Year) |>
    dplyr::mutate(Scaling_weeds = dplyr::if_else(is.na(Scaling_weeds),
      mean(Scaling_weeds, na.rm = TRUE),
      Scaling_weeds
    )) |>
    base::replace(is.na(.), 0) |>
    dplyr::mutate(Weeds_AG_MgDM = dplyr::if_else(Name_biomass != "Fallow",
      Area_ygpit_ha * Scaling_weeds * NPPpot_MgDMha / (1 + Root_Shoot_ratio_W),
      Area_ygpit_ha * Fallow_cover_share * NPPpot_MgDMha / (1 + Root_Shoot_ratio_W)
    )) |>
    dplyr::ungroup() |>
    Calc_NPP_DM_C_N()
}
