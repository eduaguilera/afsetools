# Global variable declarations to suppress R CMD check "no visible binding" notes
# These are column names used in dplyr/tidyr non-standard evaluation contexts.

utils::globalVariables(c(
  # --- npp_functions.R: calculate_potential_npp() ---
  "AET_mm", "TMP", "WaterInput_mm",
  "NPPT_Miami_MgDMha", "NPPP_Miami_MgDMha", "NPP_Miami_MgDMha",
  "NPP_Rosenzweig_MgDMha",
  "FMAP_TNPP_NCEAS_MgDMha", "FMAT_TNPP_NCEAS_MgDMha",
  "FMAP_ANPP_NCEAS_MgDMha", "FMAT_ANPP_NCEAS_MgDMha",
  "TNPP_NCEAS_MgDMha", "ANPP_NCEAS_MgDMha",
  "TNPP_tree_NCEAS_MgDMha", "ANPP_tree_NCEAS_MgDMha",
  "RS_ratio_NCEAS", "RS_ratio_tree_NCEAS",

  # --- npp_functions.R: calculate_crop_residues() ---
  "Name_biomass", "Prod_ygpit_Mg", "Area_ygpit_ha",
  "Product_kgDM_kgFM", "Residue_kgDM_kgFM", "kg_residue_kg_product_FM",
  "Prod_MgDM", "Yield_DM_Mgha",
  "IPCC_crop", "crop_group", "Slope_AG", "Intercept_AG_MgDMha",
  "Residue_IPCC_Mgha", "Residue_IPCC_MgDM", "Residue_IPCC_adj_MgDM",
  "Residue_ratio_MgFM", "Residue_ratio_MgDM", "Residue_ratio_adj_MgDM",
  "Residue_ratio_factor", "HI_correction_factor",
  "Modern_share", "HI_gap_factor",
  "Water_regime", "Year", "region_HANPP",
  "Residue_MgDM",

  # --- npp_functions.R: calculate_crop_roots() ---
  "Root_Shoot_ratio", "BG_Biomass_kgDM_ha",
  "RS_default", "RS_low_N", "RS_high_N", "RS_irrigated", "RS_rainfed",
  "BG_ref_MgDMha",
  "N_input_kgha", "N_RS_factor", "N_RS_factor_raw", "RS_N_sensitivity",
  "RS_ratio_factor", "RS_base", "RS_effective",
  "Aerial_MgDM", "Root_MgDM", "Root_MgDM_RS", "Root_MgDM_ref",
  "BG_ref_used",

  # --- npp_functions.R: calculate_npp_dm_c_n() ---
  "Weeds_AG_MgDM", "Weeds_BG_MgDM", "Weeds_NPP_MgDM",
  "Crop_NPP_MgDM", "Tot_NPP_MgDM",
  "Product_kgN_kgDM", "Residue_kgN_kgDM", "Root_kgN_kgDM",
  "Rhizodeposits_N_kgN_kgRootN",
  "Prod_MgN", "Residue_MgN", "Root_MgN", "Crop_NPP_MgN",
  "Weeds_AG_MgN", "Weeds_BG_MgN", "Weeds_NPP_MgN", "Tot_NPP_MgN",
  "Product_kgC_kgDM", "Residue_kgC_kgDM", "Root_kgC_kgDM",
  "Prod_MgC", "Residue_MgC", "Root_MgC", "Crop_NPP_MgC",
  "Weeds_AG_MgC", "Weeds_BG_MgC", "Weeds_NPP_MgC", "Tot_NPP_MgC",
  "Root_Shoot_ratio_W",
  "Residue_kgN_kgDM_W", "Root_kgN_kgDM_W", "Rhizod_kgN_kgRootN_W",
  "Residue_kgC_kgDM_W", "Root_kgC_kgDM_W",
  "Residue_MgFM", "Used_Residue_MgFM", "Use_Share",
  "Residue_soil_MgN", "Residue_soil_MgC", "Soil_Share",

  # --- npp_functions.R: calculate_crop_npp_components() ---
  "NPPpot_MgDMha", "Scaling_weeds", "Fallow_cover_share",
  "Root_kgDM_kgFM"
))
