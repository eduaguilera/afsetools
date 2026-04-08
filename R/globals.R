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
  "Residue_ratio_factor", "Irr_residue_sensitivity", "HI_correction_factor",
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
  "Root_kgDM_kgFM",

  # --- npp_functions.R: integrate_fallow(), calculate_land_scaling() ---
  "Area_Fallow", "Area_herb", "Area_share", "Dyn_HI", "Dyn_RS",
  "Land_scaling", "Land_tot", "land_scaling",

  # --- npp_functions.R: residues_as_items() ---
  "Residue_Mg",

  # --- npp_functions.R: data objects loaded by load_general_data ---
  "Crop_RS_N_response", "Fallow_cover", "HI_crop_ranges",
  "IPCC_crop_mapping", "IPCC_residue_coefs", "IPCC_root_coefs",
  "Irr_residue_crop_adj", "Irrigation_adj", "Modern_variety_adoption",
  "NPP_model_coefs", "N_input_RS_adj", "Names_biomass",
  "Residue_Shares", "Residue_kgC_kgDM_Wo", "Weed_NPP_Scaling",
  "Residue_humified_kgC_kgC", "Rhizodeposits_mass_kgC_kgDM",
  "Root_humified_kgC_kgC", "Root_mass_kgC_kgDM",

  # --- npp_functions.R: calculate_crop_npp (simple mode) ---
  "Root_ref", "FM_Mg",

  # --- calculate_footprints.R ---
  "CBS", "Primary_all", "Impact_prod", "Feed_intake",
  "Primary_prices", "CBS_item_prices", "Processing_coefs",
  "Relative_residue_price", "Primary_double",
  "items_full", "items_prod_full", "Animals_codes", "Biomass_coefs",
  "Element", "Value", "Seed", "Production", "area", "area_code",
  "item_cbs", "item_code_cbs", "unit", "item_prod", "item_code_prod",
  "Live_anim", "Live_anim_code", "LU", "MgN", "N_yield", "Win",
  "min_yield", "Yield_share", "Yield_share_excess", "draught_share",
  "Item_code_area", "Multi_type", "item_code_impact",
  "Impact", "Origin", "FU", "u_FU", "Product_residue",
  "Impact_u", "Impact_u_proc", "Value_d", "Price", "Price_cbs",
  "Value_cbs", "Alloc_raw1", "Alloc_raw2", "Alloc_cbs", "Allocation",
  "n", "Alloc",
  "Item", "tons_proc", "group",
  "Export", "Export_u", "Import",
  "FPFeed_u", "Supply", "Intake_DM",
  "area_code_p", "area_p", "Country_share",
  "Value_ds", "u_ton", "u_ton2", "u_ton_glob", "u_ton_p",
  "cf",
  "Seed_share", "Prod_class", "Consumption_u", "Exp_share",
  "u_ton_scaled", "DM_origin", "DM_LiveAnim", "Origin_share_DM",
  "Value_proc", "Proc_share", "Impact_Mu", "Elem_share", "i_ton",
  "Crop_NPPr_NoFallow", "Yield_N", "Cat_proc",
  "Value_tot", "Item_area",

  # --- impact_functions.R ---
  "Availability",

  # --- analysis_functions.R ---
  "Calcium_mg", "Calcium_mg_kgFM", "Carbohydrates_g", "Carbohydrates_g_kgFM",
  "Cat_0", "Cat_1", "Cat_1_items", "Cat_1_lookup", "Cat_FAO1", "Cat_FAO12",
  "Cat_Labour", "Cat_Labour2", "Cat_animal", "Cat_diet_agg",
  "Cat_feed", "Cat_proc", "Cats",
  "Destiny", "DM_Mg", "Edible_portion",
  "Energy_TJ", "Energy_kcal", "Farm_class", "Farm_class2",
  "GE_product_MJ_kgFM", "GWP", "GWP_100", "Gas", "Gas_raw", "Gas_type",
  "Herb_Woody", "Kcal_MJ", "LandUse", "LandUse_FAO",
  "Lipids_g", "Lipids_g_kgFM",
  "N_MgN", "Name", "Names_cats", "Order",
  "Pop_Mpeop", "Protein_N", "Protein_g",
  "Residue_C_N", "Unit", "VitaminA_microg", "VitaminA_microg_kgFM",
  "Yield", "Yield_DM", "code", "ha", "value",
  "Land_HerbWoody", "Land_Primary", "RowName",

  # --- analysis_functions.R: FillingProxy ---
  "Proxy_ratio", "Value_CarriedBackward", "Value_CarriedForward",
  "Value_interpfilled",

  # --- utility_functions.R: Arrange_dates ---
  "Month_numbers", "Month_names", "Month_number", "Month_order", "Date",

  # --- utility_functions.R: harmonize_countries ---
  "regions_full", "polity_code", "polity_name", "uISO3c",

  # --- bnf_functions.R ---
  "BGN", "BNF", "CropBNF", "CropBNF2", "Crop_NPP_MgN",
  "Leguminous_share", "NHI", "NSBNF", "Ndfa", "Ndfa_adj",
  "Name_BNF", "Names_BNF", "NSBNF_base_kgha",
  "Legs_Seeded", "Legs_SpontWeeds", "Legs_SpontWeeds_LU", "Seeded_CC_share",
  "WeedsBNF", "Weeds_Ndfa", "Weeds_Ndfa_ref", "Weeds_leg_share",
  "Weeds_NPP_MgN",
  "kgNha", "N_org_kgha", "N_synth_kgha", "PET_mm",
  "f_N_symb", "f_temp_symb", "f_water_symb", "f_env_symb",
  "f_N_ns", "f_temp_ns", "f_water_ns", "f_SOM_ns", "f_clay_ns",
  "f_pH_ns", "f_env_ns", "f_env_weed",
  "clay_pct", "SOM_pct", "soil_pH",
  "total_BNF_MgN", "total_CropBNF_MgN", "total_WeedsBNF_MgN",
  "total_NSBNF_MgN",

  # --- feed_distribution_functions.R ---
  ":=",
  "Avail_MgDM", "Avail_edible", "Cat_leg",
  "Feed_scale", "comm_group",
  "demand_MgDM", "demand_id", "avail_id", "avail_nat", "avail_prov",
  "avail_id_nat", "avail_id_prov",
  "intake_MgDM", "remaining",
  "Livestock_cat", "Province_name", "Region_name",
  "alloc", "allocated_so_far", "avail_group", "avail_limit",
  "avail_remaining", "avail_share",
  "capped_intake", "cumsum_intake",
  "demand_group", "demand_share",
  "excess", "feed_rank", "feedtype_graniv",
  "fixed_demand", "hierarchy_level", "is_monogastric",
  "item_dm", "key", "limit_dm", "msg",
  "original_Province", "original_demand_item",
  "priority_value", "reduction_factor", "remaining_cap",
  "scale_factor", "scaling_factor", "share",
  "total", "total_demand", "total_dm", "total_intake",
  "trade_origin", "used", "weight", "zoot_scale_factor",

  # --- extract_luh2.R ---
  "Area.Code", "Area_Mha", "C_input_name", "C_stock_Tg",
  "Item.Code", "Territory",

  # --- load_data.R ---
  ".data"
))
