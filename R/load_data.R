#' Load General Data, Codes, and Coefficients
#'
#' @description
#' Loads all foundational data objects from Excel files in inst/extdata/.
#' This function creates 73 objects in the calling environment including:
#' - 35 code/nomenclature objects from Codes_coefs.xlsx
#' - 3 derived objects (regions_full_uISO3, Names_biomass_cats, items_prim)
#' - 17 biomass coefficient objects from Biomass_coefs.xlsx
#' - 7 GWP objects from GWP.xlsx
#' - 3 BNF objects from BNF.xlsx
#' - 6 miscellaneous coefficient scalars
#'
#' @param path Optional character string with path to data directory.
#'   If NULL (default), uses system.file("extdata", package = "afsetools")
#'
#' @return NULL (loads objects into parent environment)
#'
#' @details
#' Objects loaded from Codes_coefs.xlsx:
#' regions_full, region_krausmann, polities_whep, polities_cats, regions_fabio,
#' Animals_codes, Names_Liv_Prod, CBS_Trade_codes, CB_processing, items_prod_full,
#' CB_processing_tidy, items_full, Names_biomass_CB, Names_cats, Cats,
#' Primary_double, items_proc_fabio, Feedstuff_FEDNA, FishStat_items, Prov,
#' Names_Prov, FAO_Sp, Crops_Eurostat, Names_MAPA, Names_Lab, CB_item,
#' Names_biomass, Biomass_names, Crop_Names, conv_bouwman, conv_krausmann,
#' HI_changes_krausmann, residue_krausmann, Liv_LU_coefs, CBS_cat
#'
#' Derived objects:
#' regions_full_uISO3, Names_biomass_cats, items_prim
#'
#' Biomass coefficient objects:
#' Biomass_coefs, Nutrients_energy, Root_Shoot_ratio_W, Residue_kgDM_kgFM_W,
#' Residue_kgN_kgDM_W, Root_kgN_kgDM_W, Residue_kgC_kgDM_W, Root_kgC_kgDM_W,
#' Rhizod_kgN_kgRootN_W, Residue_kgC_kgDM_Wo, Root_kgC_kgDM_Wo
#'
#' GWP objects:
#' GWP, GWP_C, GWP_CO2, GWP_CH4, GWP_CH4_fossil, GWP_N2ON, GWP_N2O
#'
#' BNF objects:
#' Names_BNF, BNF, Pure_legs
#'
#' Miscellaneous coefficients:
#' toe, IOM, SOM_C, Soil_depth_carbon, Protein_N, Kcal_MJ
#'
#' @examples
#' \dontrun{
#' # Load all data into current environment
#' load_general_data()
#'
#' # Check what was loaded
#' ls()
#' }
#'
#' @export
load_general_data <- function(path = NULL) {
  
  # Set path to data files
  if (is.null(path)) {
    data_path <- system.file("extdata", package = "afsetools")
    if (data_path == "") {
      stop("Cannot find extdata directory. Is the package installed correctly?")
    }
  } else {
    data_path <- path
  }
  
  # Get calling environment
  env <- parent.frame()
  
  # Load codes from Codes_coefs.xlsx ----
  wb <- openxlsx::loadWorkbook(file.path(data_path, "Codes_coefs.xlsx"))
  
  sheet_names <- c(
    "regions_full", "region_krausmann", "polities_whep", "polities_cats", 
    "regions_fabio", "Animals_codes", "Names_Liv_Prod", "CBS_Trade_codes",
    "CB_processing", "items_prod_full", "CB_processing_tidy", "items_full", 
    "Names_biomass_CB", "Names_cats", "Cats", "Primary_double", 
    "items_proc_fabio", "Feedstuff_FEDNA", "FishStat_items", "Prov", 
    "Names_Prov", "FAO_Sp", "Crops_Eurostat", "Names_MAPA", "Names_Lab", 
    "CB_item", "Names_biomass", "Biomass_names", "Crop_Names", "conv_bouwman",
    "conv_krausmann", "HI_changes_krausmann", "residue_krausmann", 
    "Liv_LU_coefs", "CBS_cat"
  )
  
  # Read all sheets and assign to environment
  for (sheet_name in sheet_names) {
    df <- openxlsx::read.xlsx(wb, sheet = sheet_name)
    assign(sheet_name, df, envir = env)
  }
  
  # Create derived objects ----
  
  # regions_full_uISO3: Regions with unique ISO3 codes
  regions_full <- get("regions_full", envir = env)
  regions_full_uISO3 <- regions_full |>
    dplyr::filter(uISO3c == 1)
  assign("regions_full_uISO3", regions_full_uISO3, envir = env)
  
  # Names_biomass_cats: Biomass names with categorizations
  Names_biomass_CB <- get("Names_biomass_CB", envir = env)
  items_full <- get("items_full", envir = env)
  Names_biomass <- get("Names_biomass", envir = env)
  Names_cats <- get("Names_cats", envir = env)
  
  Names_biomass_cats <- Names_biomass_CB |>
    dplyr::select(Name_biomass, item_cbs) |>
    dplyr::left_join(items_full |> dplyr::select(-Name_biomass), 
                     by = "item_cbs") |>
    dplyr::filter(!is.na(Cat_1)) |>
    dplyr::left_join(Names_biomass, by = "Name_biomass") |>
    dplyr::left_join(Names_cats |> dplyr::select(-Cat_1), by = "Name")
  assign("Names_biomass_cats", Names_biomass_cats, envir = env)
  
  # Update items_prod_full with categorization
  items_prod_full <- get("items_prod_full", envir = env)
  items_prod_full <- items_prod_full |>
    dplyr::left_join(Names_cats |> dplyr::select(-Order, -Farm_class), 
                     by = "Name")
  assign("items_prod_full", items_prod_full, envir = env)
  
  # items_prim: Primary items list
  Animals_codes <- get("Animals_codes", envir = env)
  items_prim <- dplyr::bind_rows(
    items_prod_full |>
      dplyr::select(item_prod, item_code_prod, item_cbs, item_code_cbs, 
                    Farm_class, Cat_Labour, Cat_FAO1) |>
      dplyr::filter(!is.na(item_cbs)),
    items_full |>
      dplyr::filter(comm_group == "Live animals") |>
      dplyr::select(item_code_cbs, item_cbs)
  ) |>
    dplyr::left_join(items_full |> dplyr::select(item_code_cbs, group), 
                     by = "item_code_cbs") |>
    dplyr::left_join(
      Animals_codes |>
        dplyr::select(item_code_cbs, Farm_class, Cat_Labour, Cat_FAO1) |>
        dplyr::rename(
          Farm_class2 = Farm_class,
          Cat_Labour2 = Cat_Labour,
          Cat_FAO12 = Cat_FAO1
        ),
      by = "item_code_cbs"
    ) |>
    dplyr::mutate(
      Farm_class = ifelse(is.na(Farm_class), Farm_class2, Farm_class),
      Cat_Labour = ifelse(is.na(Cat_Labour), Cat_Labour2, Cat_Labour),
      Cat_FAO1 = ifelse(is.na(Cat_FAO1), Cat_FAO12, Cat_FAO1)
    ) |>
    dplyr::select(-Farm_class2, -Cat_Labour2, -Cat_FAO12)
  assign("items_prim", items_prim, envir = env)
  
  # Miscellaneous Coefficients ----
  assign("toe", 41.868, envir = env) # Tonne oil equivalent, in GJ/Mg (IEA)
  assign("IOM", 8, envir = env) # Inert Organic Matter (Mg C ha-1)
  assign("SOM_C", 0.58, envir = env) # SOM C content
  assign("Soil_depth_carbon", 0.3, envir = env) # Soil depth for C calculations
  assign("Protein_N", 6.25, envir = env) # Protein to N ratio
  assign("Kcal_MJ", 238.85, envir = env) # Kcal to MJ ratio
  
  # Biomass coefficients from Biomass_coefs.xlsx ----
  Biomass_coefs <- openxlsx::read.xlsx(
    file.path(data_path, "Biomass_coefs.xlsx"),
    sheet = "Coefs",
    startRow = 2
  ) |>
    dplyr::mutate(
      Residue_kgC_kgDM = as.numeric(as.character(Residue_kgC_kgDM)),
      Root_kgC_kgDM = as.numeric(as.character(Root_kgC_kgDM)),
      Residue_humified_kgC_kgC = as.numeric(as.character(Residue_humified_kgC_kgC)),
      Root_humified_kgC_kgC = as.numeric(as.character(Root_humified_kgC_kgC)),
      Root_mass_kgC_kgDM = as.numeric(as.character(Root_mass_kgC_kgDM)),
      Rhizodeposits_mass_kgC_kgDM = as.numeric(as.character(Rhizodeposits_mass_kgC_kgDM)),
      Residue_C_N = as.numeric(as.character(Residue_C_N))
    )
  assign("Biomass_coefs", Biomass_coefs, envir = env)
  
  Nutrients_energy <- openxlsx::read.xlsx(
    file.path(data_path, "Biomass_coefs.xlsx"),
    sheet = "Nutrients_energy",
    startRow = 1
  )
  assign("Nutrients_energy", Nutrients_energy, envir = env)
  
  # Set weed coefficients from grass values
  assign("Root_Shoot_ratio_W", 
         as.numeric(Biomass_coefs |> 
                      dplyr::filter(Name_biomass == "Grass") |> 
                      dplyr::select(Root_Shoot_ratio)),
         envir = env)
  
  assign("Residue_kgDM_kgFM_W",
         as.numeric(Biomass_coefs |> 
                      dplyr::filter(Name_biomass == "Grass") |> 
                      dplyr::select(Residue_kgDM_kgFM)),
         envir = env)
  
  assign("Residue_kgN_kgDM_W",
         as.numeric(Biomass_coefs |> 
                      dplyr::filter(Name_biomass == "Grass") |> 
                      dplyr::select(Residue_kgN_kgDM)),
         envir = env)
  
  assign("Root_kgN_kgDM_W",
         as.numeric(Biomass_coefs |> 
                      dplyr::filter(Name_biomass == "Grass") |> 
                      dplyr::select(Root_kgN_kgDM)),
         envir = env)
  
  assign("Residue_kgC_kgDM_W",
         as.numeric(Biomass_coefs |> 
                      dplyr::filter(Name_biomass == "Grass") |> 
                      dplyr::select(Residue_kgC_kgDM)),
         envir = env)
  
  assign("Root_kgC_kgDM_W",
         as.numeric(Biomass_coefs |> 
                      dplyr::filter(Name_biomass == "Grass") |> 
                      dplyr::select(Root_kgC_kgDM)),
         envir = env)
  
  assign("Rhizod_kgN_kgRootN_W",
         as.numeric(Biomass_coefs |> 
                      dplyr::filter(Name_biomass == "Grass") |> 
                      dplyr::select(Rhizodeposits_N_kgN_kgRootN)),
         envir = env)
  
  assign("Residue_kgC_kgDM_Wo",
         as.numeric(Biomass_coefs |> 
                      dplyr::filter(Name_biomass == "Average wood") |> 
                      dplyr::select(Residue_kgC_kgDM)),
         envir = env)
  
  assign("Root_kgC_kgDM_Wo",
         as.numeric(Biomass_coefs |> 
                      dplyr::filter(Name_biomass == "Average wood") |> 
                      dplyr::select(Root_kgC_kgDM)),
         envir = env)
  
  # GWP coefficients from GWP.xlsx ----
  GWP <- openxlsx::read.xlsx(
    file.path(data_path, "GWP.xlsx"),
    sheet = "GWP",
    startRow = 1
  )
  assign("GWP", GWP, envir = env)
  
  assign("GWP_C", 
         as.numeric(GWP |> dplyr::filter(Gas == "C") |> dplyr::select(GWP_100)),
         envir = env)
  
  assign("GWP_CO2",
         as.numeric(GWP |> dplyr::filter(Gas == "CO2") |> dplyr::select(GWP_100)),
         envir = env)
  
  assign("GWP_CH4",
         as.numeric(GWP |> dplyr::filter(Gas == "CH4") |> dplyr::select(GWP_100)),
         envir = env)
  
  assign("GWP_CH4_fossil",
         as.numeric(GWP |> dplyr::filter(Gas == "CH4_fossil") |> dplyr::select(GWP_100)),
         envir = env)
  
  assign("GWP_N2ON",
         as.numeric(GWP |> dplyr::filter(Gas == "N2ON") |> dplyr::select(GWP_100)),
         envir = env)
  
  assign("GWP_N2O",
         as.numeric(GWP |> dplyr::filter(Gas == "N2O") |> dplyr::select(GWP_100)),
         envir = env)
  
  # BNF parameters from BNF.xlsx ----
  Names_BNF <- openxlsx::read.xlsx(
    file.path(data_path, "BNF.xlsx"),
    sheet = "Names_BNF"
  )
  assign("Names_BNF", Names_BNF, envir = env)
  
  BNF <- openxlsx::read.xlsx(
    file.path(data_path, "BNF.xlsx"),
    sheet = "BNF",
    startRow = 1
  )
  assign("BNF", BNF, envir = env)
  
  Pure_legs <- openxlsx::read.xlsx(
    file.path(data_path, "BNF.xlsx"),
    sheet = "Pure_legs",
    startRow = 1
  )
  assign("Pure_legs", Pure_legs, envir = env)
  
  message("Loaded 73 objects into environment from afsetools data files")
  invisible(NULL)
}
