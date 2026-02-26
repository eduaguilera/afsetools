#' Biomass Processing Functions
#'
#' @description
#' Functions for processing biomass data including fallow integration,
#' residue classification, and residue use calculations.
#'
#' @name biomass_functions
NULL

#' Integrate Fallow Area into Cropland Area
#'
#' @description
#' Integrates the area of fallow land into the area of crops by distributing
#' fallow area proportionally across herbaceous crops based on their area share.
#'
#' @param x Data frame with cropland and fallow area data containing columns:
#'   LandUse, Name_biomass, Area_ygpit_ha
#' @param .by Character vector of column names for area aggregation grouping
#'   (e.g., c("Year", "Region")). If NULL, no grouping is applied.
#'
#' @return Data frame with fallow area integrated into crop areas, excluding
#'   separate fallow entries. Includes Variable = "AreaProd" and Cat_1
#'   classification.
#'
#' @details
#' This function:
#' 1. Filters out non-cropland and fallow entries
#' 2. Identifies herbaceous crops using Names_biomass and Names_cats
#' 3. Calculates area share for each herbaceous crop within groups
#' 4. Distributes fallow area proportionally to herbaceous crops
#' 5. Adds Cat_1 classification from Names_cats
#'
#' Requires these objects from load_general_data():
#' - Names_biomass (with Name_biomass, Name, Herb_Woody columns)
#' - Names_cats (with Name, Cat_1 columns)
#'
#' Used in Crop_AreaNPP.R and Scenarios.R workflows.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' cropland_data |>
#'   integrate_fallow(.by = c("Year", "Region", "Item"))
#' }
integrate_fallow <- function(x,
                             .by = NULL) {
  x |>
    dplyr::filter(
      LandUse == "Cropland",
      Name_biomass != "Fallow"
    ) |>
    dplyr::left_join(Names_biomass |>
                       dplyr::left_join(Names_cats, by = "Name") |>
                       dplyr::select(Name_biomass, Herb_Woody),
                     by = "Name_biomass") |>
    dplyr::mutate(.by = dplyr::all_of(.by),
      Area_herb = dplyr::if_else(Herb_Woody == "Herbaceous",
                                 Area_ygpit_ha,
                                 0
      ),
      Area_share = Area_herb / sum(Area_herb, na.rm = TRUE)
    ) |>
  dplyr::left_join(x |>
             dplyr::filter(Name_biomass == "Fallow") |>
             dplyr::summarize(.by = dplyr::all_of(.by),
                    Area_Fallow = sum(Area_ygpit_ha))) |>
    dplyr::mutate(
      Area_Fallow = dplyr::if_else(is.na(Area_Fallow),
                                   0,
                                   Area_Fallow
      ),
      Area_ygpit_ha = Area_ygpit_ha + (Area_Fallow * Area_share)
    ) |>
    dplyr::select(-Area_Fallow, -Area_share) |>
  dplyr::left_join(Names_biomass |>
             dplyr::select(Name_biomass, Name) |>
             dplyr::left_join(Names_cats |>
                      dplyr::select(Name, Cat_1), by = "Name"),
             by = "Name_biomass") |>
    dplyr::mutate(Variable = "AreaProd")
}

#' Classify Residues as CBS Items
#'
#' @description
#' Reclassifies biomass residues and grazed weeds into standardized
#' Commodity Balance Sheet (CBS) items based on product type and land use.
#'
#' @param df Data frame containing columns: Product_residue, Cat_1, LandUse,
#'   Herb_Woody, item_cbs
#'
#' @return Data frame with item_cbs column updated to classify residues as:
#'   - "Straw" for cereal and pulse residues
#'   - "Firewood" for woody crops or non-cropland herbaceous residues
#'   - "Other crop residues" for other cropland herbaceous residues
#'   - "Grassland" for grazed weeds
#'
#' @details
#' Classification logic:
#' - Residues from Cereals/Pulses → "Straw"
#' - Residues from woody crops or non-cropland → "Firewood"
#' - Other cropland residues → "Other crop residues"
#' - Grazed weeds → "Grassland"
#' - Products retain their original item_cbs value
#'
#' @export
#'
#' @examples
#' \dontrun{
#' biomass_data |>
#'   residues_as_items()
#' }
residues_as_items <- function(df) {
  df |>
    dplyr::mutate(item_cbs = dplyr::if_else(Product_residue == "Residue",
                                            dplyr::if_else(Cat_1 %in% c("Cereals", "Pulses"),
                                                           "Straw",
                                                           dplyr::if_else(LandUse != "Cropland" | Herb_Woody == "Woody",
                                                                          "Firewood",
                                                                          "Other crop residues"
                                                           )
                                            ),
                                            dplyr::if_else(Product_residue == "GrazedWeeds",
                                                           "Grassland",
                                                           as.character(item_cbs)
                                            )
    ))
}

#' Calculate Residue Use from Crop NPP Data
#'
#' @description
#' Converts crop residues into separate items based on use shares,
#' creating both product and residue entries with appropriate classifications.
#'
#' @param df Data frame from Crop_AreaNPP workflow with columns: item_cbs,
#'   Name_biomass, Use_Share, Residue_MgDM, Prod_ygpit_Mg, Area_ygpit_ha,
#'   and other crop NPP variables
#'
#' @return Data frame with separate rows for products and residues, where:
#'   - Product rows have Product_residue = "Product"
#'   - Residue rows have Product_residue = "Residue"
#'   - Residues are converted from DM to FM using Residue_kgDM_kgFM
#'   - Production (Prod_ygpit_Mg) represents used residue mass
#'   - Residues classified using Residues_as_items()
#'
#' @details
#' This function:
#' 1. Preserves original item_cbs as item_cbs_crop
#' 2. Joins Biomass_coefs for DM/FM conversion factors
#' 3. Calculates used residue mass in fresh matter (FM) units
#' 4. Creates residue entries with Product_residue = "Residue"
#' 5. Sets Area_ygpit_ha to NA for residues (not area-based)
#' 6. Binds product and residue rows together
#' 7. Applies residue classification via Residues_as_items()
#'
#' Requires Biomass_coefs object from load_general_data() with:
#' - Name_biomass, Residue_kgDM_kgFM columns
#'
#' Used in Spain_Hist (Crop_AreaNPP.R and Scenarios.R) and Global (Crop_NPP.R)
#' workflows.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' crop_npp_data %>%
#'   residue_use()
#' }
residue_use <- function(df) {
  df <- df |>
    dplyr::mutate(item_cbs_crop = item_cbs)

  df |>
    dplyr::ungroup() |>
    dplyr::left_join(Biomass_coefs |>
                       dplyr::select(Name_biomass, Residue_kgDM_kgFM),
                     by = "Name_biomass") |>
    dplyr::mutate(
      Product_residue = "Residue",
      Residue_Mg = Use_Share * Residue_MgDM / Residue_kgDM_kgFM,
      Area_ygpit_ha = NA_real_
    ) |>
    dplyr::select(-Prod_ygpit_Mg) |>
    dplyr::rename(Prod_ygpit_Mg = Residue_Mg) |>
    dplyr::bind_rows(df |>
                       dplyr::mutate(Product_residue = "Product")) |>
    residues_as_items()
}
