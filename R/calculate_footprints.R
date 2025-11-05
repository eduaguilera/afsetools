#' Calculate Complete Environmental Footprints Along Supply Chains
#'
#' Main workflow function that traces environmental footprints through global supply chains,
#' from primary production through processing, feed, and final products. This function
#' orchestrates the entire footprint calculation process and returns all intermediate
#' and final footprint data frames.
#'
#' @param CBS Commodity Balance Sheets data frame with columns: Year, area, area_code, item, item_code, Element, Value
#' @param Primary_all Primary production data frame with crop areas and production
#' @param Impact_prod Impact data at production level with columns: Year, area_code, item_code, Impact, Value, u_FU
#' @param Crop_NPPr_NoFallow Crop NPP data excluding fallow periods
#' @param DTM Detailed Trade Matrix data (optional, depending on trade_mode)
#' @param trade_mode Character string: "gt" for gross trade or "dtm" for detailed trade matrix. Default is "gt".
#'
#' @return A named list containing all footprint data frames:
#'   \item{FP_prim}{Primary production footprints with economic allocation}
#'   \item{FP_prim_ds}{Primary product footprints including domestic supply}
#'   \item{FP_processed_raw}{Processed product footprints (raw calculation)}
#'   \item{FP_processed_ds}{Processed product footprints with domestic supply}
#'   \item{FP_feed}{Feed product footprints}
#'   \item{FP_feed_ds}{Feed product footprints with domestic supply}
#'   \item{FP_final}{Final comprehensive footprint data frame}
#'   \item{Seed_share}{Calculated seed shares by crop}
#'   \item{draught_shares}{Draught animal allocation shares}
#'
#' @details
#' This function implements a complete footprint accounting system that:
#' \itemize{
#'   \item Calculates seed shares and removes them from production
#'   \item Allocates impacts to co-products using economic allocation
#'   \item Traces impacts through processing chains
#'   \item Accounts for international trade (gross trade or bilateral trade matrix)
#'   \item Handles feed products and livestock production
#'   \item Allocates draught animal services to crop production
#' }
#'
#' The calculation requires that `load_general_data()` has been called first to load
#' all necessary coefficient tables and classification data.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(afsetools)
#' load_general_data()
#'
#' # Calculate footprints using gross trade
#' footprints <- calculate_footprints(
#'   CBS = my_cbs_data,
#'   Primary_all = my_primary_data,
#'   Impact_prod = my_impact_data,
#'   Crop_NPPr_NoFallow = my_npp_data,
#'   trade_mode = "gt"
#' )
#'
#' # Access individual footprint tables
#' fp_primary <- footprints$FP_prim
#' fp_final <- footprints$FP_final
#' }
calculate_footprints <- function(CBS,
                                  Primary_all,
                                  Impact_prod,
                                  Crop_NPPr_NoFallow,
                                  DTM = NULL,
                                  trade_mode = "gt") {
  
  # Select trade calculation method
  calc_avail_fp <- base::switch(trade_mode,
    "gt" = calc_avail_fp_gt,
    "dtm" = calc_avail_fp_dtm,
    base::stop("Error: trade_mode must be 'gt' or 'dtm'")
  )
  
  # Calculate share of seeds over production
  Seed_share <- CBS |>
    dplyr::filter(Element %in% c("Seed", "Production")) |>
    tidyr::pivot_wider(
      names_from = Element,
      values_from = Value,
      values_fill = 0
    ) |>
    dplyr::mutate(
      Seed_share = Seed / Production,
      Seed_share = tidyr::replace_na(Seed_share, 0),
      Seed_share = dplyr::if_else(Seed_share > 0.5, 0.5, Seed_share),
      Seed = Production * Seed_share,
      Seed = tidyr::replace_na(Seed, 0)
    ) |>
    dplyr::select(Year, area, area_code, item, item_code, Seed_share, Seed)
  
  # Remove seeds from CBS
  CBS_NoSeeds <- CBS |>
    dplyr::left_join(Seed_share, by = c("Year", "area", "area_code", "item", "item_code")) |>
    dplyr::mutate(Value = dplyr::if_else(Element %in% c("Production", "Domestic_supply"),
      Value - Seed,
      dplyr::if_else(Element == "Seed", 0, Value)
    )) |>
    dplyr::select(-Seed_share, -Seed)
  
  # Prepare primary production data
  Primary_raw <- Prepare_prim(Primary_all)
  
  # Calculate draught animal shares
  Draught_animals <- c("Buffalo", "Camels", "Cattle, dairy", "Cattle, non-dairy", 
                       "Asses", "Horses", "Mules", "Camelids, other")
  
  draught_shares_all <- Primary_raw |>
    dplyr::filter(unit == "tonnes") |>
    dplyr::inner_join(Primary_all |>
      dplyr::mutate(Live_anim = item) |>
      dplyr::filter(unit == "LU") |>
      dplyr::rename(LU = Value) |>
      dplyr::select(Year, area, Live_anim, LU), by = c("Year", "area", "Live_anim")) |>
    dplyr::left_join(items_prod_full |>
      dplyr::select(item_prod, Name_biomass) |>
      dplyr::rename(item = item_prod), by = c("item")) |>
    dplyr::left_join(Biomass_coefs |>
      dplyr::select(Name_biomass, Product_kgDM_kgFM, Product_kgN_kgDM), by = c("Name_biomass")) |>
    dplyr::mutate(MgN = Value * Product_kgDM_kgFM * Product_kgN_kgDM) |>
    dplyr::group_by(Year, area, Live_anim) |>
    dplyr::summarize(
      MgN = sum(MgN),
      LU = mean(LU),
      .groups = "drop"
    ) |>
    dplyr::mutate(N_yield = MgN / LU) |>
    dplyr::filter(N_yield != 0) |>
    dplyr::filter(Live_anim %in% Draught_animals) |>
    dplyr::mutate(
      Win = DescTools::Winsorize(N_yield, val = quantile(N_yield, probs = c(0.5, 1))),
      min_yield = min(Win),
      Yield_share = min_yield / N_yield,
      Yield_share_excess = dplyr::if_else(Yield_share > 1, Yield_share, 1),
      draught_share = 1 - (1 / Yield_share_excess)
    )
  
  draught_shares <- draught_shares_all |>
    dplyr::select(Year, area, Live_anim, draught_share)
  
  # Calculate footprint of primary items
  FP_prim <- Primary_raw |>
    dplyr::left_join(Impact_prod |>
      dplyr::select(Year, area_code, item_code, Impact, Value, u_FU) |>
      dplyr::rename(
        item_code_impact = item_code,
        FU = Value
      ), by = c("Year", "area_code", "item_code_impact", "Impact")) |>
    dplyr::mutate(Origin = "Production") |>
    dplyr::left_join(Seed_share |>
      dplyr::select(Year, area_code, item_code, Seed_share) |>
      dplyr::rename(item_code_cbs = item_code), by = c("Year", "area_code", "item_code_cbs")) |>
    dplyr::group_by(Year, area, area_code, Live_anim, Live_anim_code, Origin, Impact, item_code_impact, unit) |>
    dplyr::mutate(
      Seed_share = tidyr::replace_na(Seed_share, 0),
      Value = Value * (1 - Seed_share)
    ) |>
    dplyr::ungroup() |>
    # Add products and residues
    (function(df) {
      dplyr::bind_rows(
        df |> dplyr::mutate(Product_residue = "Product"),
        df |>
          dplyr::group_by(Year, area, area_code, Live_anim, Live_anim_code, unit, Item_code_area, Multi_type, item_code_impact, Impact, Origin) |>
          dplyr::summarize(
            FU = mean(FU, na.rm = TRUE),
            u_FU = mean(u_FU),
            .groups = "drop"
          ) |>
          dplyr::right_join(Crop_NPPr_NoFallow |>
            dplyr::filter(Product_residue == "Residue") |>
            dplyr::select(Year, area, item_prod, item_cbs, Prod_ygpit_Mg) |>
            dplyr::left_join(items_prod_full |>
              dplyr::select(item_code_prod, item_prod), by = c("item_prod")) |>
            dplyr::rename(
              item_code_impact = item_code_prod,
              item = item_cbs
            ) |>
            dplyr::select(-item_prod) |>
            dplyr::left_join(items_full |>
              dplyr::select(item_code, item, group), by = c("item")) |>
            dplyr::rename(
              item_cbs = item,
              item_code_cbs = item_code,
              Value = Prod_ygpit_Mg
            ), by = c("Year", "area", "item_code_impact")) |>
          dplyr::mutate(Product_residue = "Residue")
      )
    })() |>
    dplyr::filter(!is.na(Value), Value != 0) |>
    dplyr::left_join(items_prod_full |>
      dplyr::left_join(Biomass_coefs, by = c("Name_biomass")) |>
      dplyr::select(item_prod, Product_kgDM_kgFM, Product_kgN_kgDM) |>
      dplyr::rename(item = item_prod) |>
      dplyr::filter(!is.na(item)), by = c("item")) |>
    dplyr::mutate(
      Yield = Value / FU,
      Yield_DM = Yield * Product_kgDM_kgFM,
      Yield_N = Yield_DM * Product_kgN_kgDM
    ) |>
    Allocate_impacts_to_products() |>
    dplyr::group_by(Year, area, item_code_impact, item_cbs, item_code_cbs, Impact) |>
    dplyr::mutate(n = n()) |>
    dplyr::ungroup()
  
  # Aggregate to CBS items
  FP_prim_i <- FP_prim |>
    Agg_primary()
  
  # Global averages
  FP_prim_i_global <- FP_prim_i |>
    dplyr::group_by(Year, Impact, item, item_code) |>
    dplyr::summarize(
      Value = sum(Value),
      Impact_u = sum(Impact_u),
      .groups = "drop"
    ) |>
    dplyr::mutate(u_ton = Impact_u / Value)
  
  # Footprint of availability (production + import)
  FP_prim_ds <- calc_avail_fp(
    CBS_NoSeeds |>
      dplyr::left_join(items_full |>
        dplyr::select(item_code, group), by = c("item_code")) |>
      dplyr::filter(
        Element %in% c("Production", "Import"),
        group %in% c("Primary crops", "Livestock products", "Fish") | 
          item %in% c("Cotton lint", "Cottonseed", "Palm Oil", "Palm kernels")
      ),
    FP_prim_i |> dplyr::filter(!is.na(Impact))
  )
  
  FP_prim_ds_i <- FP_prim_ds |>
    dplyr::group_by(Year, area, area_code, item, item_code, Origin, Impact) |>
    dplyr::summarize(
      Value = sum(Value, na.rm = TRUE),
      Impact_u = sum(Impact_u, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(u_ton = Impact_u / Value)
  
  # Calculate import shares
  Import_share <- FP_prim_ds |>
    dplyr::group_by(Year, area, item, Impact) |>
    dplyr::summarize(
      Value = sum(Value, na.rm = TRUE),
      Impact_u = sum(Impact_u, na.rm = TRUE),
      .groups = "drop"
    ) |>
    tidyr::pivot_wider(names_from = Origin, values_from = c(Value, Impact_u), values_fill = 0)
  
  # Calculate processing shares
  Processing_shares <- CBS_NoSeeds |>
    tidyr::pivot_wider(names_from = Element, values_from = Value, values_fill = 0) |>
    dplyr::mutate(Proc_share = Processing / (Production + Import)) |>
    dplyr::select(Year, area, area_code, item, item_code, Proc_share) |>
    dplyr::filter(Proc_share > 0, !is.na(Proc_share))
  
  # Processed products
  FP_processed_raw <- Calc_impact_processed(FP_prim_ds)
  FP_processed_raw_i <- Agg_processed(FP_processed_raw)
  
  FP_processed_ds <- calc_avail_fp(
    CBS_NoSeeds |>
      dplyr::left_join(items_full |>
        dplyr::select(item_code, group), by = c("item_code")) |>
      dplyr::filter(
        Element %in% c("Production", "Import"),
        group == "Processed"
      ),
    FP_processed_raw_i
  )
  
  # Re-processed products (e.g., wine to alcohol)
  FP_reprocessed_raw <- Calc_impact_processed(FP_processed_ds)
  FP_reprocessed_raw_i <- Agg_processed(FP_reprocessed_raw)
  
  FP_processed_ds_i <- dplyr::bind_rows(
    FP_processed_ds,
    FP_reprocessed_raw_i |>
      dplyr::mutate(Element = "Production")
  ) |>
    dplyr::group_by(Year, area, area_code, item, item_code, Origin, Impact) |>
    dplyr::summarize(
      Value = sum(Value, na.rm = TRUE),
      Impact_u = sum(Impact_u, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(u_ton = Impact_u / Value)
  
  # Feed products
  FP_feed_i <- dplyr::bind_rows(FP_prim_i, FP_processed_raw_i, FP_reprocessed_raw_i)
  
  FP_feed_ds <- calc_avail_fp(
    CBS_NoSeeds |>
      dplyr::filter(Element %in% c("Production", "Import")),
    FP_feed_i
  )
  
  FP_feed_ds_i <- FP_feed_ds |>
    dplyr::group_by(Year, area, area_code, item, item_code, Origin, Impact) |>
    dplyr::summarize(
      Value = sum(Value, na.rm = TRUE),
      Impact_u = sum(Impact_u, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(u_ton = Impact_u / Value)
  
  # Final comprehensive footprint
  FP_final <- dplyr::bind_rows(
    FP_prim_ds_i |> dplyr::mutate(Product_group = "Primary"),
    FP_processed_ds_i |> dplyr::mutate(Product_group = "Processed"),
    FP_feed_ds_i |> dplyr::mutate(Product_group = "Feed")
  )
  
  # Return all footprint objects as a named list
  return(list(
    FP_prim = FP_prim,
    FP_prim_ds = FP_prim_ds,
    FP_processed_raw = FP_processed_raw,
    FP_processed_ds = FP_processed_ds,
    FP_feed = FP_feed_ds,
    FP_feed_ds = FP_feed_ds_i,
    FP_final = FP_final,
    Seed_share = Seed_share,
    draught_shares = draught_shares
  ))
}
