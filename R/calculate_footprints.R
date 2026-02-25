#' Calculate Complete Environmental Footprints Along Supply Chains
#'
#' Main workflow function that traces environmental footprints through global supply chains,
#' from primary production through processing, feed, and final products. This function
#' orchestrates the entire footprint calculation process and returns all intermediate
#' and final footprint data frames.
#'
#' @param dtm Optional detailed trade matrix. If `NULL` (default), the function
#'   uses gross-trade mode (`calc_avail_fp_gt()`). If provided, it automatically
#'   uses detailed bilateral trade mode (`calc_avail_fp_dtm()`). Required columns
#'   when provided: `Year`, `area_code`, `area_code_p`, `area_p`,
#'   `item_code_cbs`, `Element`, `Country_share` (with `Element == "Import"`
#'   rows used). `Impact` is generated internally by expanding `dtm` rows across
#'   impacts present in `Impact_prod` for each year.
#'
#' @return A named list of intermediate and final footprint tables, including:
#'   `FP_prim`, `FP_prim_i`, `FP_prim_i_global`, `FP_prim_ds`, `FP_prim_ds_i`,
#'   `FP_processed_raw`, `FP_processed_raw_i`, `FP_processed_ds`,
#'   `FP_processed_ds_i`, `FP_reprocessed_raw`, `FP_reprocessed_raw_i`,
#'   `FP_raw_all`, `FP_feed_raw`, `FP_feed`, `FP_feed_i`, `FP_feed_ds`,
#'   `FP_feed_ds_i`, `FP_ioc`, `FP_i`, `FP_final`, `Seed_share`,
#'   `draught_shares`, `Import_share`, and `Processing_shares`.
#'
#' @details
#' This function implements a complete footprint accounting system that:
#' \itemize{
#'   \item Calculates seed shares and removes them from production
#'   \item Allocates impacts to co-products using economic allocation
#'   \item Traces impacts through processing chains
#'   \item Accounts for international trade (gross trade when `dtm` is `NULL`,
#'     bilateral trade matrix when `dtm` is provided)
#'   \item Handles feed products and livestock production
#'   \item Allocates draught animal services to crop production
#' }
#'
#' `calculate_footprints()` only takes `dtm` explicitly. All other required
#' inputs are read implicitly from objects in the calling environment.
#'
#' Required workflow objects (created outside `load_general_data()`):
#' \describe{
#'   \item{`CBS`}{Commodity balance table. Columns used: `Year`, `area`,
#'   `area_code`, `item_cbs`, `item_code_cbs`, `Element`, `Value`.}
#'   \item{`Primary_all`}{Primary production / co-product table. Columns used:
#'   `Year`, `area`, `area_code`, `item_prod`, `item_code_prod`, `unit`,
#'   `Value`, `item_cbs`, `item_code_cbs`, `Live_anim`, `Live_anim_code`.}
#'   \item{`Impact_prod`}{Production impact table. Columns used: `Year`, `area`,
#'   `item_code_prod`, `Impact`, `Value`, `u_FU` (and optionally `item_prod`).}
#'   \item{`Crop_NPPr_NoFallow`}{Crop NPP / residue table. Columns used:
#'   `Year`, `area`, `item_prod`, `item_cbs`, `Product_residue`,
#'   `Prod_ygpit_Mg`.}
#'   \item{`Feed_intake`}{Feed intake table. Columns used: `Year`, `area`,
#'   `Live_anim`, `item_cbs`, `item_code_cbs`, `Supply`, `Intake_DM`.
#'   `area_code` is recovered from footprint tables during the feed join.}
#'   \item{`Primary_prices`}{Primary product prices. Columns used: `Year`,
#'   `item_code_prod`, `Price`.}
#'   \item{`CBS_item_prices`}{CBS item prices. Columns used: `Year`, `Element`,
#'   `item_cbs`, `item_code_cbs`, `Price`.}
#'   \item{`Processing_coefs`}{Processing conversion coefficients. Columns used:
#'   `Year`, `area_code`, `item_code_cbs`, `item_cbs`, `cf`.}
#'   \item{`Relative_residue_price`}{Numeric scalar used to value residues
#'   relative to product prices.}
#' }
#'
#' Required auxiliary objects loaded by `load_general_data()`:
#' \describe{
#'   \item{`items_full` (sheet `items_full` in `Codes_coefs.xlsx`)}{Columns used:
#'   `item_cbs`, `item_code_cbs`, `group`.}
#'   \item{`items_prod_full` (sheet `items_prod_full` in `Codes_coefs.xlsx`)}{
#'   Columns used: `item_prod`, `item_code_prod`, `Name_biomass`.}
#'   \item{`Animals_codes` (sheet `Animals_codes` in `Codes_coefs.xlsx`)}{
#'   Columns used: `item_cbs`, `item_code_cbs`.}
#'   \item{`Primary_double` (sheet `Primary_double` in `Codes_coefs.xlsx`)}{
#'   Columns used (via `Prepare_prim()`): `Item_area`, `item_prod`,
#'   `item_code_prod`, `Multi_type`.}
#'   \item{`Biomass_coefs` (`Biomass_coefs.xlsx`)}{Columns used:
#'   `Name_biomass`, `Product_kgDM_kgFM`, `Product_kgN_kgDM`.}
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(afsetools)
#' load_general_data()
#'
#' # Requires workflow objects in the environment:
#' # CBS, Primary_all, Impact_prod, Crop_NPPr_NoFallow, Feed_intake,
#' # Primary_prices, CBS_item_prices, Processing_coefs, Relative_residue_price
#'
#' # Calculate footprints using gross trade (omit dtm)
#' footprints <- calculate_footprints()
#'
#' # To use bilateral trade, pass dtm = my_dtm_data (or dtm = DTM)
#'
#' # Access individual footprint tables
#' fp_primary <- footprints$FP_prim
#' fp_final <- footprints$FP_final
#' }
calculate_footprints <- function(dtm = NULL) {

  cbs <- CBS
  primary <- Primary_all
  impact_prod <- Impact_prod
  crop_nppr <- Crop_NPPr_NoFallow
  feed_intake <- Feed_intake

  # Select trade calculation method automatically from dtm availability.
  calc_avail_fp <- if (is.null(dtm)) {
    function(filtered_cbs, df) {
      calc_avail_fp_gt(filtered_cbs, df, cbs = cbs)
    }
  } else {
    function(filtered_cbs, df) {
      calc_avail_fp_dtm(
        filtered_cbs,
        df,
        cbs = cbs,
        dtm = dtm,
        impact_prod = impact_prod
      )
    }
  }

  # data.table fast path for large aggregations.
  fast_sum_value_impact <- function(df, by_cols) {
    by_cols <- as.character(unlist(by_cols, use.names = FALSE))
    df |>
      dplyr::group_by(!!!rlang::syms(by_cols)) |>
      dplyr::summarize(
        Value = sum(Value, na.rm = TRUE),
        Impact_u = sum(Impact_u, na.rm = TRUE),
        .groups = "drop"
      )
  }

  fast_sum_impact <- function(df, by_cols) {
    by_cols <- as.character(unlist(by_cols, use.names = FALSE))
    df |>
      dplyr::group_by(!!!rlang::syms(by_cols)) |>
      dplyr::summarize(
        Impact_u = sum(Impact_u, na.rm = TRUE),
        .groups = "drop"
      )
  }
  
  # Calculate share of seeds over production
  Seed_share <- cbs |>
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
    dplyr::select(Year, area, area_code, item_cbs, item_code_cbs, Seed_share, Seed)
  
  # Remove seeds from CBS
  CBS_NoSeeds <- cbs |>
    dplyr::left_join(Seed_share, by = c("Year", "area", "area_code", "item_cbs", "item_code_cbs")) |>
    dplyr::mutate(Value = dplyr::if_else(Element %in% c("Production", "Domestic_supply"),
      Value - Seed,
      dplyr::if_else(Element == "Seed", 0, Value)
    )) |>
    dplyr::select(-Seed_share, -Seed)
  
  # Prepare primary production data
  Primary_raw <- Prepare_prim(primary)
  
  # Calculate draught animal shares
  Draught_animals <- c("Buffalo", "Camels", "Cattle, dairy", "Cattle, non-dairy", 
                       "Asses", "Horses", "Mules", "Camelids, other")
  
  draught_shares_all <- Primary_raw |>
    dplyr::filter(unit == "tonnes") |>
    dplyr::inner_join(primary |>
      dplyr::mutate(Live_anim = item_prod) |>
      dplyr::filter(unit == "LU") |>
      dplyr::rename(LU = Value) |>
      dplyr::select(Year, area, Live_anim, LU), by = c("Year", "area", "Live_anim")) |>
    dplyr::left_join(items_prod_full |>
      dplyr::select(item_prod, Name_biomass), by = c("item_prod")) |>
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
    dplyr::mutate(item_code_impact = as.character(item_code_impact)) |>
    dplyr::left_join(impact_prod |>
      dplyr::select(Year, area, item_code_prod, Impact, Value, u_FU) |>
      dplyr::mutate(item_code_prod = as.character(item_code_prod)) |>
      dplyr::rename(
        item_code_impact = item_code_prod,
        FU = Value
      ), by = c("Year", "area", "item_code_impact")) |>
    dplyr::mutate(Origin = "Production") |>
    dplyr::left_join(Seed_share |>
      dplyr::select(Year, area_code, item_code_cbs, Seed_share), by = c("Year", "area_code", "item_code_cbs")) |>
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
          dplyr::right_join(crop_nppr |>
            dplyr::filter(Product_residue == "Residue") |>
            dplyr::select(Year, area, item_prod, item_cbs, Prod_ygpit_Mg) |>
            dplyr::left_join(items_prod_full |>
              dplyr::select(item_code_prod, item_prod), by = c("item_prod")) |>
            dplyr::mutate(item_code_prod = as.character(item_code_prod)) |>
            dplyr::rename(
              item_code_impact = item_code_prod,
              Value = Prod_ygpit_Mg
            ) |>
            dplyr::left_join(items_full |>
              dplyr::select(item_cbs, item_code_cbs, group), by = c("item_cbs")) |>
            dplyr::select(-group), by = c("Year", "area", "item_code_impact")) |>
          dplyr::mutate(Product_residue = "Residue")
      )
    })() |>
    dplyr::filter(!is.na(Value), Value != 0) |>
    dplyr::left_join(items_prod_full |>
      dplyr::left_join(Biomass_coefs, by = c("Name_biomass")) |>
      dplyr::select(item_prod, Product_kgDM_kgFM, Product_kgN_kgDM) |>
      dplyr::filter(!is.na(item_prod)), by = c("item_prod")) |>
    dplyr::mutate(
      Yield = Value / FU,
      Yield_DM = Yield * Product_kgDM_kgFM,
      Yield_N = Yield_DM * Product_kgN_kgDM
    ) |>
    Allocate_impacts_to_products(draught_shares = draught_shares) |>
    dplyr::group_by(Year, area, item_code_impact, item_cbs, item_code_cbs, Impact) |>
    dplyr::mutate(n = n()) |>
    dplyr::ungroup()
  
  # Aggregate to CBS items
  FP_prim_i <- FP_prim |>
    Agg_primary()
  
  # Global averages
  FP_prim_i_global <- FP_prim_i |>
    dplyr::group_by(Year, Impact, item_cbs, item_code_cbs) |>
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
        dplyr::select(item_code_cbs, group), by = c("item_code_cbs")) |>
      dplyr::filter(
        Element %in% c("Production", "Import"),
        group %in% c("Primary crops", "Livestock products", "Fish")
      ),
    FP_prim_i |> dplyr::filter(!is.na(Impact))
  )
  
  FP_prim_ds_i <- FP_prim_ds |>
    dplyr::group_by(Year, area, area_code, Element, Origin, Impact, item_cbs) |>
    dplyr::summarize(
      Impact_Mu = sum(Impact_u, na.rm = TRUE) / 1000000,
      Value = sum(Value, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::group_by(Year, area, Impact) |>
    dplyr::mutate(Elem_share = Impact_Mu / sum(Impact_Mu)) |>
    dplyr::ungroup()
  
  # Calculate import shares
  Import_share <- FP_prim_ds |>
    dplyr::group_by(Year, area, Element, Impact) |>
    dplyr::summarize(
      Impact_Mu = sum(Impact_u, na.rm = TRUE) / 1000000,
      .groups = "drop"
    ) |>
    dplyr::group_by(Year, area, Impact) |>
    dplyr::mutate(Elem_share = Impact_Mu / sum(Impact_Mu)) |>
    dplyr::ungroup()
  
  # Calculate processing shares
  processing_shares <- CBS_NoSeeds |>
    dplyr::filter(Element == "Processing") |>
    dplyr::rename(Value_proc = Value) |>
    dplyr::left_join(
      CBS_NoSeeds |>
        dplyr::filter(Element %in% c("Production", "Import")) |>
        dplyr::group_by(Year, area_code, item_code_cbs) |>
        dplyr::summarize(Value_ds = sum(Value, na.rm = TRUE), .groups = "drop"),
      by = c("Year", "area_code", "item_code_cbs")
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(Proc_share = Value_proc / Value_ds)
  
  # Processed products
  FP_processed_raw <- Calc_impact_processed(FP_prim_ds, processing_shares = processing_shares)
  FP_processed_raw_i <- Agg_processed(FP_processed_raw)
  
  FP_processed_ds <- calc_avail_fp(
    CBS_NoSeeds |>
      dplyr::left_join(items_full |>
        dplyr::select(item_code_cbs, group), by = c("item_code_cbs")) |>
      dplyr::filter(
        Element %in% c("Production", "Import"),
        group %in% c("Crop products", "Processed")
      ),
    FP_processed_raw_i
  )
  
  # Re-processed products (e.g., wine to alcohol)
  FP_reprocessed_raw <- Calc_impact_processed(FP_processed_ds, processing_shares = processing_shares)
  FP_reprocessed_raw_i <- Agg_processed(FP_reprocessed_raw)
  
  FP_processed_ds_i <- dplyr::bind_rows(
    FP_processed_ds |>
      dplyr::mutate(Cat_proc = "Processed"),
    FP_reprocessed_raw_i |>
      dplyr::mutate(Cat_proc = "Reprocessed")
  ) |>
    dplyr::group_by(Year, area, area_code, Element, Origin, Impact, item_cbs, Cat_proc) |>
    dplyr::summarize(
      Value = sum(Value, na.rm = TRUE),
      Impact_Mu = sum(Impact_u, na.rm = TRUE) / 1000000,
      .groups = "drop"
    ) |>
    dplyr::group_by(Year, area, Impact) |>
    dplyr::mutate(Elem_share = Impact_Mu / sum(Impact_Mu)) |>
    dplyr::ungroup() |>
    dplyr::filter(Impact_Mu != 0) |>
    dplyr::mutate(i_ton = Impact_Mu * 1000000 / Value)

  FP_raw_all <- fast_sum_value_impact(
    dplyr::bind_rows(
      FP_prim_ds,
      FP_processed_ds,
      FP_reprocessed_raw_i
    ),
    by_cols = c("Year", "area", "area_code", "Element", "Origin", "Impact", "item_cbs", "item_code_cbs")
  ) |>
    dplyr::mutate(u_ton = Impact_u / Value) |>
    dplyr::group_by(Year, area, area_code, item_cbs, item_code_cbs, Impact) |>
    dplyr::mutate(u_ton_scaled = Impact_u / sum(Value)) |>
    dplyr::ungroup()

  # Feed footprint of livestock products (legacy script logic)
  FP_feed_raw <- feed_intake |>
    dplyr::left_join(
      Animals_codes |>
        dplyr::select(
          Live_anim = item_cbs,
          Live_anim_code = item_code_cbs
        ) |>
        dplyr::distinct(),
      by = c("Live_anim")
    ) |>
    dplyr::left_join(
      FP_raw_all |>
        dplyr::select(Year, area, area_code, item_cbs, item_code_cbs, Impact, Origin, u_ton_scaled),
      by = c("Year", "area", "item_cbs", "item_code_cbs")
    ) |>
    dplyr::mutate(FPFeed_u = Supply * u_ton_scaled) |>
    dplyr::group_by(Year, area, area_code, Live_anim, Live_anim_code, Impact, Origin) |>
    dplyr::mutate(DM_origin = sum(Intake_DM, na.rm = TRUE)) |>
    dplyr::group_by(Year, area, area_code, Live_anim, Live_anim_code, Impact) |>
    dplyr::mutate(
      DM_LiveAnim = sum(Intake_DM, na.rm = TRUE),
      Origin_share_DM = DM_origin / DM_LiveAnim
    ) |>
    dplyr::ungroup()

  FP_feed <- FP_feed_raw |>
    dplyr::group_by(Year, area, area_code, Live_anim, Live_anim_code, Impact, Origin) |>
    dplyr::summarize(
      Impact_u = sum(FPFeed_u, na.rm = TRUE),
      Origin_share_DM = mean(Origin_share_DM, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::filter(!is.na(Origin)) |>
    dplyr::mutate(
      FU = 1,
      u_FU = Impact_u / FU
    ) |>
    dplyr::left_join(
      primary |>
        dplyr::filter(unit == "tonnes"),
      by = c("Year", "area", "area_code", "Live_anim" = "item_prod", "Live_anim_code" = "item_code_prod")
    ) |>
    dplyr::mutate(
      Value_tot = Value,
      Value = Value_tot * Origin_share_DM
    ) |>
    dplyr::mutate(
      item_code_impact = Live_anim_code,
      item_code_prod = Live_anim_code,
      Product_residue = "Product"
    ) |>
    Allocate_impacts_to_products(draught_shares = draught_shares) |>
    dplyr::filter(!is.na(Impact))

  FP_feed_i <- Agg_primary(FP_feed) |>
    dplyr::filter(!is.na(item_cbs))

  FP_feed_ds <- calc_avail_fp(
    CBS_NoSeeds |>
      dplyr::left_join(items_full |>
        dplyr::select(item_code_cbs, group), by = c("item_code_cbs")) |>
      dplyr::filter(
        Element %in% c("Production", "Import"),
        group == "Livestock products"
      ),
    FP_feed_i
  ) |>
    dplyr::filter(!is.na(Origin))

  # Keep a summary table for package workflows that expected a summarized feed output.
  FP_feed_ds_i <- FP_feed_ds |>
    dplyr::group_by(Year, area, area_code, item_cbs, item_code_cbs, Origin, Impact) |>
    dplyr::summarize(
      Value = sum(Value, na.rm = TRUE),
      Impact_u = sum(Impact_u, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(u_ton = Impact_u / Value)

  FP_ioc <- dplyr::bind_rows(
    FP_raw_all |>
      dplyr::left_join(items_full |>
        dplyr::select(item_code_cbs, group), by = c("item_code_cbs")) |>
      dplyr::mutate(Prod_class = dplyr::if_else(
        group == "Livestock products",
        "Animal",
        "Cropland"
      )),
    FP_feed_ds |>
      dplyr::mutate(Prod_class = "Cropland")
  ) |>
    dplyr::filter(!is.na(Impact))

  FP_ioc <- fast_sum_impact(
    FP_ioc,
    by_cols = c("Year", "area", "area_code", "item_cbs", "item_code_cbs", "Element", "Impact", "Prod_class", "Origin")
  ) |>
    dplyr::left_join(
      CBS_NoSeeds,
      by = c("Year", "area", "area_code", "item_cbs", "item_code_cbs", "Element")
    ) |>
    dplyr::mutate(u_ton = Impact_u / Value) |>
    dplyr::ungroup() |>
    dplyr::left_join(
      items_full |>
        dplyr::select(item_cbs, item_code_cbs, group),
      by = c("item_cbs", "item_code_cbs")
    )

  FP_i <- fast_sum_impact(
    FP_ioc,
    by_cols = c("Year", "area", "area_code", "Prod_class", "item_cbs", "item_code_cbs", "Element", "Impact")
  ) |>
    dplyr::left_join(
      CBS_NoSeeds,
      by = c("Year", "area", "area_code", "item_cbs", "item_code_cbs", "Element")
    ) |>
    dplyr::mutate(u_ton = Impact_u / Value) |>
    dplyr::ungroup()

  FP_final <- fast_sum_value_impact(
    FP_ioc,
    by_cols = c("Year", "Impact", "area", "area_code", "Element", "Origin", "item_cbs", "item_code_cbs", "group")
  ) |>
    dplyr::mutate(u_ton = Impact_u / Value) |>
    dplyr::left_join(
      CBS_NoSeeds |>
        tidyr::pivot_wider(
          names_from = Element,
          values_from = Value,
          values_fill = 0
        ) |>
        dplyr::mutate(
          Exp_share = Export / (Production + Import),
          Exp_share = dplyr::if_else(
            is.infinite(Exp_share) | Exp_share > 1,
            1,
            Exp_share
          )
        ) |>
        dplyr::select(Year, area, item_cbs, Exp_share),
      by = c("Year", "area", "item_cbs")
    ) |>
    dplyr::mutate(
      Export_u = Impact_u * Exp_share,
      Consumption_u = Impact_u - Export_u
    )
  
  # Return all footprint objects as a named list
  return(list(
    FP_prim = FP_prim,
    FP_prim_i = FP_prim_i,
    FP_prim_i_global = FP_prim_i_global,
    FP_prim_ds = FP_prim_ds,
    FP_prim_ds_i = FP_prim_ds_i,
    FP_processed_raw = FP_processed_raw,
    FP_processed_raw_i = FP_processed_raw_i,
    FP_processed_ds = FP_processed_ds,
    FP_processed_ds_i = FP_processed_ds_i,
    FP_reprocessed_raw = FP_reprocessed_raw,
    FP_reprocessed_raw_i = FP_reprocessed_raw_i,
    FP_raw_all = FP_raw_all,
    FP_feed_raw = FP_feed_raw,
    FP_feed = FP_feed,
    FP_feed_i = FP_feed_i,
    FP_feed_ds = FP_feed_ds,
    FP_feed_ds_i = FP_feed_ds_i,
    FP_ioc = FP_ioc,
    FP_i = FP_i,
    FP_final = FP_final,
    Seed_share = Seed_share,
    draught_shares = draught_shares,
    Import_share = Import_share,
    Processing_shares = processing_shares
  ))
}
