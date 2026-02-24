#' Impact Tracing Functions
#'
#' Functions to trace environmental impacts through supply chains from primary
#' production to processed products, accounting for trade and economic allocation.

#' Prepare Primary Production Database
#'
#' Prepares the primary production database by adding impact codes and filtering
#' multi-products (cotton, oil palm, etc.).
#'
#' @param Prim_all Primary production data frame with columns: unit, item_prod, item_code_prod, Value
#'
#' @return A data frame with item_code_impact added for joining with impact data
#' @export
#'
#' @examples
#' \dontrun{
#' primary_data <- Prepare_prim(Primary_all)
#' }
Prepare_prim <- function(Prim_all) {
  Prim_all |>
    dplyr::filter(unit == "tonnes") |>
    dplyr::left_join(Primary_double |> # Joining code of corresponding primary product for double products
      dplyr::filter(!is.na(Item_area)) |>
      dplyr::select(Item_area, item_code_prod, Multi_type) |>
      dplyr::left_join(Primary_double |>
        dplyr::filter(is.na(Item_area)) |>
        dplyr::select(item_prod, item_code_prod) |>
        dplyr::rename(
          Item_area = item_prod,
          Item_code_area = item_code_prod
        ), by = c("Item_area")) |>
      dplyr::select(item_code_prod, Item_code_area, Multi_type), by = c("item_code_prod")) |>
    dplyr::mutate(item_code_impact = dplyr::if_else(!is.na(Live_anim_code), # Code for joining impacts: same as product except for double products and animal products
      Live_anim_code,
      dplyr::if_else(!is.na(Item_code_area),
        Item_code_area,
        item_code_prod
      )
    )) |>
    dplyr::anti_join(Primary_double |> # Removing primary double products (Seed cotton, Hemp, etc.)
      dplyr::filter(is.na(Item_area)) |>
      dplyr::select(item_code_prod), by = c("item_code_prod"))
}

#' Allocate Impacts to Products Based on Economic Value
#'
#' Performs economic allocation of environmental impacts among co-products
#' (e.g., cotton lint and cottonseed) and creates draught animal items.
#'
#' @param df A data frame with impact data for products
#' @param draught_shares Draught animal allocation shares by `Year`, `area`, `Live_anim`
#'
#' @return A data frame with allocated impacts (Allocation, Impact_u, u_ton columns added)
#' @export
#'
#' @examples
#' \dontrun{
#' allocated_impacts <- Allocate_impacts_to_products(impact_data, draught_shares)
#' }
Allocate_impacts_to_products <- function(df, draught_shares) {
  dplyr::bind_rows(
    df |> # Create "Draught" item to allocate some impacts to it
      dplyr::group_by(Year, area, area_code, Live_anim, Live_anim_code, item_code_impact, Origin, Impact) |>
      dplyr::summarize(
        FU = mean(FU), # Functional units are same for all products within item_code_impacts
        u_FU = mean(u_FU), # Impact per functional unit is same for all products within item_code_impacts
        .groups = "drop"
      ) |>
      dplyr::inner_join(draught_shares, by = c("Year", "area", "Live_anim")) |>
      dplyr::filter(draught_share > 0) |>
      dplyr::mutate(
        Product_residue = "Draught",
        item_cbs = "Animal draught"
      ) |>
      dplyr::left_join(items_full |>
        dplyr::select(item_cbs, item_code_cbs), by = c("item_cbs")), # Add codes for draught items
    df |> # The rest of the products
      dplyr::left_join(draught_shares, by = c("Year", "area", "Live_anim")) |>
      dplyr::mutate(draught_share = tidyr::replace_na(draught_share, 0)) |>
      dplyr::left_join(Primary_prices |>
        dplyr::select(Year, item_code_prod, Price), by = c("Year", "item_code_prod")) |>
      dplyr::left_join(CBS_item_prices |>
        dplyr::filter(Element == "Export") |>
        dplyr::select(Year, item_code_cbs, Price) |>
        dplyr::rename(Price_cbs = Price), by = c("Year", "item_code_cbs")) |>
      dplyr::group_by(Year, area_code, item_code_impact, Origin, Impact) |>
      dplyr::mutate(
        Price = dplyr::if_else(Product_residue == "Residue",
          Price * Relative_residue_price, # Price of residues based on relative price over products
          Price
        ),
        Value_d = Value * Price,
        Alloc_raw1 = Value_d / sum(Value_d), # Allocation based on share of total value, considering item_prod
        Value_cbs = Value * Price_cbs,
        Alloc_cbs = Value_cbs / sum(Value_cbs), # Allocation based on share of total value, considering item_cbs
        n = n(),
        Alloc_raw2 = dplyr::if_else(is.na(Alloc_raw1),
          dplyr::if_else(n == 1,
            1, # If Alloc_raw1 is NA and there's only one product, allocation is 1
            Alloc_cbs # If Alloc_raw1 is NA and there are multiple products, use Alloc_cbs
          ),
          Alloc_raw1
        )
      ) |>
      dplyr::ungroup()
  ) |>
    dplyr::mutate(
      Allocation = dplyr::if_else(is.na(Alloc_raw2),
        draught_share, # Allocation to draught based on draught share
        Alloc_raw2 * (1 - draught_share)
      ), # Product allocation based on Alloc_raw2, minus draught share
      Impact_u = FU * u_FU * Allocation,
      u_ton = Impact_u / Value
    ) |>
    dplyr::group_by(Year, area_code, item_code_impact, Origin, Impact) |>
    dplyr::mutate(n = n()) |>
    dplyr::ungroup()
}

#' Get Global Average Export Footprint
#'
#' Calculates the global average footprint of exported products, weighted by export shares.
#'
#' @param df A data frame with product footprints
#' @param cbs Commodity balance sheet data used to calculate export shares
#'
#' @return A data frame with global average footprints (u_ton_glob)
#' @export
#'
#' @examples
#' \dontrun{
#' global_footprint <- get_global_export_footprint(product_footprints, cbs)
#' }
get_global_export_footprint <- function(df, cbs) {
  df |>
    dplyr::group_by(Year, area_code, item_code_cbs, Impact) |>
    dplyr::summarize(
      Value = sum(Value, na.rm = TRUE), # Aggregate origins in imported products
      Impact_u = sum(Impact_u, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::left_join(cbs |> # Export share per country
      tidyr::pivot_wider(
        names_from = Element,
        values_from = Value,
        values_fill = 0
      ) |>
      dplyr::mutate(
        Exp_share = Export / (Production + Import),
        Exp_share = dplyr::if_else(is.infinite(Exp_share) | Exp_share > 1,
          1,
          Exp_share
        )
      ), by = c("Year", "area_code", "item_code_cbs")) |>
    dplyr::mutate(
      Value = Value * Exp_share,
      Impact_u = Impact_u * Exp_share
    ) |>
    dplyr::group_by(Year, item_code_cbs, Impact) |>
    dplyr::summarize(
      Value = sum(Value, na.rm = TRUE), # Gets impact of exported product at global level
      Impact_u = sum(Impact_u, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(u_ton_glob = Impact_u / Value) |>
    dplyr::select(Year, item_code_cbs, Impact, u_ton_glob) |>
    dplyr::mutate(
      Element = "Import",
      Origin = "Import"
    )
}

#' Calculate Available Footprint Using Gross Trade
#'
#' Calculates the footprint of product availability (production + import) using
#' gross trade data (no bilateral trade detail).
#'
#' @param filtered_cbs Filtered CBS data with production and import
#' @param df Product footprint data frame
#' @param cbs Commodity balance sheet data
#'
#' @return A data frame with availability footprints including imports
#' @export
#'
#' @examples
#' \dontrun{
#' avail_fp <- calc_avail_fp_gt(filtered_cbs, product_footprints, cbs)
#' }
calc_avail_fp_gt <- function(filtered_cbs, df, cbs) {
  dplyr::bind_rows(
    df, # Production impact
    filtered_cbs |> # Import impact
      dplyr::filter(Element == "Import") |>
      dplyr::left_join(
        get_global_export_footprint(df, cbs = cbs),
        by = c("Year", "item_code_cbs", "Element")
      )
  ) |>
    dplyr::mutate(
      u_ton2 = dplyr::if_else(is.na(u_ton),
        u_ton_glob,
        u_ton
      ),
      Impact_u = dplyr::if_else(is.na(Value),
        Impact_u, # Original Impact_u used when there's no Value data (draught animals)
        Value * u_ton2
      )
    )
}

#' Calculate Available Footprint Using Detailed Trade Matrix
#'
#' Calculates the footprint of product availability using bilateral trade data (DTM).
#'
#' @param filtered_cbs Filtered CBS data with production and import
#' @param df Product footprint data frame
#' @param cbs Commodity balance sheet data
#' @param dtm Detailed trade matrix data
#' @param impact_prod Production impact table (new schema with `area`, not
#'   `area_code`; used here only for `Year`/`Impact` coverage)
#'
#' @return A data frame with availability footprints including bilateral import footprints
#' @export
#'
#' @examples
#' \dontrun{
#' avail_fp <- calc_avail_fp_dtm(filtered_cbs, product_footprints, cbs, dtm, impact_prod)
#' }
calc_avail_fp_dtm <- function(filtered_cbs, df, cbs, dtm, impact_prod) {
  dplyr::bind_rows(
    df, # Production impact
    filtered_cbs |> # Import impact
      dplyr::filter(Element == "Import") |>
      dplyr::left_join(
        get_global_export_footprint(df, cbs = cbs),
        by = c("Year", "item_code_cbs", "Element")
      ) |>
      dplyr::left_join(dtm |> # Get footprint in each partner
        dplyr::left_join(impact_prod |> dplyr::group_by(Year, Impact) |>
          dplyr::summarize(Year = mean(Year, na.rm = TRUE), .groups = "drop"), by = c("Year")) |>
        dplyr::filter(Element == "Import") |>
        dplyr::select(Year, area_code, area_code_p, area_p, item_code_cbs, Element, Impact, Country_share) |>
        dplyr::left_join(df |>
          dplyr::group_by(Year, area_code, item_code_cbs, item_cbs, Impact) |>
          dplyr::summarize(
            Value = sum(Value, na.rm = TRUE), # Aggregate origins in imported products
            Impact_u = sum(Impact_u, na.rm = TRUE),
            .groups = "drop"
          ) |>
          dplyr::mutate(
            u_ton_p = Impact_u / Value,
            area_code_p = area_code
          ) |>
          dplyr::select(Year, area_code_p, item_code_cbs, Impact, u_ton_p), by = c("Year", "area_code_p", "item_code_cbs", "Impact")) |>
        dplyr::mutate(Origin = "Import") |>
        dplyr::filter(!is.na(Country_share)), by = c("Year", "area_code", "item_code_cbs", "Element", "Impact", "Origin"))
  ) |>
    dplyr::mutate(
      Country_share = tidyr::replace_na(Country_share, 1),
      Value = Value * Country_share,
      u_ton2 = dplyr::if_else(is.na(u_ton),
        dplyr::if_else(is.na(u_ton_p),
          u_ton_glob,
          u_ton_p
        ),
        u_ton
      ),
      Impact_u = dplyr::if_else(is.na(Value),
        Impact_u, # Original Impact_u used when there's no Value data (draught animals)
        Value * u_ton2
      )
    )
}

#' Calculate Embodied Impact of Processed Products
#'
#' Traces impacts through processing chains using processing shares and conversion coefficients.
#'
#' @param df Product footprint data frame
#' @param processing_shares Processing shares table
#'
#' @return A data frame with processed product footprints
#' @export
#'
#' @examples
#' \dontrun{
#' processed_fp <- Calc_impact_processed(primary_footprints, processing_shares)
#' }
Calc_impact_processed <- function(df, processing_shares) {
  df |>
    dplyr::group_by(Year, area, area_code, item_cbs, item_code_cbs, Origin, Impact) |>
    dplyr::summarize(
      Value = sum(Value, na.rm = TRUE),
      Impact_u = sum(Impact_u, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(u_ton = Impact_u / Value) |>
    dplyr::inner_join(processing_shares, by = c("Year", "area", "area_code", "item_cbs", "item_code_cbs")) |>
    dplyr::group_by(Year, area_code, item_code_cbs, Impact) |>
    dplyr::left_join(Processing_coefs |>
      dplyr::select(Year, area_code, item_code_cbs = item_code, Item, cf), by = c("Year", "area_code", "item_code_cbs")) |>
    dplyr::left_join(CBS_item_prices |>
      dplyr::filter(Element == "Export") |>
      dplyr::select(Year, Item = item_cbs, Price), by = c("Year", "Item")) |>
    dplyr::filter(Value != 0) |>
    dplyr::group_by(Year, area_code, item_code_cbs, Origin, Impact) |>
    dplyr::mutate(
      tons_proc = Value * Proc_share * cf, # Tons of resulting processed product
      Value_d = tons_proc * Price, # Monetary value of processed product
      Alloc = Value_d / sum(Value_d),
      n = n(),
      Allocation = dplyr::if_else(is.na(Alloc),
        dplyr::if_else(n == 1, # Allocation set to 1 for products with only one processed product
          1,
          Alloc
        ),
        Alloc
      ),
      Impact_u_proc = Impact_u * Proc_share * Allocation,
      u_ton_proc = Impact_u_proc / tons_proc,
      Element = "Production"
    ) |>
    dplyr::ungroup()
}

#' Aggregate Primary Products to CBS Items
#'
#' Aggregates primary production footprints from production items to CBS (commodity balance sheet) items.
#'
#' @param df Primary product footprint data frame
#'
#' @return A data frame with footprints aggregated to CBS item level
#' @export
#'
#' @examples
#' \dontrun{
#' primary_cbs <- Agg_primary(primary_footprints)
#' }
Agg_primary <- function(df) {
  df |>
    dplyr::group_by(Year, area, area_code, item_cbs, item_code_cbs, Origin, Impact) |>
    dplyr::summarize(
      Value = sum(Value),
      Impact_u = sum(Impact_u),
      .groups = "drop"
    ) |>
    dplyr::mutate(u_ton = Impact_u / Value) |>
    dplyr::mutate(Element = "Production")
}

#' Aggregate Processed Products from Countries to Origins
#'
#' Aggregates processed product footprints from country-level to origin-level.
#'
#' @param df Processed product footprint data frame
#'
#' @return A data frame with processed footprints aggregated by origin
#' @export
#'
#' @examples
#' \dontrun{
#' processed_agg <- Agg_processed(processed_footprints)
#' }
Agg_processed <- function(df) {
  df |>
    dplyr::group_by(Year, area, area_code, Item, Origin, Impact) |>
    dplyr::summarize(
      Value = sum(tons_proc, na.rm = TRUE),
      Impact_u = sum(Impact_u_proc, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      u_ton = Impact_u / Value,
      Element = "Production"
    ) |>
    dplyr::rename(item_cbs = Item) |>
    dplyr::left_join(items_full |>
      dplyr::select(item_cbs, item_code_cbs), by = c("item_cbs")) |>
    dplyr::filter(!is.na(item_cbs))
}
