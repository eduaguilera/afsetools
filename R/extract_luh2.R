#' Extract Data from Land-Use Harmonization 2 (LUH2) Dataset
#'
#' @description
#' Extracts carbon stock and area data from the Land Use Harmonization 2 (LUH2)
#' dataset v2h. Processes land use states, carbon densities, and harvest data
#' from netCDF files and aggregates to specified regions.
#'
#' @param L_files_path Character string with path to LUH2 data files directory
#' @param RegionNames Spatial object (sf) with region polygons for extraction
#' @param studied_period Numeric vector with years to extract (e.g., 1700:2022)
#'
#' @return Data frame Stock_area with columns:
#'   - Region_name: Region identifier
#'   - Year: Year
#'   - Land_Use: Specific land use category (Primary_forested, Secondary_nonforested, etc.)
#'   - LandUse: Aggregated land use (Forest, Grassland_schrubland, Pasture, Cropland, Urban)
#'   - Disturbance: Disturbance level (Primary, Secondary, Managed_pasture, Rangeland, Cropland, Urban)
#'   - Area_Mha: Area in million hectares
#'   - C_stock_Tg: Carbon stock in teragrams
#'   - C_density_Mg_ha: Carbon density in megagrams per hectare
#'
#' @details
#' The function processes the following LUH2 layers:
#' - Primary forested and non-forested lands
#' - Secondary forested and non-forested lands
#' - Managed pasture and rangeland
#' - Cropland (C3 annual, C3 perennial, C4 annual, C4 perennial, C3 N-fixing)
#' - Urban areas
#'
#' Carbon stocks are calculated using:
#' - Potential carbon density (ptbio) for primary lands
#' - Secondary biomass carbon density (secmb) for secondary lands
#'
#' @references
#' Hurtt, G. C. et al. (2011) doi:10.1007/s10584-011-0153-2
#' https://luh.umd.edu/data.shtml
#' LUH2 v2h data: http://gsweb1vh2.umd.edu/LUH2/LUH2_v2h/
#'
#' @examples
#' \dontrun{
#' library(sf)
#' library(raster)
#'
#' # Load region polygons
#' regions <- st_read("path/to/regions.shp")
#'
#' # Extract LUH2 data for 1900-2020
#' stock_area <- extract_luh2(
#'   L_files_path = "path/to/LUH2/",
#'   RegionNames = regions,
#'   studied_period = 1900:2020
#' )
#' }
#'
#' @export
extract_luh2 <- function(L_files_path, RegionNames, studied_period) {
  
  # Calculate layer indices for raster brick subsetting
  initial_year <- min(studied_period)
  final_year <- max(studied_period)
  initial_brick_layer <- initial_year - 849
  final_brick_layer <- final_year - 849
  study_period <- initial_brick_layer:final_brick_layer
  
  # Define year names
  YearNames <- as.character(studied_period)
  
  # Define helper functions ----
  
  # Calculate area in hectares from fraction and cell area
  calc_area <- function(x) {
    raster::overlay(x, Cell_area, fun = function(x, y) {(x * y / 10000)})
  }
  
  # Calculate C stock from area and potential C density
  calc_prim_stock <- function(x) {
    raster::overlay(x, Pot_C_Density, fun = function(x, y) {(x * y * 10)})
  }
  
  # Calculate C stock from area and secondary C density
  calc_sec_stock <- function(x) {
    raster::overlay(x, Sec_C_Density, fun = function(x, y) {(x * y * 10)})
  }
  
  # Extract C stock by region
  extract_C_stock <- function(x, y) {
    C_stock_df <- dplyr::as_tibble(raster::extract(
      x, RegionNames, fun = sum, na.rm = TRUE
    ))
    base::names(C_stock_df) <- YearNames
    
    C_stock <- C_stock_df |>
      dplyr::mutate(RowName = row.names(C_stock_df)) |>
      dplyr::left_join(
        RegionNames |>
          dplyr::mutate(RowName = row.names(RegionNames)) |>
          sf::st_set_geometry(NULL),
        by = "RowName"
      ) |>
      dplyr::select(-RowName) |>
      tidyr::pivot_longer(-Region_name, names_to = "Year",
                           values_to = "C_stock_Tg") |>
      dplyr::mutate(Land_Use = y)
    
    return(C_stock)
  }
  
  # Extract area by region
  extract_area <- function(x, y) {
    area_df <- dplyr::as_tibble(raster::extract(
      x, RegionNames, fun = sum, na.rm = TRUE
    ))
    base::names(area_df) <- YearNames
    
    area <- area_df |>
      dplyr::mutate(RowName = row.names(area_df)) |>
      dplyr::left_join(
        RegionNames |>
          dplyr::mutate(RowName = row.names(RegionNames)) |>
          sf::st_set_geometry(NULL),
        by = "RowName"
      ) |>
      dplyr::select(-RowName) |>
      tidyr::pivot_longer(-Region_name, names_to = "Year",
                           values_to = "Area_Mha") |>
      dplyr::mutate(Land_Use = y)
    
    return(area)
  }
  
  # Function to prepare raster layer (set NoData, replace NA)
  prepare_raster_layer <- function(r) {
    if (is.na(r@file@nodatavalue)) {
      r@file@nodatavalue <- 0
    }
    r[is.na(r)] <- r@file@nodatavalue
    return(r)
  }
  
  # Function to process RasterStack or RasterBrick
  prepare_raster <- function(r) {
    if (methods::is(r, "RasterLayer")) {
      r <- prepare_raster_layer(r)
    } else if (methods::is(r, "RasterStack") || methods::is(r, "RasterBrick")) {
      for (i in 1:raster::nlayers(r)) {
        r[[i]] <- prepare_raster_layer(r[[i]])
      }
    } else {
      stop("Unsupported raster type")
    }
    return(r)
  }
  
  # Load general maps ----
  message("Loading LUH2 base layers...")
  
  Pot_C_Density <- raster::raster(
    file.path(L_files_path, "Hurtt LUC/LUH2 v2h/staticData_quarterdeg.nc"),
    varname = "ptbio" # kg C / m^2
  )
  
  Cell_area <- raster::raster(
    file.path(L_files_path, "Hurtt LUC/LUH2 v2h/staticData_quarterdeg.nc"),
    varname = "carea" # km^2
  )
  
  Sec_C_Density <- raster::brick(
    file.path(L_files_path, "Hurtt LUC/LUH2 v2h/states.nc"),
    varname = "secmb" # kg C / m^2
  )
  
  # Subset study period
  Sec_C_Density <- raster::subset(Sec_C_Density, study_period)
  Sec_C_Density@z$Date <- Sec_C_Density@z$Date[study_period]
  
  # Load state maps ----
  message("Loading LUH2 state layers...")
  
  Prim_Forested <- raster::brick(
    file.path(L_files_path, "Hurtt LUC/LUH2 v2h/states.nc"),
    varname = "primf"
  )
  Prim_nonforested <- raster::brick(
    file.path(L_files_path, "Hurtt LUC/LUH2 v2h/states.nc"),
    varname = "primn"
  )
  Sec_Forested <- raster::brick(
    file.path(L_files_path, "Hurtt LUC/LUH2 v2h/states.nc"),
    varname = "secdf"
  )
  Sec_nonforested <- raster::brick(
    file.path(L_files_path, "Hurtt LUC/LUH2 v2h/states.nc"),
    varname = "secdn"
  )
  Managed_Pasture <- raster::brick(
    file.path(L_files_path, "Hurtt LUC/LUH2 v2h/states.nc"),
    varname = "pastr"
  )
  Rangeland <- raster::brick(
    file.path(L_files_path, "Hurtt LUC/LUH2 v2h/states.nc"),
    varname = "range"
  )
  C3_ann <- raster::brick(
    file.path(L_files_path, "Hurtt LUC/LUH2 v2h/states.nc"),
    varname = "c3ann"
  )
  C3_per <- raster::brick(
    file.path(L_files_path, "Hurtt LUC/LUH2 v2h/states.nc"),
    varname = "c3per"
  )
  C4_ann <- raster::brick(
    file.path(L_files_path, "Hurtt LUC/LUH2 v2h/states.nc"),
    varname = "c4ann"
  )
  C4_per <- raster::brick(
    file.path(L_files_path, "Hurtt LUC/LUH2 v2h/states.nc"),
    varname = "c4per"
  )
  C3_N_fix <- raster::brick(
    file.path(L_files_path, "Hurtt LUC/LUH2 v2h/states.nc"),
    varname = "c3nfx"
  )
  Urban <- raster::brick(
    file.path(L_files_path, "Hurtt LUC/LUH2 v2h/states.nc"),
    varname = "urban"
  )
  
  # Subset all layers to study period ----
  message("Subsetting layers to study period...")
  
  Sec_Forested <- Sec_Forested[[study_period]]
  Sec_Forested@z$Date <- Sec_Forested@z$Date[study_period]
  
  Sec_nonforested <- Sec_nonforested[[study_period]]
  Sec_nonforested@z$Date <- Sec_nonforested@z$Date[study_period]
  
  Prim_Forested <- raster::subset(Prim_Forested, study_period)
  Prim_Forested@z$Date <- Prim_Forested@z$Date[study_period]
  
  Prim_nonforested <- raster::subset(Prim_nonforested, study_period)
  Prim_nonforested@z$Date <- Prim_nonforested@z$Date[study_period]
  
  Managed_Pasture <- raster::subset(Managed_Pasture, study_period)
  Managed_Pasture@z$Date <- Managed_Pasture@z$Date[study_period]
  
  Rangeland <- raster::subset(Rangeland, study_period)
  Rangeland@z$Date <- Rangeland@z$Date[study_period]
  
  C3_ann <- raster::subset(C3_ann, study_period)
  C3_ann@z$Date <- C3_ann@z$Date[study_period]
  
  C3_per <- raster::subset(C3_per, study_period)
  C3_per@z$Date <- C3_per@z$Date[study_period]
  
  C4_ann <- raster::subset(C4_ann, study_period)
  C4_ann@z$Date <- C4_ann@z$Date[study_period]
  
  C4_per <- raster::subset(C4_per, study_period)
  C4_per@z$Date <- C4_per@z$Date[study_period]
  
  C3_N_fix <- raster::subset(C3_N_fix, study_period)
  C3_N_fix@z$Date <- C3_N_fix@z$Date[study_period]
  
  Urban <- raster::subset(Urban, study_period)
  Urban@z$Date <- Urban@z$Date[study_period]
  
  # Prepare all raster layers ----
  message("Preparing raster layers...")
  
  Sec_Forested <- prepare_raster(Sec_Forested)
  Sec_nonforested <- prepare_raster(Sec_nonforested)
  Prim_Forested <- prepare_raster(Prim_Forested)
  Prim_nonforested <- prepare_raster(Prim_nonforested)
  Managed_Pasture <- prepare_raster(Managed_Pasture)
  Rangeland <- prepare_raster(Rangeland)
  C3_ann <- prepare_raster(C3_ann)
  C3_per <- prepare_raster(C3_per)
  C4_ann <- prepare_raster(C4_ann)
  C4_per <- prepare_raster(C4_per)
  C3_N_fix <- prepare_raster(C3_N_fix)
  Urban <- prepare_raster(Urban)
  Sec_C_Density <- prepare_raster(Sec_C_Density)
  
  # Calculate land use areas ----
  message("Calculating land use areas...")
  
  Prim_Forested_area_br <- calc_area(Prim_Forested)
  Prim_nonforested_area_br <- calc_area(Prim_nonforested)
  Sec_Forested_area_br <- calc_area(Sec_Forested)
  Sec_nonforested_area_br <- calc_area(Sec_nonforested)
  Managed_Pasture_area_br <- calc_area(Managed_Pasture)
  Rangeland_area_br <- calc_area(Rangeland)
  C3_ann_area_br <- calc_area(C3_ann)
  C3_per_area_br <- calc_area(C3_per)
  C4_ann_area_br <- calc_area(C4_ann)
  C4_per_area_br <- calc_area(C4_per)
  C3_N_fix_area_br <- calc_area(C3_N_fix)
  Urban_area_br <- calc_area(Urban)
  
  # Calculate C stocks ----
  message("Calculating carbon stocks...")
  
  Prim_Forested_C_stock_br <- calc_prim_stock(Prim_Forested_area_br)
  Prim_nonforested_C_stock_br <- calc_prim_stock(Prim_nonforested_area_br)
  Sec_Forested_C_stock_br <- calc_sec_stock(Sec_Forested_area_br)
  Sec_nonforested_C_stock_br <- calc_sec_stock(Sec_nonforested_area_br)
  
  # Extract and join C stocks and areas by region ----
  message("Extracting data by region...")
  
  Stock_area <- dplyr::left_join(
    dplyr::bind_rows(
      extract_area(Prim_Forested_area_br, "Primary_forested"),
      extract_area(Prim_nonforested_area_br, "Primary_nonforested"),
      extract_area(Sec_Forested_area_br, "Secondary_forested"),
      extract_area(Sec_nonforested_area_br, "Secondary_nonforested"),
      extract_area(Managed_Pasture_area_br, "Managed_pasture"),
      extract_area(Rangeland_area_br, "Rangeland"),
      extract_area(C3_ann_area_br, "C3_ann"),
      extract_area(C3_per_area_br, "C3_per"),
      extract_area(C4_ann_area_br, "C4_ann"),
      extract_area(C4_per_area_br, "C4_per"),
      extract_area(C3_N_fix_area_br, "C3_N_fix"),
      extract_area(Urban_area_br, "Urban")
    ) |> dplyr::mutate(Year = as.numeric(Year)),
    dplyr::bind_rows(
      extract_C_stock(Prim_Forested_C_stock_br, "Primary_forested"),
      extract_C_stock(Prim_nonforested_C_stock_br, "Primary_nonforested"),
      extract_C_stock(Sec_Forested_C_stock_br, "Secondary_forested"),
      extract_C_stock(Sec_nonforested_C_stock_br, "Secondary_nonforested")
    ) |> dplyr::mutate(Year = as.numeric(Year)),
    by = c("Region_name", "Year", "Land_Use")
  ) |>
    dplyr::mutate(
      C_stock_Tg = dplyr::if_else(is.na(C_stock_Tg), 0, C_stock_Tg),
      C_density_Mg_ha = C_stock_Tg / Area_Mha
    ) |>
    dplyr::mutate(
      LandUse = dplyr::case_when(
        Land_Use == "Primary_forested" ~ "Forest",
        Land_Use == "Primary_nonforested" ~ "Grassland_schrubland",
        Land_Use == "Secondary_forested" ~ "Forest",
        Land_Use == "Secondary_nonforested" ~ "Grassland_schrubland",
        Land_Use == "Managed_pasture" ~ "Pasture",
        Land_Use == "Rangeland" ~ "Pasture",
        Land_Use == "C3_ann" ~ "Cropland",
        Land_Use == "C3_per" ~ "Cropland",
        Land_Use == "C4_ann" ~ "Cropland",
        Land_Use == "C4_per" ~ "Cropland",
        Land_Use == "C3_N_fix" ~ "Cropland",
        Land_Use == "Urban" ~ "Urban"
      ),
      Disturbance = dplyr::case_when(
        Land_Use == "Primary_forested" ~ "Primary",
        Land_Use == "Primary_nonforested" ~ "Primary",
        Land_Use == "Secondary_forested" ~ "Secondary",
        Land_Use == "Secondary_nonforested" ~ "Secondary",
        Land_Use == "Managed_pasture" ~ "Managed_pasture",
        Land_Use == "Rangeland" ~ "Rangeland",
        Land_Use == "C3_ann" ~ "Cropland",
        Land_Use == "C3_per" ~ "Cropland",
        Land_Use == "C4_ann" ~ "Cropland",
        Land_Use == "C4_per" ~ "Cropland",
        Land_Use == "C3_N_fix" ~ "Cropland",
        Land_Use == "Urban" ~ "Urban"
      )
    ) |>
    dplyr::filter(Area_Mha != 0)
  
  message("LUH2 extraction complete!")
  
  return(Stock_area)
}
