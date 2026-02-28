#' Calculate Potential Net Primary Production (NPP)
#'
#' Calculates potential NPP using various models including Miami, NCEAS, and
#' Rosenzweig. All model coefficients are read from `NPP_model_coefs` (loaded
#' by `load_general_data()`) rather than hardcoded in the function.
#'
#' @param Dataset A data frame containing climate data with columns:
#'   \describe{
#'     \item{TMP}{Mean annual temperature (degrees C).}
#'     \item{WaterInput_mm}{Total water input: precipitation + irrigation (mm).}
#'     \item{AET_mm}{Actual evapotranspiration (mm).}
#'   }
#'
#' @return A data frame with calculated NPP values from different models
#'   in Mg DM/ha.
#'
#' @details
#' Requires the following objects from `load_general_data()`:
#' - `NPP_model_coefs` — model parameters (Miami, NCEAS, Rosenzweig)
#' - `Residue_kgC_kgDM_Wo` — C content of woody residues for DM conversion
#' - `Residue_kgC_kgDM_W` — C content of non-tree residues for DM conversion
#'
#' Models implemented:
#' - **Miami** (Lieth 1975): min of temperature and precipitation limits
#' - **NCEAS tree** (Del Grosso et al. 2008, Table 1): min(F_MAP, F_MAT)
#'   for both TNPP and ANPP
#' - **NCEAS non-tree** (Del Grosso et al. 2008, Table 2): precipitation-only
#'   saturating functions for TNPP and ANPP
#' - **Rosenzweig** (1968): log-linear model based on AET
#'
#' @references
#' Lieth, H. (1975) Modeling the Primary Productivity of the World. In:
#'   Primary Productivity of the Biosphere, Springer.
#'
#' Del Grosso, S. et al. (2008) Global potential net primary production
#'   predicted from vegetation class, precipitation, and temperature.
#'   Ecology 89:2117-2126.
#'
#' Rosenzweig, M.L. (1968) Net Primary Productivity of Terrestrial
#'   Communities: Prediction from Climatological Data. Am Nat 102:67-74.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' load_general_data()
#' climate_data <- data.frame(TMP = 15, WaterInput_mm = 800, AET_mm = 700)
#' npp_results <- calculate_potential_npp(climate_data)
#' }
calculate_potential_npp <- function(Dataset) {

  # --- Extract model coefficients from NPP_model_coefs table ----------------
  # Helper to look up a single coefficient value by Model, Component, Parameter
  .npp_coef <- function(model, param, component = NULL) {
    mask <- NPP_model_coefs$Model == model &
      NPP_model_coefs$Parameter == param
    if (!is.null(component)) {
      mask <- mask & NPP_model_coefs$Component == component
    }
    val <- NPP_model_coefs$Value[mask]
    if (length(val) != 1L) {
      stop("NPP_model_coefs: no unique match for Model='", model,
           "', Parameter='", param, "'",
           if (!is.null(component)) paste0(", Component='", component, "'"),
           call. = FALSE)
    }
    val
  }

  # Miami model (Lieth 1975)
  miami_T_max  <- .npp_coef("Miami", "Max_gCm2yr", "F_MAT")
  miami_T_mid  <- .npp_coef("Miami", "Midpoint",   "F_MAT")
  miami_T_rate <- .npp_coef("Miami", "Rate",        "F_MAT")
  miami_P_max  <- .npp_coef("Miami", "Max_gCm2yr", "F_MAP")
  miami_P_rate <- .npp_coef("Miami", "Rate",        "F_MAP")

  # NCEAS tree TNPP (Del Grosso et al. 2008, Table 1)
  tree_T_coef   <- .npp_coef("NCEAS_tree_TNPP", "Coefficient")
  tree_T_exp    <- .npp_coef("NCEAS_tree_TNPP", "Exponent")
  tree_T_div    <- .npp_coef("NCEAS_tree_TNPP", "Exp_divisor")
  tree_T_max    <- .npp_coef("NCEAS_tree_TNPP", "Max_gCm2yr")
  tree_T_mid    <- .npp_coef("NCEAS_tree_TNPP", "Midpoint")
  tree_T_rate   <- .npp_coef("NCEAS_tree_TNPP", "Rate")

  # NCEAS tree ANPP (Del Grosso et al. 2008, Table 1)
  treeA_coef    <- .npp_coef("NCEAS_tree_ANPP", "Coefficient")
  treeA_exp     <- .npp_coef("NCEAS_tree_ANPP", "Exponent")
  treeA_div     <- .npp_coef("NCEAS_tree_ANPP", "Exp_divisor")
  treeA_max     <- .npp_coef("NCEAS_tree_ANPP", "Max_gCm2yr")
  treeA_mid     <- .npp_coef("NCEAS_tree_ANPP", "Midpoint")
  treeA_rate    <- .npp_coef("NCEAS_tree_ANPP", "Rate")

  # NCEAS non-tree TNPP (Del Grosso et al. 2008, Table 2)
  nt_T_max      <- .npp_coef("NCEAS_nontree_TNPP", "Max_gCm2yr")
  nt_T_rate     <- .npp_coef("NCEAS_nontree_TNPP", "Rate")

  # NCEAS non-tree ANPP (Del Grosso et al. 2008, Table 2)
  nt_A_max      <- .npp_coef("NCEAS_nontree_ANPP", "Max_gCm2yr")
  nt_A_rate     <- .npp_coef("NCEAS_nontree_ANPP", "Rate")

  # Rosenzweig (1968)
  rosen_slope   <- .npp_coef("Rosenzweig", "Slope")
  rosen_int     <- .npp_coef("Rosenzweig", "Intercept")

  # --- Compute NPP models ---------------------------------------------------
  Dataset |>
    dplyr::mutate(
      # Miami model: temperature and precipitation components
      NPPT_Miami_MgDMha = (miami_T_max / (1 + exp(miami_T_mid - miami_T_rate * TMP))) / 100,
      NPPP_Miami_MgDMha = (miami_P_max * (1 - exp(-miami_P_rate * WaterInput_mm))) / 100,
      # NCEAS tree-dominated TNPP: F(MAP) and F(MAT)
      FMAP_TNPP_NCEAS_MgDMha = ((tree_T_coef * WaterInput_mm^tree_T_exp) / exp(tree_T_div * WaterInput_mm)) / (100 * Residue_kgC_kgDM_Wo),
      FMAT_TNPP_NCEAS_MgDMha = tree_T_max / (1 + exp(tree_T_mid - tree_T_rate * TMP)) / (100 * Residue_kgC_kgDM_Wo),
      # NCEAS tree-dominated ANPP: F(MAP) and F(MAT)
      FMAP_ANPP_NCEAS_MgDMha = ((treeA_coef * WaterInput_mm^treeA_exp) / exp(treeA_div * WaterInput_mm)) / (100 * Residue_kgC_kgDM_Wo),
      FMAT_ANPP_NCEAS_MgDMha = treeA_max / (1 + exp(treeA_mid - treeA_rate * TMP)) / (100 * Residue_kgC_kgDM_Wo),
      # NCEAS non-tree TNPP and ANPP (precipitation-only)
      TNPP_NCEAS_MgDMha = nt_T_max * (1 - exp(-nt_T_rate * WaterInput_mm)) / (100 * Residue_kgC_kgDM_W),
      ANPP_NCEAS_MgDMha = nt_A_max * (1 - exp(-nt_A_rate * WaterInput_mm)) / (100 * Residue_kgC_kgDM_W),
      # Combined tree models: min of temperature and precipitation limits
      TNPP_tree_NCEAS_MgDMha = pmin(FMAP_TNPP_NCEAS_MgDMha, FMAT_TNPP_NCEAS_MgDMha),
      ANPP_tree_NCEAS_MgDMha = pmin(FMAP_ANPP_NCEAS_MgDMha, FMAT_ANPP_NCEAS_MgDMha),
      # Miami combined: min of temperature and precipitation
      NPP_Miami_MgDMha = pmin(NPPT_Miami_MgDMha, NPPP_Miami_MgDMha),
      # Rosenzweig (AET-based)
      NPP_Rosenzweig_MgDMha = 10^(rosen_slope * log10(AET_mm) + rosen_int) / 100,
      # Derived root:shoot ratios from NCEAS models
      RS_ratio_NCEAS = (TNPP_NCEAS_MgDMha - ANPP_NCEAS_MgDMha) / ANPP_NCEAS_MgDMha,
      RS_ratio_tree_NCEAS = (TNPP_tree_NCEAS_MgDMha - ANPP_tree_NCEAS_MgDMha) / ANPP_tree_NCEAS_MgDMha
    )
}

#' Calculate Crop Above-Ground Residue Biomass
#'
#' Estimates crop residue (straw, stover, etc.) dry matter from yield data
#' using an ensemble of IPCC 2019 linear models, Biomass_coefs ratios, and
#' context-dependent adjustments for irrigation, N input, and modern variety
#' adoption.
#'
#' @description
#' Three estimation methods are combined:
#'
#' 1. **IPCC linear model** (IPCC 2019 Refinement, Vol.4, Ch.11, Table 11.1a):
#'    `AG_residue_DM = Slope * Yield_DM + Intercept`
#'
#' 2. **Biomass_coefs ratio model** (package default):
#'    `Residue_FM = Yield_FM * kg_residue_kg_product_FM`
#'
#' 3. **Modern variety adjustment** (Krausmann et al. 2013; Evenson & Gollin
#'    2003): Historical correction for pre-Green-Revolution varieties that had
#'    lower harvest index (more residue per unit product).
#'
#' The final estimate is a weighted mean of methods 1 and 2, with irrigation
#' and variety-era adjustments applied.
#'
#' @param Dataset Data frame with crop production data. Required columns:
#'   \describe{
#'     \item{Name_biomass}{Crop name matching Biomass_coefs classification.}
#'     \item{Prod_ygpit_Mg}{Production in Mg fresh matter.}
#'     \item{Area_ygpit_ha}{Harvested area in hectares.}
#'     \item{Year}{Year of production (for modern variety adjustment).}
#'     \item{region_HANPP}{Region name matching Modern_variety_adoption.}
#'     \item{Water_regime}{One of "Irrigated", "Rainfed", or "Mixed".}
#'   }
#' @param w_ipcc Numeric weight for the IPCC linear model in the ensemble
#'   (0-1). Default 0.5. The Biomass_coefs ratio gets weight `1 - w_ipcc`.
#'
#' @return Data frame with added columns:
#'   \describe{
#'     \item{Prod_MgDM}{Product dry matter (Mg).}
#'     \item{Residue_MgDM}{Estimated above-ground residue dry matter (Mg).}
#'     \item{Yield_DM_Mgha}{Yield in Mg DM per hectare (intermediate).}
#'   }
#'
#' @details
#' **IPCC linear model**: For each crop mapped via `IPCC_crop_mapping`, the
#' per-hectare residue is estimated as:
#' `Residue_IPCC_Mgha = Slope_AG * Yield_DM_Mgha + Intercept_AG_MgDMha`
#' This captures the empirical finding (Lassaletta et al. 2014) that residue
#' production is not purely proportional to yield — there is a base structural
#' component (intercept).
#'
#' **Irrigation adjustment**: Irrigated crops typically have higher harvest
#' index (Sadras 2007), meaning less residue per unit product. The adjustment
#' factor (default 0.90 for irrigated) is from the `Irrigation_adj` table.
#'
#' **Modern variety correction**: Pre-Green-Revolution varieties had lower
#' harvest index. The `Modern_variety_adoption` table provides crop-group-
#' specific regional time-series of adoption rates from Evenson & Gollin
#' (2003), interpolated to annual resolution. Combined with `HI_crop_ranges`
#' (crop-specific HI gap factors), this computes:
#' `HI_correction = 1 + (1 - Modern_share) * (HI_gap_factor - 1)`.
#'
#' **Ensemble**: The final residue estimate blends both methods:
#' `Residue_MgDM = w_ipcc * IPCC_estimate + (1 - w_ipcc) * ratio_estimate`
#'
#' Requires these objects from `load_general_data()`:
#' - `Biomass_coefs` (with `Product_kgDM_kgFM`, `Residue_kgDM_kgFM`,
#'   `kg_residue_kg_product_FM`)
#' - `IPCC_residue_coefs`, `IPCC_crop_mapping` (with `crop_group` column)
#' - `Irrigation_adj`, `Modern_variety_adoption`, `HI_crop_ranges`
#'
#' @export
#'
#' @references
#' IPCC (2019) 2019 Refinement to the 2006 IPCC Guidelines for National
#'   Greenhouse Gas Inventories, Volume 4, Chapter 11, Table 11.1a.
#'
#' Krausmann et al. (2013) Global human appropriation of net primary
#'   production doubled in the 20th century. PNAS 110:10324-10329.
#'
#' Lassaletta et al. (2014) 50 year trends in nitrogen use efficiency of
#'   world cropping systems. Biogeosciences 11:2889-2907.
#'
#' Sadras (2007) Evolutionary aspects of the trade-off between seed size
#'   and number in crops. Field Crops Research 100:125-138.
#'
#' @examples
#' \dontrun{
#' load_general_data()
#' crop_data |>
#'   calculate_crop_residues(w_ipcc = 0.5)
#' }
calculate_crop_residues <- function(Dataset, w_ipcc = 0.5) {

  # --- Join Biomass_coefs for DM conversion and residue:product ratio --------
  Dataset <- Dataset |>
    dplyr::left_join(
      Biomass_coefs |>
        dplyr::select(Name_biomass, Product_kgDM_kgFM, Residue_kgDM_kgFM,
                      kg_residue_kg_product_FM),
      by = "Name_biomass"
    )

  # --- Basic DM conversions --------------------------------------------------
  Dataset <- Dataset |>
    dplyr::mutate(
      Prod_ygpit_Mg = dplyr::if_else(is.na(Prod_ygpit_Mg),
                                     0,
                                     as.numeric(as.character(Prod_ygpit_Mg))),
      Prod_MgDM = Prod_ygpit_Mg * Product_kgDM_kgFM,
      Yield_DM_Mgha = dplyr::if_else(
        Area_ygpit_ha > 0,
        Prod_MgDM / Area_ygpit_ha,
        0
      )
    )

  # --- Join IPCC residue coefficients ----------------------------------------
  Dataset <- Dataset |>
    dplyr::left_join(IPCC_crop_mapping, by = "Name_biomass") |>
    dplyr::left_join(
      IPCC_residue_coefs |>
        dplyr::select(IPCC_crop, Slope_AG, Intercept_AG_MgDMha),
      by = "IPCC_crop"
    )

  # --- Method 1: IPCC linear model (per hectare, then scale) -----------------
  # AG_residue_DM (Mg/ha) = Slope * Yield_DM (Mg/ha) + Intercept
  Dataset <- Dataset |>
    dplyr::mutate(
      Residue_IPCC_Mgha = dplyr::if_else(
        !is.na(Slope_AG),
        Slope_AG * Yield_DM_Mgha + Intercept_AG_MgDMha,
        NA_real_
      ),
      # Ensure non-negative
      Residue_IPCC_Mgha = pmax(Residue_IPCC_Mgha, 0),
      Residue_IPCC_MgDM = Residue_IPCC_Mgha * Area_ygpit_ha
    )

  # --- Method 2: Biomass_coefs ratio model -----------------------------------
  Dataset <- Dataset |>
    dplyr::mutate(
      Residue_ratio_MgFM = Prod_ygpit_Mg * kg_residue_kg_product_FM,
      Residue_ratio_MgDM = Residue_ratio_MgFM * Residue_kgDM_kgFM
    )

  # --- Irrigation adjustment (requires Water_regime column) ------------------
  Dataset <- Dataset |>
    dplyr::left_join(
      Irrigation_adj |>
        dplyr::select(Water_regime, Residue_ratio_factor),
      by = "Water_regime"
    ) |>
    dplyr::mutate(
      Residue_ratio_factor = tidyr::replace_na(Residue_ratio_factor, 1.0)
    )

  # --- Modern variety HI correction (requires Year, region_HANPP) ------------
  # Uses crop-group-specific adoption rates from Evenson & Gollin (2003) and
  # crop-specific HI gap factors from HI_crop_ranges to compute the
  # HI_correction_factor.  Falls back to 1.0 when crop_group is not available.
  Dataset <- Dataset |>
    dplyr::left_join(
      Modern_variety_adoption |>
        dplyr::select(region_HANPP, crop_group, Year, Modern_share),
      by = c("region_HANPP", "crop_group", "Year")
    ) |>
    dplyr::left_join(
      HI_crop_ranges |>
        dplyr::select(crop_group, HI_gap_factor),
      by = "crop_group"
    ) |>
    dplyr::mutate(
      Modern_share     = tidyr::replace_na(Modern_share, 1.0),
      HI_gap_factor    = tidyr::replace_na(HI_gap_factor, 1.0),
      HI_correction_factor = 1 + (1 - Modern_share) *
        (HI_gap_factor - 1)
    )

  # --- Ensemble: weighted mean of both methods -------------------------------
  # Apply irrigation and variety corrections to the ratio-based estimate
  Dataset <- Dataset |>
    dplyr::mutate(
      # Adjust ratio-based residue for irrigation and variety era
      Residue_ratio_adj_MgDM = Residue_ratio_MgDM *
        Residue_ratio_factor * HI_correction_factor,
      # IPCC model already implicitly captures HI via slope; only irrigate adj
      Residue_IPCC_adj_MgDM = dplyr::if_else(
        !is.na(Residue_IPCC_MgDM),
        Residue_IPCC_MgDM * Residue_ratio_factor * HI_correction_factor,
        NA_real_
      ),
      # Weighted ensemble (use ratio-only when IPCC not available)
      Residue_MgDM = dplyr::if_else(
        !is.na(Residue_IPCC_adj_MgDM),
        w_ipcc * Residue_IPCC_adj_MgDM +
          (1 - w_ipcc) * Residue_ratio_adj_MgDM,
        Residue_ratio_adj_MgDM
      ),
      # Floor at zero, replace NA with 0 (fallow, etc.)
      Residue_MgDM = pmax(tidyr::replace_na(Residue_MgDM, 0), 0)
    )

  # --- Clean up intermediate columns -----------------------------------------
  Dataset |>
    dplyr::select(
      -dplyr::any_of(c(
        "IPCC_crop", "crop_group", "Slope_AG", "Intercept_AG_MgDMha",
        "Residue_IPCC_Mgha", "Residue_IPCC_MgDM", "Residue_IPCC_adj_MgDM",
        "Residue_ratio_MgFM", "Residue_ratio_MgDM", "Residue_ratio_adj_MgDM",
        "Residue_ratio_factor", "HI_correction_factor",
        "Modern_share", "HI_gap_factor"
      ))
    )
}


#' Calculate Crop Below-Ground (Root) Biomass
#'
#' Estimates root biomass dry matter from above-ground biomass using an
#' ensemble of IPCC root:shoot ratios, reference root biomass values,
#' and adjustments for N input and irrigation regime.
#'
#' @description
#' Three estimation approaches are combined:
#'
#' 1. **IPCC root:shoot ratio** (IPCC 2019 Refinement, Vol.4, Ch.11):
#'    `Root_DM = Aerial_DM * RS_ratio`, where RS_ratio is adjusted for
#'    N input and irrigation from the `IPCC_root_coefs` table.
#'
#' 2. **Reference root biomass** (Biomass_coefs `BG_Biomass_kgDM_ha`):
#'    A fixed per-hectare root biomass value, independent of yield.
#'    `Root_ref = BG_Biomass_kgDM_ha / 1000 * Area_ha`
#'
#' 3. **Biomass_coefs RS fallback**: When IPCC coefficients are not
#'    available, uses the `Root_Shoot_ratio` from Biomass_coefs.
#'
#' The final estimate averages the RS-based and reference-based approaches,
#' capped at 3x the default RS ratio to prevent unrealistic values.
#'
#' @param Dataset Data frame with above-ground biomass already calculated.
#'   Required columns:
#'   \describe{
#'     \item{Name_biomass}{Crop name matching Biomass_coefs.}
#'     \item{Prod_MgDM}{Product dry matter (Mg).}
#'     \item{Residue_MgDM}{Residue dry matter (Mg).}
#'     \item{Area_ygpit_ha}{Harvested area in hectares.}
#'     \item{Water_regime}{"Irrigated", "Rainfed", or "Mixed".}
#'     \item{N_input_kgha}{Nitrogen application rate (kg N/ha/yr).}
#'   }
#' @param w_ref Numeric weight for the reference root biomass approach in
#'   the ensemble (0-1). Default 0.5. The RS-based approach gets weight
#'   `1 - w_ref`.
#'
#' @return Data frame with added column:
#'   \describe{
#'     \item{Root_MgDM}{Estimated below-ground root dry matter (Mg).}
#'   }
#'
#' @details
#' **N-input adjustment**: Higher nitrogen availability decreases root
#' allocation due to functional equilibrium (Poorter & Nagel 2000). The
#' `N_input_RS_adj` table classifies N rates into 5 classes with
#' multiplicative RS factors (0.80 for >200 kg N/ha to 1.20 for <20 kg
#' N/ha). This generic factor is then scaled by the crop-group-specific
#' `RS_N_sensitivity` from `Crop_RS_N_response` (e.g., legumes respond
#' minimally to soil N because they fix their own).
#'
#' **Irrigation adjustment**: Irrigated crops develop shallower root systems
#' (Benjamin et al. 2014). Factor from `Irrigation_adj` table (default
#' 0.85 for irrigated).
#'
#' **RS selection priority**: IPCC_root_coefs > Biomass_coefs fallback.
#' When irrigation or N-input columns are present, the IPCC table provides
#' context-specific RS values; otherwise the default `RS_default` is used.
#'
#' **Ensemble and cap**: Final root = weighted average of RS-based and
#' reference-based estimates, capped at 3x the default RS ratio times
#' aerial biomass.
#'
#' Requires from `load_general_data()`:
#' - `Biomass_coefs` (Root_Shoot_ratio, BG_Biomass_kgDM_ha)
#' - `IPCC_root_coefs`, `IPCC_crop_mapping` (with `crop_group` column)
#' - `N_input_RS_adj`, `Irrigation_adj`, `Crop_RS_N_response`
#'
#' @export
#'
#' @references
#' IPCC (2019) 2019 Refinement to the 2006 IPCC Guidelines for National
#'   Greenhouse Gas Inventories, Volume 4, Chapter 11.
#'
#' Bolinder, M.A. et al. (2007) Root biomass and shoot to root ratios as
#'   related to above ground biomass. J Agric Sci 145:127-137.
#'
#' Poorter, H. & Nagel, O. (2000) The role of biomass allocation in the
#'   growth response of plants to different levels of light, CO2, nutrients
#'   and water. New Phytologist 147:135-147.
#'
#' Benjamin, J.G. et al. (2014) Water deficit effects on root distribution
#'   of soybean, field pea and chickpea. Agronomy Journal 106:2033-2040.
#'
#' @examples
#' \dontrun{
#' load_general_data()
#' crop_data |>
#'   calculate_crop_residues() |>
#'   calculate_crop_roots(w_ref = 0.5)
#' }
calculate_crop_roots <- function(Dataset, w_ref = 0.5) {

  # --- Join Biomass_coefs for default RS and BG reference --------------------
  Dataset <- Dataset |>
    dplyr::select(-dplyr::any_of(c("Root_Shoot_ratio",
                                   "BG_Biomass_kgDM_ha"))) |>
    dplyr::left_join(
      Biomass_coefs |>
        dplyr::select(Name_biomass, Root_Shoot_ratio, BG_Biomass_kgDM_ha),
      by = "Name_biomass"
    )

  # --- Join IPCC root coefficients -------------------------------------------
  Dataset <- Dataset |>
    dplyr::select(-dplyr::any_of(c("IPCC_crop", "crop_group"))) |>
    dplyr::left_join(IPCC_crop_mapping, by = "Name_biomass")

  Dataset <- Dataset |>
    dplyr::left_join(
      IPCC_root_coefs |>
        dplyr::select(IPCC_crop, RS_default, RS_low_N, RS_high_N,
                      RS_irrigated, RS_rainfed, BG_ref_MgDMha),
      by = "IPCC_crop"
    )

  # --- N-input based RS adjustment (requires N_input_kgha column) -----------
  # Use N_input_RS_adj table from load_general_data() for data-driven lookup
  Dataset <- Dataset |>
    dplyr::mutate(
      N_RS_factor_raw = findInterval(
        N_input_kgha,
        vec = N_input_RS_adj$N_input_min
      )
    ) |>
    dplyr::mutate(
      # Clamp to valid range [1, nrow(N_input_RS_adj)]
      N_RS_factor_raw = pmax(1L, pmin(N_RS_factor_raw,
                                      nrow(N_input_RS_adj))),
      N_RS_factor_raw = N_input_RS_adj$RS_adjustment[N_RS_factor_raw]
    )

  # --- Crop-group-specific RS N sensitivity (Poorter & Nagel 2000) ----------
  # Scale the generic N_RS_factor by crop-group sensitivity.
  # Legumes (low sensitivity) barely respond; cereals respond fully.
  Dataset <- Dataset |>
    dplyr::left_join(
      Crop_RS_N_response |>
        dplyr::select(crop_group, RS_N_sensitivity),
      by = "crop_group"
    ) |>
    dplyr::mutate(
      RS_N_sensitivity = tidyr::replace_na(RS_N_sensitivity, 1.0),
      N_RS_factor = 1 + (N_RS_factor_raw - 1) * RS_N_sensitivity
    )

  # --- Irrigation-based RS (requires Water_regime column) -------------------
  Dataset <- Dataset |>
    dplyr::left_join(
      Irrigation_adj |>
        dplyr::select(Water_regime, RS_ratio_factor),
      by = "Water_regime"
    ) |>
    dplyr::mutate(
      RS_ratio_factor = tidyr::replace_na(RS_ratio_factor, 1.0)
    )

  # --- Compute effective RS ratio --------------------------------------------
  Dataset <- Dataset |>
    dplyr::mutate(
      # Use IPCC default if available, else Biomass_coefs
      RS_base = dplyr::if_else(
        !is.na(RS_default),
        RS_default,
        Root_Shoot_ratio
      ),
      # Apply N and irrigation adjustments
      RS_effective = RS_base * N_RS_factor * RS_ratio_factor,
      # Above-ground total
      Aerial_MgDM = Prod_MgDM + Residue_MgDM
    )

  # --- Method 1: RS ratio approach -------------------------------------------
  Dataset <- Dataset |>
    dplyr::mutate(
      Root_MgDM_RS = Aerial_MgDM * RS_effective
    )

  # --- Method 2: Reference root biomass per hectare --------------------------
  # Use IPCC BG_ref if available, else Biomass_coefs BG_Biomass
  Dataset <- Dataset |>
    dplyr::mutate(
      BG_ref_used = dplyr::case_when(
        !is.na(BG_ref_MgDMha) ~ BG_ref_MgDMha,
        !is.na(BG_Biomass_kgDM_ha) ~ BG_Biomass_kgDM_ha / 1000,
        TRUE ~ NA_real_
      ),
      Root_MgDM_ref = dplyr::if_else(
        !is.na(BG_ref_used),
        BG_ref_used * Area_ygpit_ha,
        NA_real_
      )
    )

  # --- Ensemble: weighted average of RS and reference approaches -------------
  Dataset <- Dataset |>
    dplyr::mutate(
      Root_MgDM = dplyr::case_when(
        # Both available: weighted average
        !is.na(Root_MgDM_RS) & !is.na(Root_MgDM_ref) ~
          (1 - w_ref) * Root_MgDM_RS + w_ref * Root_MgDM_ref,
        # Only RS available
        !is.na(Root_MgDM_RS) ~ Root_MgDM_RS,
        # Only reference available
        !is.na(Root_MgDM_ref) ~ Root_MgDM_ref,
        # Neither
        TRUE ~ 0
      ),
      # Cap at 3x default RS ratio to avoid unrealistic values
      Root_MgDM = dplyr::if_else(
        Aerial_MgDM > 0 & Root_MgDM / Aerial_MgDM > RS_base * 3,
        Aerial_MgDM * RS_base * 3,
        Root_MgDM
      ),
      Root_MgDM = pmax(tidyr::replace_na(Root_MgDM, 0), 0)
    )

  # --- Clean up intermediate columns -----------------------------------------
  Dataset |>
    dplyr::select(
      -dplyr::any_of(c(
        "IPCC_crop", "crop_group", "RS_default", "RS_low_N", "RS_high_N",
        "RS_irrigated", "RS_rainfed", "BG_ref_MgDMha",
        "N_RS_factor_raw", "N_RS_factor", "RS_N_sensitivity",
        "RS_ratio_factor", "RS_base", "RS_effective",
        "Aerial_MgDM", "Root_MgDM_RS", "BG_ref_used", "Root_MgDM_ref",
        "BG_Biomass_kgDM_ha"
      ))
    )
}


#' Calculate Crop Net Primary Production Components
#'
#' Wrapper function that calculates complete crop NPP by calling
#' `calculate_crop_residues()` for above-ground residue estimation and
#' `calculate_crop_roots()` for below-ground root estimation, then
#' assembles total crop NPP.
#'
#' @description
#' This function replaces and enhances the previous `calculate_crop_npp()`
#' by decomposing the estimation into specialized sub-functions that each
#' use IPCC 2019 guidelines, literature-based coefficients, and
#' context-dependent adjustments for irrigation, N input, and variety era.
#'
#' The calculation pipeline is:
#' 1. **Residues**: `calculate_crop_residues()` estimates above-ground
#'    residue biomass from yield using IPCC linear model + ratio ensemble.
#' 2. **Roots**: `calculate_crop_roots()` estimates below-ground biomass
#'    from aerial biomass using IPCC RS ratios + reference values.
#' 3. **Assembly**: Total crop NPP = Product + Residue + Root (all in DM).
#'
#' @param Dataset Data frame with crop area and production data. Required
#'   columns:
#'   \describe{
#'     \item{Name_biomass}{Crop name matching Biomass_coefs.}
#'     \item{Prod_ygpit_Mg}{Production in Mg fresh matter.}
#'     \item{Area_ygpit_ha}{Harvested area in hectares.}
#'     \item{Year}{Production year (for modern variety correction).}
#'     \item{region_HANPP}{Region name (for modern variety correction).}
#'     \item{Water_regime}{"Irrigated", "Rainfed", or "Mixed".}
#'     \item{N_input_kgha}{Nitrogen application rate (kg N/ha/yr).}
#'   }
#' @param w_ipcc Numeric (0-1). Weight for IPCC linear residue model in the
#'   residue estimation ensemble. Default 0.5.
#' @param w_ref Numeric (0-1). Weight for the reference root biomass in the
#'   root estimation ensemble. Default 0.5.
#'
#' @return Data frame with added columns:
#'   \describe{
#'     \item{Prod_MgDM}{Product dry matter (Mg).}
#'     \item{Residue_MgDM}{Above-ground residue dry matter (Mg).}
#'     \item{Root_MgDM}{Below-ground root dry matter (Mg).}
#'     \item{Crop_NPP_MgDM}{Total crop NPP = Prod + Residue + Root (Mg DM).}
#'     \item{Yield_DM_Mgha}{Yield in Mg DM per hectare.}
#'   }
#'   Intermediate coefficient columns from Biomass_coefs are dropped.
#'
#' @details
#' **Backward compatibility**: This function produces the same output columns
#' as the previous `calculate_crop_npp()`. The old function signature using an
#' `HI` parameter for dynamic harvest index is preserved via the legacy alias
#' `Calculate_crop_NPP()`.
#'
#' **Required context columns**: The Dataset must include Year,
#' region_HANPP, Water_regime, and N_input_kgha columns. These drive the
#' IPCC context-dependent adjustments for modern variety era, irrigation
#' regime, and nitrogen input effects on root allocation.
#'
#' Requires from `load_general_data()`:
#' - `Biomass_coefs`
#' - `IPCC_residue_coefs`, `IPCC_root_coefs`, `IPCC_crop_mapping`
#' - `Irrigation_adj`, `Modern_variety_adoption`, `N_input_RS_adj`
#'
#' @export
#'
#' @references
#' IPCC (2019) 2019 Refinement to the 2006 IPCC Guidelines, Vol.4, Ch.11.
#'
#' Bolinder et al. (2007) Root biomass and shoot to root ratios. J Agric
#'   Sci 145:127-137.
#'
#' Krausmann et al. (2013) Global human appropriation of net primary
#'   production. PNAS 110:10324-10329.
#'
#' @examples
#' \dontrun{
#' load_general_data()
#'
#' # Dataset must include all required columns
#' crop_npp <- crop_data |>
#'   calculate_crop_npp(w_ipcc = 0.5, w_ref = 0.5)
#' }
calculate_crop_npp <- function(Dataset, w_ipcc = 0.5, w_ref = 0.5) {

  Dataset |>
    calculate_crop_residues(w_ipcc = w_ipcc) |>
    calculate_crop_roots(w_ref = w_ref) |>
    dplyr::mutate(
      Crop_NPP_MgDM = Prod_MgDM + Residue_MgDM + Root_MgDM
    ) |>
    dplyr::select(
      -dplyr::any_of(c(
        "Product_kgDM_kgFM", "Residue_kgDM_kgFM",
        "kg_residue_kg_product_FM", "Root_Shoot_ratio"
      ))
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
#' npp_nutrients <- calculate_npp_dm_c_n(area_npp_data)
#' }
calculate_npp_dm_c_n <- function(AreaNPP) {
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
#' @param .by Character vector of column names for grouping operations (e.g.,
#'   c("Year", "Region")). Used for scaling weeds calculations. If NULL, no
#'   grouping is applied.
#'
#' @return A data frame with complete cropland NPP components (crop + weeds) in DM, C, N
#' @export
#'
#' @examples
#' \dontrun{
#' # With grouping by Year
#' cropland_npp <- calculate_crop_npp_components(crop_npp_potential, .by = "Year")
#' 
#' # With multiple grouping variables
#' cropland_npp <- calculate_crop_npp_components(crop_npp_potential, 
#'                                                 .by = c("Year", "Region"))
#' 
#' # Without grouping
#' cropland_npp <- calculate_crop_npp_components(crop_npp_potential, .by = NULL)
#' }
calculate_crop_npp_components <- function(Crop_NPPpot, .by = NULL) {
  biomass_coef_cols <- c(
    "Product_kgDM_kgFM",
    "Residue_kgDM_kgFM",
    "Root_kgDM_kgFM",
    "kg_residue_kg_product_FM",
    "Root_Shoot_ratio",
    "Product_kgN_kgDM",
    "Residue_kgN_kgDM",
    "Root_kgN_kgDM",
    "Rhizodeposits_N_kgN_kgRootN",
    "Product_kgC_kgDM",
    "Residue_kgC_kgDM",
    "Root_kgC_kgDM"
  )

  Crop_NPPpot |>
    dplyr::left_join(
      Biomass_coefs |>
        dplyr::select(dplyr::any_of(c("Name_biomass", biomass_coef_cols))),
      by = c("Name_biomass")
    ) |>
    dplyr::left_join(Weed_NPP_Scaling) |>
    dplyr::left_join(Residue_Shares) |>
    dplyr::left_join(Fallow_cover) |>
    dplyr::mutate(.by = dplyr::all_of(.by),
      Scaling_weeds = dplyr::if_else(is.na(Scaling_weeds),
      mean(Scaling_weeds, na.rm = TRUE),
      Scaling_weeds
    )) |>
    (\(x) base::replace(x, is.na(x), 0))() |>
    dplyr::mutate(.by = dplyr::all_of(.by),
      Weeds_AG_MgDM = dplyr::if_else(Name_biomass != "Fallow",
      Area_ygpit_ha * Scaling_weeds * NPPpot_MgDMha / (1 + Root_Shoot_ratio_W),
      Area_ygpit_ha * Fallow_cover_share * NPPpot_MgDMha / (1 + Root_Shoot_ratio_W)
    )) |>
    calculate_npp_dm_c_n() |>
    dplyr::select(-dplyr::any_of(biomass_coef_cols))
}

# Backward-compatible aliases for renamed functions ----
# These ensure existing code in dependent repos (Global, Spain_Hist) continues
# to work after function renaming.

#' @rdname calculate_crop_npp
#' @usage NULL
#' @export
Calculate_crop_NPP <- function(Dataset, HI = NULL, w_ipcc = 0.5,
                                w_ref = 0.5) {
  warning(
    "Calculate_crop_NPP() is deprecated. Use calculate_crop_npp() ",
    "instead.\nThe HI parameter is no longer used. Dynamic HI/RS ",
    "adjustments are now handled internally via IPCC coefficients, ",
    "irrigation, N input, and modern variety tables.\n",
    "Pass Year, region_HANPP, Water_regime, N_input_kgha columns in ",
    "Dataset instead.",
    call. = FALSE
  )
  calculate_crop_npp(Dataset, w_ipcc = w_ipcc, w_ref = w_ref)
}

#' @rdname calculate_crop_npp_components
#' @usage NULL
#' @export
Calc_CropNPP_components <- calculate_crop_npp_components

#' @rdname calculate_potential_npp
#' @usage NULL
#' @export
Calc_NPP_potentials <- calculate_potential_npp

#' @rdname calculate_npp_dm_c_n
#' @usage NULL
#' @export
Calc_NPP_DM_C_N <- calculate_npp_dm_c_n
