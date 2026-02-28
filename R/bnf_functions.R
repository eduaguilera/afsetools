# ============================================================================
# Biological Nitrogen Fixation Functions
# ============================================================================
#
# Literature-based functions for estimating symbiotic (crop legumes, weeds/
# cover crops) and non-symbiotic (free-living and associative) biological
# nitrogen fixation (BNF) in agricultural systems.
#
# Improvements over legacy Calc_N_fix():
#   - Environmental modifiers for temperature, water, N inputs, SOM, pH
#   - Corrected join logic for crop-BNF parameter mapping
#   - Separate modular functions for each BNF component
#   - Literature-grounded parameterisation with adjustable coefficients
#
# Key references:
#   Herridge et al. (2008) Plant Soil 311:1-18
#   Peoples et al. (2009) Symbiosis 48:1-17
#   Salvagiotti et al. (2008) Field Crops Research 108:1-13
#   Cleveland et al. (1999) Global Biogeochem. Cycles 13:623-645
#   Reed et al. (2011) Annu. Rev. Ecol. Evol. Syst. 42:489-512
#   Dynarski & Houlton (2018) New Phytologist 217:68-85
#   Ladha et al. (2016) Scientific Reports 6:19355
#   Anglade et al. (2015) Nutr. Cycl. Agroecosyst. 103:37-56
# ============================================================================


# ============================================================================
# Internal Helper Functions
# ============================================================================

#' Ensure optional environmental columns exist with neutral defaults
#'
#' Adds missing optional columns with defaults that produce no
#' environmental adjustment (factor = 1) when used in modifier functions.
#'
#' @param x Data frame to check.
#' @return Data frame with all optional columns guaranteed to exist.
#' @noRd
.bnf_ensure_env_cols <- function(x) {
  # --- N inputs: default 0 (no adjustment to BNF) ---
  if (!"N_synth_kgha" %in% names(x)) x[["N_synth_kgha"]] <- 0
  if (!"N_org_kgha" %in% names(x)) x[["N_org_kgha"]] <- 0

  # --- Climate: default NA (modifier returns 1) ---
  if (!"TMP" %in% names(x)) x[["TMP"]] <- NA_real_
  if (!"PET_mm" %in% names(x)) x[["PET_mm"]] <- NA_real_

  # --- Water input: compute from components if absent ---
  if (!"WaterInput_mm" %in% names(x)) {
    if ("precip_mm" %in% names(x)) {
      irrig <- if ("irrig_mm" %in% names(x)) {
        x[["irrig_mm"]]
      } else {
        0
      }
      x[["WaterInput_mm"]] <- x[["precip_mm"]] + irrig
    } else {
      x[["WaterInput_mm"]] <- NA_real_
    }
  }

  # --- Soil: default NA (modifier returns 1) ---
  if (!"SOM_pct" %in% names(x)) x[["SOM_pct"]] <- NA_real_
  if (!"soil_pH" %in% names(x)) x[["soil_pH"]] <- NA_real_
  if (!"clay_pct" %in% names(x)) x[["clay_pct"]] <- NA_real_

  x
}


#' Join BNF parameters to data via Name_biomass mapping
#'
#' Joins the Names_BNF -> BNF parameter lookup to a data frame on
#' Name_biomass. Skips if BNF columns are already present.
#'
#' @param x Data frame with a Name_biomass column.
#' @return Data frame with BNF parameters (Ndfa, NHI, BGN, kgNha,
#'   Leguminous_share) added.
#' @noRd
.bnf_join_params <- function(x) {
  # Skip if already joined
  if (all(c("Ndfa", "kgNha") %in% names(x))) return(x)

  bnf_params <- Names_BNF |>
    dplyr::left_join(
      BNF |>
        dplyr::select(
          Name_BNF, Ndfa, NHI, BGN, kgNha,
          Leguminous_share
        ),
      by = "Name_BNF"
    ) |>
    dplyr::select(
      Name_biomass, Name_BNF, Ndfa, NHI, BGN,
      kgNha, Leguminous_share
    )

  x |>
    dplyr::left_join(bnf_params, by = "Name_biomass")
}


# --- Environmental response functions ---------------------------------------
# Each returns a 0-1 factor (or normalized around 1) representing the
# fraction of potential BNF realized under given conditions.

#' Differential N fertilization inhibition of symbiotic BNF
#'
#' Negative exponential model with separate rate constants for
#' synthetic and organic N. Mineral (synthetic) N inhibits nitrogenase
#' more strongly than organic N, which mineralizes slowly and has
#' lower instantaneous root-zone concentrations.
#'
#' @param n_synth Synthetic N input (kg N/ha).
#' @param n_org Organic N input (kg N/ha).
#' @param k_synth Rate constant for synthetic N (default 0.0035).
#'   At 200 kg synthetic N/ha, factor ~0.50.
#' @param k_org Rate constant for organic N (default 0.0018).
#'   Weaker inhibition reflecting slow mineralization.
#' @return Numeric vector, range (0, 1].
#'
#' @references
#' Salvagiotti F et al. (2008) Field Crops Research 108:1-13.
#' Streeter JG, Wong PP (1988) Plant Physiol. 88:1327-1332.
#' Peoples MB et al. (2009) Symbiosis 48:1-17.
#' @noRd
.bnf_f_n_symbiotic <- function(n_synth, n_org,
                               k_synth = 0.0035,
                               k_org = 0.0018) {
  exp(-k_synth * n_synth - k_org * n_org)
}


#' Differential N fertilization inhibition of non-symbiotic BNF
#'
#' Stronger inhibition than symbiotic: free-living fixers are more
#' sensitive to ambient N availability (energy cost trade-off).
#' Separate rate constants for synthetic vs. organic N sources.
#'
#' @param n_synth Synthetic N input (kg N/ha).
#' @param n_org Organic N input (kg N/ha).
#' @param k_synth Rate constant for synthetic N (default 0.005).
#' @param k_org Rate constant for organic N (default 0.0025).
#' @return Numeric vector, range (0, 1].
#'
#' @references
#' Dynarski KA, Houlton BZ (2018) New Phytologist 217:68-85.
#' @noRd
.bnf_f_n_nonsymbiotic <- function(n_synth, n_org,
                                  k_synth = 0.005,
                                  k_org = 0.0025) {
  exp(-k_synth * n_synth - k_org * n_org)
}


#' Temperature response of nitrogenase activity
#'
#' Gaussian function centred at optimum. Nitrogenase is a thermally
#' sensitive metalloenzyme with peak activity near 25 degrees C.
#'
#' @param temp_c Temperature in degrees C.
#' @param t_opt Optimum temperature (default 25).
#' @param sigma Width parameter (default 8 for symbiotic, use 10 for
#'   non-symbiotic which has broader community-level tolerance).
#' @return Numeric vector, range (0, 1].
#'
#' @references
#' Hungria M, Vargas MAT (2000) Field Crops Research 65:151-164.
#' Houlton BZ et al. (2008) Nature 454:327-330.
#' @noRd
.bnf_f_temperature <- function(temp_c, t_opt = 25, sigma = 8) {
  exp(-((temp_c - t_opt)^2) / (2 * sigma^2))
}


#' Water availability response for BNF
#'
#' Based on aridity index (water input / PET). BNF is sensitive to
#' drought through effects on plant growth, nodule oxygen regulation,
#' and carbon supply. Full activity above the humid threshold (AI > 0.65).
#'
#' @param water_mm Total water input (precipitation + irrigation), mm.
#' @param pet_mm Potential evapotranspiration, mm.
#' @return Numeric vector, range (0, 1].
#'
#' @references
#' Serraj R et al. (1999) Plant Physiology 120:577-586.
#' Zahran HH (1999) Microbiol. Mol. Biol. Rev. 63:968-989.
#' @noRd
.bnf_f_water <- function(water_mm, pet_mm) {
  ai <- dplyr::if_else(pet_mm > 0, water_mm / pet_mm, 1)
  pmin(1, ai / 0.65)
}


#' Soil organic matter effect on non-symbiotic BNF
#'
#' Michaelis-Menten model: SOM provides carbon energy for heterotrophic
#' free-living N fixers. Normalized at SOM_ref so that typical agricultural
#' soils yield factor = 1, higher SOM enhances fixation, and low SOM
#' reduces it.
#'
#' @param som_pct Soil organic matter content (percent).
#' @param k_som Half-saturation constant (default 2.0 percent SOM).
#' @param som_ref Reference SOM for normalization (default 2.5 percent).
#' @return Numeric vector, normalized around 1 at som_ref.
#'
#' @references
#' Dynarski KA, Houlton BZ (2018) New Phytologist 217:68-85.
#' Reed SC et al. (2011) Annu. Rev. Ecol. Evol. Syst. 42:489-512.
#' @noRd
.bnf_f_som <- function(som_pct, k_som = 2.0, som_ref = 2.5) {
  raw <- som_pct / (som_pct + k_som)
  ref <- som_ref / (som_ref + k_som)
  raw / ref
}


#' Soil pH effect on BNF
#'
#' Gaussian response centred at optimal pH for nitrogenase activity and
#' nodulation. Both very acidic and alkaline conditions reduce fixation.
#'
#' @param ph Soil pH.
#' @param ph_opt Optimal pH (default 6.8).
#' @param sigma Width parameter (default 1.5).
#' @return Numeric vector, range (0, 1].
#'
#' @references
#' Belnap J (2002) Biol. Fertil. Soils 35:128-135.
#' @noRd
.bnf_f_ph <- function(ph, ph_opt = 6.8, sigma = 1.5) {
  exp(-((ph - ph_opt)^2) / (2 * sigma^2))
}


#' Soil clay content effect on non-symbiotic BNF
#'
#' Michaelis-Menten model: clay provides microsites and water
#' retention that support free-living N-fixing bacterial communities.
#' Normalized at clay_ref so that typical agricultural soils yield
#' factor = 1, higher clay enhances fixation, and sandy soils
#' reduce it.
#'
#' @param clay_pct Soil clay content (percent).
#' @param k_clay Half-saturation constant (default 20 percent clay).
#' @param clay_ref Reference clay for normalization (default 25
#'   percent, typical agricultural loam).
#' @return Numeric vector, normalized around 1 at clay_ref.
#'
#' @references
#' Roper MM, Gupta VVSR (2016) Soil Biology and Biochemistry
#'   94:143-152.
#' Limmer C, Drake HL (1996) Soil Biology and Biochemistry
#'   28:177-183.
#' @noRd
.bnf_f_clay <- function(clay_pct, k_clay = 20, clay_ref = 25) {
  raw <- clay_pct / (clay_pct + k_clay)
  ref <- clay_ref / (clay_ref + k_clay)
  raw / ref
}


#' Validate input data for BNF functions
#'
#' Checks that input is a data frame with required columns.
#' Warns on zero rows. Stops on missing required columns.
#'
#' @param x Input to validate.
#' @param required_cols Character vector of required column names.
#' @param fn_name Character. Name of calling function for messages.
#' @return TRUE invisibly if validation passes.
#' @noRd
.bnf_validate_input <- function(x, required_cols, fn_name) {
  if (!is.data.frame(x)) {
    stop(fn_name, ": input must be a data frame.", call. = FALSE)
  }
  if (nrow(x) == 0) {
    warning(fn_name, ": input has zero rows.", call. = FALSE)
    return(invisible(TRUE))
  }
  missing <- setdiff(required_cols, names(x))
  if (length(missing) > 0) {
    stop(
      fn_name, ": missing required columns: ",
      paste(missing, collapse = ", "), call. = FALSE
    )
  }
  invisible(TRUE)
}


# ============================================================================
# Exported Component Functions
# ============================================================================

#' Calculate Crop Legume Symbiotic BNF
#'
#' Estimates symbiotic biological nitrogen fixation by crop legumes using
#' two complementary methods, with optional environmental adjustments for
#' nitrogen fertilization, temperature, and water availability.
#'
#' @description
#' Two estimation methods are calculated simultaneously:
#'
#' 1. **NPP method** (`CropBNF`): Based on total crop nitrogen from NPP
#'    estimation:
#'    \deqn{CropBNF = Crop\_NPP\_MgN \times Ndfa_{adj} \times Leg\_share}
#'
#' 2. **Anglade method** (`CropBNF2`): Based on product nitrogen,
#'    below-ground N (BGN), and nitrogen harvest index (NHI) following
#'    Anglade et al. (2015) and Lassaletta et al. (2014):
#'    \deqn{CropBNF2 = Prod\_MgN \times Leg\_share \times Ndfa_{adj}
#'    \times BGN / NHI}
#'
#' When environmental columns are available, the reference Ndfa is
#' adjusted:
#' \deqn{Ndfa_{adj} = Ndfa_{ref} \times f_N \times f_T \times f_W}
#'
#' @param x Data frame with crop NPP data. Required columns:
#'   \describe{
#'     \item{Name_biomass}{Crop name matching Names_BNF classification.}
#'     \item{Crop_NPP_MgN}{Crop NPP in Mg N.}
#'     \item{Prod_MgN}{Product nitrogen in Mg.}
#'   }
#'
#'   Optional environmental columns (if absent, no adjustment applied):
#'   \describe{
#'     \item{N_synth_kgha}{Synthetic N fertilizer (kg N/ha).}
#'     \item{N_org_kgha}{Organic N inputs (kg N/ha).}
#'     \item{TMP}{Mean temperature (degrees C).}
#'     \item{WaterInput_mm}{Precipitation + irrigation (mm). Computed
#'       from precip_mm + irrig_mm if absent.}
#'     \item{precip_mm}{Precipitation (mm).}
#'     \item{irrig_mm}{Irrigation water applied (mm).}
#'     \item{PET_mm}{Potential evapotranspiration (mm).}
#'   }
#'
#' @param k_n_synth Numeric. Rate constant for N inhibition by
#'   synthetic N (default 0.0035). At 200 kg synthetic N/ha, Ndfa
#'   reduces to ~50 percent of reference.
#' @param k_n_org Numeric. Rate constant for N inhibition by organic
#'   N (default 0.0018). Weaker than synthetic because organic N
#'   mineralizes slowly (Peoples et al. 2009).
#' @param t_opt Numeric. Optimal temperature for nitrogenase (default
#'   25 degrees C).
#' @param t_sigma Numeric. Width of temperature Gaussian (default 8).
#'
#' @return Data frame with added columns:
#'   \describe{
#'     \item{Ndfa, NHI, BGN, Leguminous_share}{BNF parameters.}
#'     \item{N_total_kgha}{Total N input (synthetic + organic).}
#'     \item{f_N_symb}{N inhibition factor (0-1).}
#'     \item{f_temp_symb}{Temperature factor (0-1).}
#'     \item{f_water_symb}{Water availability factor (0-1).}
#'     \item{f_env_symb}{Combined environmental factor.}
#'     \item{Ndfa_adj}{Adjusted Ndfa after environmental correction.}
#'     \item{CropBNF}{Crop BNF via NPP method (Mg N).}
#'     \item{CropBNF2}{Crop BNF via Anglade method (Mg N).}
#'     \item{Alpha1, Alpha2}{BNF per unit product N.}
#'   }
#'
#' @details
#' **N fertilizer inhibition**: Mineral N inhibits nitrogenase and
#' nodule formation (Salvagiotti et al. 2008; Streeter & Wong 1988).
#' Synthetic N inhibits more strongly than organic N:
#' \deqn{f_N = \exp(-k_{synth} \times N_{synth} - k_{org} \times
#'   N_{org})}
#'
#' **Temperature**: Gaussian centred at T_opt, reflecting thermal
#' sensitivity of nitrogenase (Hungria & Vargas 2000):
#' \deqn{f_T = \exp\left(-\frac{(T - T_{opt})^2}{2
#'   \sigma^2}\right)}
#'
#' **Water stress**: BNF declines under drought via carbon and oxygen
#' effects on nodules (Serraj et al. 1999). Modelled via aridity index:
#' \deqn{f_W = \min(1, \text{AI} / 0.65)}
#'
#' Requires `Names_BNF` and `BNF` objects from `load_general_data()`.
#'
#' @references
#' Anglade J et al. (2015) Nutrient Cycling in Agroecosystems
#'   103:37-56.
#'
#' Hungria M, Vargas MAT (2000) Field Crops Research 65:151-164.
#'
#' Lassaletta L et al. (2014) Biogeosciences 11:2889-2907.
#'
#' Salvagiotti F et al. (2008) Field Crops Research 108:1-13.
#'
#' Serraj R et al. (1999) Plant Physiology 120:577-586.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' load_general_data()
#' # Basic usage (no environmental adjustment):
#' crop_data |> calc_crop_bnf()
#'
#' # With environmental data:
#' crop_data |>
#'   dplyr::mutate(
#'     N_synth_kgha = 80, TMP = 18,
#'     precip_mm = 600, PET_mm = 900
#'   ) |>
#'   calc_crop_bnf()
#' }
calc_crop_bnf <- function(x,
                          k_n_synth = 0.0035,
                          k_n_org = 0.0018,
                          t_opt = 25,
                          t_sigma = 8) {
  # --- Input validation ---
  .bnf_validate_input(
    x, c("Name_biomass", "Crop_NPP_MgN", "Prod_MgN"),
    "calc_crop_bnf"
  )
  if (nrow(x) == 0) return(x)

  # --- Setup ---
  x <- .bnf_ensure_env_cols(x)
  x <- .bnf_join_params(x)

  # --- Compute environmental modifiers and crop BNF ---
  x |>
    dplyr::mutate(
      N_total_kgha = N_synth_kgha + N_org_kgha,
      # N inhibition - differential by source
      # (Salvagiotti et al. 2008; Peoples et al. 2009)
      f_N_symb = .bnf_f_n_symbiotic(
        N_synth_kgha, N_org_kgha,
        k_synth = k_n_synth, k_org = k_n_org
      ),
      # Temperature (Hungria & Vargas 2000)
      f_temp_symb = dplyr::if_else(
        !is.na(TMP),
        .bnf_f_temperature(TMP, t_opt, t_sigma),
        1
      ),
      # Water availability (Serraj et al. 1999)
      f_water_symb = dplyr::if_else(
        !is.na(WaterInput_mm) & !is.na(PET_mm) &
          PET_mm > 0,
        .bnf_f_water(WaterInput_mm, PET_mm),
        1
      ),
      # Combined environmental factor
      f_env_symb = f_N_symb * f_temp_symb * f_water_symb,
      # Adjusted Ndfa (capped at reference Ndfa)
      Ndfa_adj = dplyr::if_else(
        !is.na(Ndfa),
        pmin(Ndfa * f_env_symb, Ndfa),
        NA_real_
      ),
      # --- NPP method ---
      CropBNF = dplyr::if_else(
        !is.na(Ndfa_adj),
        Crop_NPP_MgN * Ndfa_adj * Leguminous_share,
        0
      ),
      # --- Anglade method ---
      CropBNF2 = dplyr::if_else(
        !is.na(Ndfa_adj) & !is.na(NHI) & NHI > 0,
        Prod_MgN * Leguminous_share * Ndfa_adj *
          BGN / NHI,
        0
      ),
      Alpha1 = dplyr::if_else(
        Prod_MgN > 0, CropBNF / Prod_MgN, NA_real_
      ),
      Alpha2 = dplyr::if_else(
        Prod_MgN > 0, CropBNF2 / Prod_MgN, NA_real_
      )
    )
}


#' Calculate Weed and Cover Crop Symbiotic BNF
#'
#' Estimates symbiotic BNF from leguminous weeds and seeded cover crops.
#' The legume fraction in field vegetation is a weighted average of
#' spontaneous weeds and seeded cover crops, based on management data.
#'
#' @description
#' Weed BNF is calculated as:
#' \deqn{WeedsBNF = Weeds\_NPP\_MgN \times Ndfa_{weeds} \times
#'   Leg\_share_{weighted}}
#'
#' Where the weighted legume share accounts for both spontaneous
#' leguminous weeds and deliberately seeded legume cover crops:
#' \deqn{Leg\_share = Leg\_spont \times (1 - CC\_share) +
#'   Leg\_seeded \times CC\_share}
#'
#' Environmental adjustments (N inhibition, temperature, water) are
#' applied to the weed Ndfa when the corresponding columns are
#' present (same mechanism as crop BNF).
#'
#' @param x Data frame with weed NPP data. Required columns:
#'   \describe{
#'     \item{Weeds_NPP_MgN}{Weed NPP in Mg N.}
#'     \item{LandUse}{Land use type (Cropland, Grassland, etc.).}
#'     \item{Legs_Seeded}{Legume fraction in seeded cover crops
#'       (0-1).}
#'     \item{Seeded_CC_share}{Share of area with seeded cover crops
#'       (0-1).}
#'   }
#'
#'   Optional environmental columns: see \code{\link{calc_crop_bnf}}.
#'
#' @param k_n_synth Numeric. Rate constant for N inhibition by
#'   synthetic N (default 0.0035).
#' @param k_n_org Numeric. Rate constant for N inhibition by
#'   organic N (default 0.0018).
#' @param t_opt Numeric. Optimal temperature (default 25).
#' @param t_sigma Numeric. Temperature Gaussian width (default 8).
#'
#' @return Data frame with added columns:
#'   \describe{
#'     \item{Weeds_Ndfa_ref}{Reference Ndfa for weeds.}
#'     \item{Weeds_Ndfa}{Adjusted Ndfa for weeds.}
#'     \item{Weeds_leg_share}{Weighted legume fraction.}
#'     \item{f_env_weed}{Environmental adjustment factor.}
#'     \item{WeedsBNF}{Weed BNF in Mg N.}
#'   }
#'
#' @details
#' Requires BNF object from `load_general_data()` (reads
#' "Weeds" row for reference Ndfa and spontaneous legume share).
#' Cover crop seeded share is only applied on cropland.
#'
#' @references
#' Peoples MB et al. (2009) Symbiosis 48:1-17.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' load_general_data()
#' weed_data |> calc_weed_bnf()
#' }
calc_weed_bnf <- function(x,
                          k_n_synth = 0.0035,
                          k_n_org = 0.0018,
                          t_opt = 25,
                          t_sigma = 8) {
  # --- Input validation ---
  .bnf_validate_input(
    x,
    c("Weeds_NPP_MgN", "LandUse", "Legs_Seeded",
      "Seeded_CC_share"),
    "calc_weed_bnf"
  )
  if (nrow(x) == 0) return(x)

  # --- Setup ---
  x <- .bnf_ensure_env_cols(x)

  # Extract weed BNF parameters from data object
  weeds_row <- BNF |>
    dplyr::filter(Name_BNF == "Weeds")
  weeds_ndfa_ref <- as.numeric(weeds_row$Ndfa)
  legs_spont <- as.numeric(weeds_row$Leguminous_share)

  # Pre-compute N_total_kgha if not already present
  if (!"N_total_kgha" %in% names(x)) {
    x[["N_total_kgha"]] <- x[["N_synth_kgha"]] + x[["N_org_kgha"]]
  }

  x |>
    dplyr::mutate(
      # Environmental adjustment (same mechanisms as crop)
      f_env_weed =
        .bnf_f_n_symbiotic(
          N_synth_kgha, N_org_kgha,
          k_synth = k_n_synth, k_org = k_n_org
        ) *
        dplyr::if_else(
          !is.na(TMP),
          .bnf_f_temperature(TMP, t_opt, t_sigma),
          1
        ) *
        dplyr::if_else(
          !is.na(WaterInput_mm) & !is.na(PET_mm) &
            PET_mm > 0,
          .bnf_f_water(WaterInput_mm, PET_mm),
          1
        ),
      Weeds_Ndfa_ref = weeds_ndfa_ref,
      Weeds_Ndfa = pmin(
        Weeds_Ndfa_ref * f_env_weed, Weeds_Ndfa_ref
      ),
      Legs_SpontWeeds = legs_spont,
      Legs_Seeded = tidyr::replace_na(Legs_Seeded, 0),
      Seeded_CC_share = dplyr::if_else(
        LandUse == "Cropland", Seeded_CC_share, 0
      ),
      # Weighted legume share
      Weeds_leg_share =
        (Legs_SpontWeeds * (1 - Seeded_CC_share)) +
        (Legs_Seeded * Seeded_CC_share),
      WeedsBNF =
        Weeds_NPP_MgN * Weeds_Ndfa * Weeds_leg_share,
      WeedsBNF = tidyr::replace_na(WeedsBNF, 0)
    )
}


#' Calculate Non-Symbiotic BNF
#'
#' Estimates free-living and associative BNF in agricultural soils
#' using crop-specific base rates and environmental modifiers for
#' temperature, water availability, N inputs, soil organic matter,
#' and soil pH.
#'
#' @description
#' Non-symbiotic BNF (NSBNF) is performed by free-living soil bacteria
#' (e.g. Azotobacter, Clostridium), associative bacteria in the
#' rhizosphere (e.g. Azospirillum), cyanobacteria in paddy soils, and
#' endophytic bacteria (e.g. Gluconacetobacter in sugarcane).
#'
#' The model calculates:
#' \deqn{NSBNF = NSBNF_{base} \times f_N \times f_T \times f_W
#'   \times f_{SOM} \times f_{pH} \times Area / 1000}
#'
#' Where \eqn{NSBNF_{base}} comes from the BNF data table
#' (crop-specific: rice 33, sugarcane 25 kg N/ha/yr) or a default
#' rate for general cropland.
#'
#' @param x Data frame. Required columns:
#'   \describe{
#'     \item{Area_ygpit_ha}{Harvested area in hectares.}
#'   }
#'
#'   Optional columns (if absent, modifier = 1):
#'   \describe{
#'     \item{N_synth_kgha}{Synthetic N fertilizer (kg N/ha).}
#'     \item{N_org_kgha}{Organic N inputs (kg N/ha).}
#'     \item{TMP}{Mean temperature (degrees C).}
#'     \item{WaterInput_mm}{Precipitation + irrigation (mm).}
#'     \item{precip_mm}{Precipitation (mm).}
#'     \item{irrig_mm}{Irrigation (mm).}
#'     \item{PET_mm}{Potential evapotranspiration (mm).}
#'     \item{SOM_pct}{Soil organic matter content (percent).}
#'     \item{soil_pH}{Soil pH.}
#'     \item{kgNha}{Crop-specific NSBNF base rate from BNF table.
#'       If absent and Name_biomass exists, joined automatically.}
#'   }
#'
#' @param nsbnf_default_kgha Numeric. Default NSBNF base rate for
#'   crops without a specific value in the BNF table (default 5 kg
#'   N/ha/yr). Based on Cleveland et al. (1999) for temperate
#'   agricultural soils.
#' @param k_n_ns_synth Numeric. Rate constant for N inhibition of
#'   NSBNF by synthetic N (default 0.005). Stronger than symbiotic
#'   inhibition since free-living fixers avoid the energy cost of
#'   fixation when mineral N is available.
#' @param k_n_ns_org Numeric. Rate constant for N inhibition of
#'   NSBNF by organic N (default 0.0025). Weaker than synthetic
#'   because organic N mineralizes slowly.
#' @param t_opt_ns Numeric. Optimal temperature for non-symbiotic
#'   fixation (default 25).
#' @param t_sigma_ns Numeric. Temperature Gaussian width (default 10).
#'   Broader than symbiotic (8) because diverse microbial communities
#'   provide thermal buffering.
#' @param k_som Numeric. SOM half-saturation constant (default 2.0
#'   percent).
#' @param som_ref Numeric. Reference SOM for normalization (default
#'   2.5 percent).
#' @param ph_opt Numeric. Optimal pH (default 6.8).
#' @param ph_sigma Numeric. pH Gaussian width (default 1.5).
#' @param k_clay Numeric. Half-saturation constant for clay effect
#'   on NSBNF (default 20 percent clay).
#' @param clay_ref Numeric. Reference clay content for
#'   normalization (default 25 percent, typical loam).
#'
#' @return Data frame with added columns:
#'   \describe{
#'     \item{NSBNF_base_kgha}{Base rate before adjustment (kg N/ha).}
#'     \item{f_N_ns}{N inhibition factor.}
#'     \item{f_temp_ns}{Temperature factor.}
#'     \item{f_water_ns}{Water factor.}
#'     \item{f_SOM_ns}{SOM factor (normalized at som_ref).}
#'     \item{f_pH_ns}{pH factor.}
#'     \item{f_clay_ns}{Clay texture factor (normalized at
#'       clay_ref).}
#'     \item{f_env_ns}{Combined environmental factor.}
#'     \item{NSBNF}{Non-symbiotic BNF in Mg N.}
#'   }
#'
#' @details
#' **Base rates** from literature:
#' \itemize{
#'   \item Rice paddies: 33 kg N/ha/yr (Ladha et al. 2016) -
#'     cyanobacteria and heterotrophic anaerobic fixation
#'   \item Sugarcane: 25 kg N/ha/yr (Urquiaga et al. 2012) -
#'     endophytic fixation (Gluconacetobacter diazotrophicus)
#'   \item General cropland: 5 kg N/ha/yr default
#'     (Cleveland et al. 1999)
#' }
#'
#' **N inhibition** (Dynarski & Houlton 2018): At 100 kg N/ha,
#' NSBNF reduces to ~61 percent of base. Free-living fixers avoid
#' the energy cost of fixation when mineral N is available.
#'
#' **SOM effect** (Reed et al. 2011; Dynarski & Houlton 2018):
#' Heterotrophic fixers require carbon as energy source. Michaelis-
#' Menten kinetics normalized at typical agricultural SOM (2.5
#' percent). High-SOM soils show enhanced NSBNF.
#'
#' **pH effect** (Belnap 2002): Nitrogenase activity and microbial
#' communities are sensitive to soil pH. Gaussian around optimal
#' 6.8 with moderate decline in very acidic (pH < 5) or alkaline
#' (pH > 8) soils.
#'
#' @references
#' Cleveland CC et al. (1999) Global Biogeochemical Cycles
#'   13:623-645.
#'
#' Dynarski KA, Houlton BZ (2018) New Phytologist 217:68-85.
#'
#' Ladha JK et al. (2016) Scientific Reports 6:19355.
#'
#' Reed SC et al. (2011) Annual Review of Ecology, Evolution,
#'   and Systematics 42:489-512.
#'
#' Urquiaga S et al. (2012) Plant and Soil 356:5-21.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' load_general_data()
#'
#' # Basic: only area required, uses defaults
#' df |> calc_nonsymbiotic_bnf()
#'
#' # With environmental data:
#' df |>
#'   dplyr::mutate(
#'     N_synth_kgha = 120, TMP = 22,
#'     precip_mm = 700, PET_mm = 850,
#'     SOM_pct = 3.2, soil_pH = 6.4
#'   ) |>
#'   calc_nonsymbiotic_bnf()
#' }
calc_nonsymbiotic_bnf <- function(x,
                                  nsbnf_default_kgha = 5,
                                  k_n_ns_synth = 0.005,
                                  k_n_ns_org = 0.0025,
                                  t_opt_ns = 25,
                                  t_sigma_ns = 10,
                                  k_som = 2.0,
                                  som_ref = 2.5,
                                  ph_opt = 6.8,
                                  ph_sigma = 1.5,
                                  k_clay = 20,
                                  clay_ref = 25) {
  # --- Input validation ---
  .bnf_validate_input(
    x, "Area_ygpit_ha", "calc_nonsymbiotic_bnf"
  )
  if (nrow(x) == 0) return(x)

  # --- Setup ---
  x <- .bnf_ensure_env_cols(x)

  # Ensure kgNha column exists (join if needed)
  if (!"kgNha" %in% names(x)) {
    if ("Name_biomass" %in% names(x)) {
      x <- .bnf_join_params(x)
    } else {
      x[["kgNha"]] <- NA_real_
    }
  }

  # Pre-compute N_total_kgha if not already present
  if (!"N_total_kgha" %in% names(x)) {
    x[["N_total_kgha"]] <- x[["N_synth_kgha"]] + x[["N_org_kgha"]]
  }

  x |>
    dplyr::mutate(
      # Base rate (crop-specific or default)
      NSBNF_base_kgha = dplyr::if_else(
        !is.na(kgNha), kgNha, nsbnf_default_kgha
      ),
      # --- Environmental modifiers ---
      # N inhibition - differential by source
      # (Dynarski & Houlton 2018)
      f_N_ns = .bnf_f_n_nonsymbiotic(
        N_synth_kgha, N_org_kgha,
        k_synth = k_n_ns_synth, k_org = k_n_ns_org
      ),
      # Temperature (broader tolerance than symbiotic)
      f_temp_ns = dplyr::if_else(
        !is.na(TMP),
        .bnf_f_temperature(TMP, t_opt_ns, t_sigma_ns),
        1
      ),
      # Water availability
      f_water_ns = dplyr::if_else(
        !is.na(WaterInput_mm) & !is.na(PET_mm) &
          PET_mm > 0,
        .bnf_f_water(WaterInput_mm, PET_mm),
        1
      ),
      # SOM - C energy for heterotrophic fixers
      f_SOM_ns = dplyr::if_else(
        !is.na(SOM_pct),
        .bnf_f_som(SOM_pct, k_som, som_ref),
        1
      ),
      # Soil pH
      f_pH_ns = dplyr::if_else(
        !is.na(soil_pH),
        .bnf_f_ph(soil_pH, ph_opt, ph_sigma),
        1
      ),
      # Clay texture (Roper & Gupta 2016)
      f_clay_ns = dplyr::if_else(
        !is.na(clay_pct),
        .bnf_f_clay(clay_pct, k_clay, clay_ref),
        1
      ),
      # Combined factor
      f_env_ns = f_N_ns * f_temp_ns * f_water_ns *
        f_SOM_ns * f_pH_ns * f_clay_ns,
      # Final NSBNF (Mg N)
      NSBNF = NSBNF_base_kgha * f_env_ns *
        Area_ygpit_ha / 1000
    )
}


# ============================================================================
# Master Function
# ============================================================================

#' Calculate Total Biological Nitrogen Fixation
#'
#' Master function that calculates all BNF components (symbiotic crop,
#' symbiotic weed/cover crop, and non-symbiotic) using literature-based
#' environmental modifiers. Replaces and improves upon `Calc_N_fix()`.
#'
#' @description
#' Total BNF is the sum of three components:
#' \deqn{BNF = CropBNF + WeedsBNF + NSBNF}
#'
#' Each component is estimated using reference parameters from the BNF
#' data table, optionally adjusted for local environmental conditions
#' (N inputs, temperature, moisture, SOM, pH) when these variables are
#' present in the input data.
#'
#' **When no environmental columns are present**, the function produces
#' results comparable to `Calc_N_fix()` (all adjustment factors = 1),
#' ensuring backward compatibility.
#'
#' @param x Data frame with crop NPP and area data. Required columns:
#'   \describe{
#'     \item{Name_biomass}{Crop name matching Names_BNF.}
#'     \item{Crop_NPP_MgN}{Crop NPP in Mg N.}
#'     \item{Prod_MgN}{Product nitrogen in Mg.}
#'     \item{Weeds_NPP_MgN}{Weed NPP in Mg N.}
#'     \item{LandUse}{Land use type (Cropland, etc.).}
#'     \item{Area_ygpit_ha}{Harvested area in hectares.}
#'     \item{Legs_Seeded}{Legume fraction in seeded cover crops.}
#'     \item{Seeded_CC_share}{Share of area with seeded CCs.}
#'   }
#'
#'   Optional environmental columns (auto-detected):
#'   \describe{
#'     \item{N_synth_kgha}{Synthetic N fertilizer (kg N/ha).}
#'     \item{N_org_kgha}{Organic N inputs (kg N/ha).}
#'     \item{TMP}{Mean temperature (degrees C).}
#'     \item{WaterInput_mm}{Water input (mm). Or provide precip_mm
#'       and optionally irrig_mm.}
#'     \item{PET_mm}{Potential evapotranspiration (mm).}
#'     \item{SOM_pct}{Soil organic matter (percent).}
#'     \item{soil_pH}{Soil pH.}
#'   }
#'
#' @param k_n_symb_synth Numeric. Synthetic N inhibition constant
#'   for symbiotic BNF (default 0.0035).
#' @param k_n_symb_org Numeric. Organic N inhibition constant
#'   for symbiotic BNF (default 0.0018).
#' @param k_n_ns_synth Numeric. Synthetic N inhibition constant
#'   for non-symbiotic BNF (default 0.005).
#' @param k_n_ns_org Numeric. Organic N inhibition constant
#'   for non-symbiotic BNF (default 0.0025).
#' @param t_opt Numeric. Optimal temperature (default 25).
#' @param t_sigma_symb Numeric. Temperature width for symbiotic
#'   (default 8).
#' @param t_sigma_ns Numeric. Temperature width for non-symbiotic
#'   (default 10).
#' @param nsbnf_default_kgha Numeric. Default NSBNF base rate
#'   (default 5 kg N/ha/yr).
#' @param k_som Numeric. SOM half-saturation (default 2.0).
#' @param som_ref Numeric. Reference SOM (default 2.5).
#' @param ph_opt Numeric. Optimal pH (default 6.8).
#' @param ph_sigma Numeric. pH Gaussian width (default 1.5).
#' @param k_clay Numeric. Clay half-saturation for NSBNF (default
#'   20).
#' @param clay_ref Numeric. Reference clay percent (default 25).
#'
#' @return Data frame with all intermediate columns plus:
#'   \describe{
#'     \item{Fert_type}{"BNF" character flag.}
#'     \item{CropBNF}{Symbiotic crop BNF, NPP method (Mg N).}
#'     \item{CropBNF2}{Symbiotic crop BNF, Anglade method (Mg N).}
#'     \item{WeedsBNF}{Symbiotic weed/cover crop BNF (Mg N).}
#'     \item{NSBNF}{Non-symbiotic BNF (Mg N).}
#'     \item{BNF}{Total BNF (Mg N).}
#'     \item{f_N_symb, f_temp_symb, f_water_symb}{Symbiotic
#'       adjustment factors.}
#'     \item{f_N_ns, f_temp_ns, f_water_ns, f_SOM_ns,
#'       f_pH_ns, f_clay_ns}{Non-symbiotic adjustment factors.}
#'   }
#'
#' @details
#' ## Symbiotic BNF (crop legumes)
#'
#' Two methods are calculated in parallel:
#' - **NPP method**: Uses total crop NPP nitrogen and Ndfa.
#' - **Anglade method**: Uses product nitrogen with below-ground N
#'   (BGN) and N harvest index (NHI) corrections.
#'
#' Ndfa is adjusted for N fertilization, temperature, and water.
#'
#' ## Symbiotic BNF (weeds and cover crops)
#'
#' Based on weed NPP nitrogen, with legume share as a weighted
#' average of spontaneous weeds and seeded cover crops. Same
#' environmental adjustments as crop legume BNF.
#'
#' ## Non-symbiotic BNF
#'
#' Base rate from BNF table (rice 33, sugarcane 25 kg N/ha/yr) or
#' default (5 kg N/ha/yr). Adjusted for:
#' - N inputs (free-living fixers avoid fixation cost when N
#'   available)
#' - Temperature (broad Gaussian, sigma = 10)
#' - Water (aridity index threshold)
#' - SOM (C energy for heterotrophic fixers, Michaelis-Menten)
#' - pH (Gaussian around 6.8)
#'
#' @seealso
#' \code{\link{calc_crop_bnf}} for standalone crop legume BNF.
#' \code{\link{calc_weed_bnf}} for standalone weed/cover crop BNF.
#' \code{\link{calc_nonsymbiotic_bnf}} for standalone NSBNF.
#'
#' @references
#' Anglade J et al. (2015) Nutr. Cycl. Agroecosyst. 103:37-56.
#'
#' Cleveland CC et al. (1999) Global Biogeochem. Cycles 13:623-645.
#'
#' Dynarski KA, Houlton BZ (2018) New Phytologist 217:68-85.
#'
#' Herridge DF et al. (2008) Plant and Soil 311:1-18.
#'
#' Hungria M, Vargas MAT (2000) Field Crops Res. 65:151-164.
#'
#' Ladha JK et al. (2016) Scientific Reports 6:19355.
#'
#' Peoples MB et al. (2009) Symbiosis 48:1-17.
#'
#' Reed SC et al. (2011) Annu. Rev. Ecol. Evol. Syst. 42:489-512.
#'
#' Salvagiotti F et al. (2008) Field Crops Res. 108:1-13.
#'
#' Serraj R et al. (1999) Plant Physiology 120:577-586.
#'
#' Urquiaga S et al. (2012) Plant and Soil 356:5-21.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' load_general_data()
#'
#' # Backward-compatible usage (no environmental data):
#' bnf_results <- npp_data |> calc_bnf()
#'
#' # With full environmental characterisation:
#' bnf_results <- npp_data |>
#'   dplyr::mutate(
#'     N_synth_kgha = 80, N_org_kgha = 20,
#'     TMP = 18, precip_mm = 650, irrig_mm = 100,
#'     PET_mm = 900, SOM_pct = 3.0, soil_pH = 6.5
#'   ) |>
#'   calc_bnf()
#' }
calc_bnf <- function(x,
                     k_n_symb_synth = 0.0035,
                     k_n_symb_org = 0.0018,
                     k_n_ns_synth = 0.005,
                     k_n_ns_org = 0.0025,
                     t_opt = 25,
                     t_sigma_symb = 8,
                     t_sigma_ns = 10,
                     nsbnf_default_kgha = 5,
                     k_som = 2.0,
                     som_ref = 2.5,
                     ph_opt = 6.8,
                     ph_sigma = 1.5,
                     k_clay = 20,
                     clay_ref = 25) {

  # === 1. Setup: ensure env columns and join BNF params ===
  x <- .bnf_ensure_env_cols(x)
  x <- .bnf_join_params(x)

  # Extract weed parameters from BNF table
  weeds_row <- BNF |>
    dplyr::filter(Name_BNF == "Weeds")
  weeds_ndfa_ref <- as.numeric(weeds_row$Ndfa)
  legs_spont <- as.numeric(weeds_row$Leguminous_share)

  # === 2. Compute everything in a single mutate chain ===
  x |>
    dplyr::mutate(
      Fert_type = "BNF",
      N_total_kgha = N_synth_kgha + N_org_kgha,

      # -------------------------------------------------------
      # SYMBIOTIC: Environmental adjustment factors
      # -------------------------------------------------------
      # N inhibition - differential by source
      # (Salvagiotti et al. 2008; Peoples et al. 2009)
      f_N_symb = .bnf_f_n_symbiotic(
        N_synth_kgha, N_org_kgha,
        k_synth = k_n_symb_synth, k_org = k_n_symb_org
      ),
      # Temperature (Hungria & Vargas 2000)
      f_temp_symb = dplyr::if_else(
        !is.na(TMP),
        .bnf_f_temperature(TMP, t_opt, t_sigma_symb),
        1
      ),
      # Water (Serraj et al. 1999)
      f_water_symb = dplyr::if_else(
        !is.na(WaterInput_mm) & !is.na(PET_mm) &
          PET_mm > 0,
        .bnf_f_water(WaterInput_mm, PET_mm),
        1
      ),
      f_env_symb = f_N_symb * f_temp_symb * f_water_symb,

      # Adjusted Ndfa (capped at reference)
      Ndfa_adj = dplyr::if_else(
        !is.na(Ndfa),
        pmin(Ndfa * f_env_symb, Ndfa),
        NA_real_
      ),

      # -------------------------------------------------------
      # CROP LEGUME BNF
      # -------------------------------------------------------
      # NPP method
      CropBNF = dplyr::if_else(
        !is.na(Ndfa_adj),
        Crop_NPP_MgN * Ndfa_adj * Leguminous_share,
        0
      ),
      # Anglade method
      CropBNF2 = dplyr::if_else(
        !is.na(Ndfa_adj) & !is.na(NHI) & NHI > 0,
        Prod_MgN * Leguminous_share * Ndfa_adj *
          BGN / NHI,
        0
      ),
      Alpha1 = dplyr::if_else(
        Prod_MgN > 0, CropBNF / Prod_MgN, NA_real_
      ),
      Alpha2 = dplyr::if_else(
        Prod_MgN > 0, CropBNF2 / Prod_MgN, NA_real_
      ),

      # -------------------------------------------------------
      # WEED / COVER CROP BNF
      # -------------------------------------------------------
      f_env_weed = f_env_symb,
      Weeds_Ndfa_ref = weeds_ndfa_ref,
      Weeds_Ndfa = pmin(
        Weeds_Ndfa_ref * f_env_weed, Weeds_Ndfa_ref
      ),
      Legs_SpontWeeds = legs_spont,
      Legs_Seeded = tidyr::replace_na(Legs_Seeded, 0),
      Seeded_CC_share = dplyr::if_else(
        LandUse == "Cropland", Seeded_CC_share, 0
      ),
      Weeds_leg_share =
        (Legs_SpontWeeds * (1 - Seeded_CC_share)) +
        (Legs_Seeded * Seeded_CC_share),
      WeedsBNF =
        Weeds_NPP_MgN * Weeds_Ndfa * Weeds_leg_share,
      WeedsBNF = tidyr::replace_na(WeedsBNF, 0),

      # -------------------------------------------------------
      # NON-SYMBIOTIC BNF
      # -------------------------------------------------------
      NSBNF_base_kgha = dplyr::if_else(
        !is.na(kgNha), kgNha, nsbnf_default_kgha
      ),
      # N inhibition - differential by source
      # (Dynarski & Houlton 2018)
      f_N_ns = .bnf_f_n_nonsymbiotic(
        N_synth_kgha, N_org_kgha,
        k_synth = k_n_ns_synth, k_org = k_n_ns_org
      ),
      # Temperature (broader tolerance)
      f_temp_ns = dplyr::if_else(
        !is.na(TMP),
        .bnf_f_temperature(TMP, t_opt, t_sigma_ns),
        1
      ),
      # Water
      f_water_ns = dplyr::if_else(
        !is.na(WaterInput_mm) & !is.na(PET_mm) &
          PET_mm > 0,
        .bnf_f_water(WaterInput_mm, PET_mm),
        1
      ),
      # SOM (C energy for heterotrophic fixers)
      f_SOM_ns = dplyr::if_else(
        !is.na(SOM_pct),
        .bnf_f_som(SOM_pct, k_som, som_ref),
        1
      ),
      # Soil pH
      f_pH_ns = dplyr::if_else(
        !is.na(soil_pH),
        .bnf_f_ph(soil_pH, ph_opt, ph_sigma),
        1
      ),
      # Clay texture (Roper & Gupta 2016)
      f_clay_ns = dplyr::if_else(
        !is.na(clay_pct),
        .bnf_f_clay(clay_pct, k_clay, clay_ref),
        1
      ),
      f_env_ns = f_N_ns * f_temp_ns * f_water_ns *
        f_SOM_ns * f_pH_ns * f_clay_ns,
      NSBNF = NSBNF_base_kgha * f_env_ns *
        Area_ygpit_ha / 1000,

      # -------------------------------------------------------
      # TOTAL BNF
      # -------------------------------------------------------
      BNF = CropBNF + WeedsBNF + NSBNF
    )
}


# ============================================================================
# Diagnostics Function
# ============================================================================

#' Summarize BNF Results by Crop or Region
#'
#' Produces a concise summary of BNF components and environmental
#' adjustment factors from the output of \code{\link{calc_bnf}} or
#' its component functions. Useful for diagnostics, parameter
#' sensitivity analysis, and reporting.
#'
#' @param x Data frame output from \code{calc_bnf()} or similar,
#'   containing BNF result columns.
#' @param group_by Character vector of columns to group by.
#'   Default \code{c("Name_biomass")}. Set to \code{NULL} for
#'   overall summary.
#'
#' @return A tibble with per-group summaries:
#'   \describe{
#'     \item{n}{Number of observations.}
#'     \item{total_CropBNF_MgN}{Sum of symbiotic crop BNF.}
#'     \item{total_WeedsBNF_MgN}{Sum of weed/CC BNF.}
#'     \item{total_NSBNF_MgN}{Sum of non-symbiotic BNF.}
#'     \item{total_BNF_MgN}{Sum of total BNF.}
#'     \item{mean_Ndfa_adj}{Mean adjusted Ndfa.}
#'     \item{mean_f_env_symb}{Mean symbiotic env factor.}
#'     \item{mean_f_env_ns}{Mean non-symbiotic env factor.}
#'     \item{pct_CropBNF}{Percent of total from crop.}
#'     \item{pct_WeedsBNF}{Percent of total from weeds.}
#'     \item{pct_NSBNF}{Percent of total from NSBNF.}
#'   }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' load_general_data()
#' bnf_results <- npp_data |> calc_bnf()
#' summarize_bnf(bnf_results)
#' summarize_bnf(bnf_results, group_by = c("Region", "Year"))
#' }
summarize_bnf <- function(x, group_by = "Name_biomass") {
  # Check required columns
  bnf_cols <- c("CropBNF", "WeedsBNF", "NSBNF", "BNF")
  missing <- setdiff(bnf_cols, names(x))
  if (length(missing) > 0) {
    stop(
      "summarize_bnf: missing BNF result columns: ",
      paste(missing, collapse = ", "),
      ". Run calc_bnf() first.",
      call. = FALSE
    )
  }

  # Ensure optional columns exist for summarising
  has_ndfa <- "Ndfa_adj" %in% names(x)
  has_f_symb <- "f_env_symb" %in% names(x)
  has_f_ns <- "f_env_ns" %in% names(x)

  if (!has_ndfa) x[["Ndfa_adj"]] <- NA_real_
  if (!has_f_symb) x[["f_env_symb"]] <- NA_real_
  if (!has_f_ns) x[["f_env_ns"]] <- NA_real_

  # Build grouping
  if (!is.null(group_by)) {
    present_groups <- intersect(group_by, names(x))
    if (length(present_groups) > 0) {
      x <- dplyr::group_by(x, dplyr::across(
        dplyr::all_of(present_groups)
      ))
    }
  }

  x |>
    dplyr::summarise(
      n = dplyr::n(),
      total_CropBNF_MgN = sum(CropBNF, na.rm = TRUE),
      total_WeedsBNF_MgN = sum(WeedsBNF, na.rm = TRUE),
      total_NSBNF_MgN = sum(NSBNF, na.rm = TRUE),
      total_BNF_MgN = sum(BNF, na.rm = TRUE),
      mean_Ndfa_adj = mean(Ndfa_adj, na.rm = TRUE),
      mean_f_env_symb = mean(f_env_symb, na.rm = TRUE),
      mean_f_env_ns = mean(f_env_ns, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      pct_CropBNF = dplyr::if_else(
        total_BNF_MgN > 0,
        100 * total_CropBNF_MgN / total_BNF_MgN,
        NA_real_
      ),
      pct_WeedsBNF = dplyr::if_else(
        total_BNF_MgN > 0,
        100 * total_WeedsBNF_MgN / total_BNF_MgN,
        NA_real_
      ),
      pct_NSBNF = dplyr::if_else(
        total_BNF_MgN > 0,
        100 * total_NSBNF_MgN / total_BNF_MgN,
        NA_real_
      )
    )
}