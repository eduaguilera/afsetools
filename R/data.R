#' Data Objects Loaded by load_general_data()
#'
#' This package contains 80 data objects that are loaded into the environment
#' when you call \code{load_general_data()}. These include nomenclatures, 
#' biomass coefficients, global warming potentials, IPCC crop residue and root
#' coefficients, NPP model coefficients, and other parameters needed for
#' environmental footprint calculations.
#'
#' @details
#' The data objects include:
#' \itemize{
#'   \item 35 nomenclature and classification objects
#'   \item 17 biomass coefficient objects
#'   \item 6 IPCC residue/root coefficient objects
#'   \item 1 NPP model coefficient object
#'   \item 7 global warming potential objects
#'   \item 3 biological nitrogen fixation objects
#'   \item 6 constants and scalars
#'   \item Color palettes and utility vectors
#' }
#'
#' For complete documentation of all data objects, see the DATA_REFERENCE.md file
#' in the package repository, or use \code{?object_name} for specific objects
#' after loading them with \code{load_general_data()}.
#'
#' @name afsetools-data
#' @docType data
#' @keywords datasets
NULL

#' Biomass Coefficients
#'
#' Main coefficient table containing dry matter, nitrogen, carbon, and energy content
#' for different biomass types. Used for converting between different biomass units.
#'
#' @format A data frame with coefficients for biomass conversion including:
#' \describe{
#'   \item{item}{Item code or name}
#'   \item{DM_content}{Dry matter content (fraction)}
#'   \item{N_content}{Nitrogen content (kg N / kg DM)}
#'   \item{C_content}{Carbon content (kg C / kg DM)}
#'   \item{energy_MJ_kg}{Energy content (MJ / kg)}
#' }
#' @source Harmonized from multiple scientific databases and literature
#' @name Biomass_coefs
#' @docType data
#' @keywords datasets
NULL

#' Global Warming Potentials
#'
#' GWP values for different greenhouse gases and time horizons based on IPCC guidelines.
#'
#' @format A data frame with GWP values:
#' \describe{
#'   \item{Gas}{Greenhouse gas name}
#'   \item{GWP_20}{20-year global warming potential}
#'   \item{GWP_100}{100-year global warming potential (most commonly used)}
#'   \item{GWP_500}{500-year global warming potential}
#' }
#' @source IPCC Assessment Reports
#' @name GWP
#' @docType data  
#' @keywords datasets
NULL

#' Biological Nitrogen Fixation Parameters
#'
#' Parameters for calculating biological nitrogen fixation by crop type,
#' including symbiotic and non-symbiotic fixation.
#'
#' @format A data frame with BNF coefficients:
#' \describe{
#'   \item{Name_BNF}{Crop or system name}
#'   \item{Ndfa}{Proportion of nitrogen derived from atmosphere (0-1)}
#'   \item{Leguminous_share}{Fraction of leguminous species (0-1)}
#'   \item{BGN}{Below-ground nitrogen parameter}
#'   \item{NHI}{Nitrogen harvest index}
#' }
#' @source Scientific literature compilation
#' @name BNF
#' @docType data
#' @keywords datasets  
NULL

#' Item Classifications
#'
#' Harmonized nomenclature for food and agricultural items with codes, names,
#' and category classifications.
#'
#' @format A data frame with item information:
#' \describe{
#'   \item{item_code}{Standardized item code}
#'   \item{item_name}{Item name}
#'   \item{category}{Product category}
#'   \item{group}{Product group}
#' }
#' @source FAO and other international databases
#' @name items_full
#' @docType data
#' @keywords datasets
NULL

#' Regional Classifications  
#'
#' Country codes and regional aggregations for geographic analysis.
#'
#' @format A data frame with geographic information:
#' \describe{
#'   \item{area_code}{Country/region code}
#'   \item{area_name}{Country/region name}
#'   \item{region}{Regional grouping}
#'   \item{income_group}{World Bank income classification}
#' }
#' @source UN, FAO, World Bank classifications
#' @name regions_full
#' @docType data
#' @keywords datasets
NULL

#' IPCC Crop Residue Coefficients
#'
#' Linear model coefficients for estimating above-ground crop residue biomass
#' from yield, based on IPCC 2006 Guidelines (Vol.4, Ch.11, Table 11.2).
#'
#' @format A data frame with columns:
#' \describe{
#'   \item{IPCC_crop}{IPCC crop category name.}
#'   \item{Slope_AG}{Slope of the linear residue model (Mg DM residue per
#'     Mg DM yield per hectare).}
#'   \item{Intercept_AG_MgDMha}{Intercept (Mg DM / ha), representing
#'     structural baseline residue production.}
#'   \item{RS_ratio_IPCC}{Default root-to-shoot ratio from IPCC.}
#'   \item{N_AG_residue}{Nitrogen content of above-ground residue
#'     (kg N / kg DM).}
#'   \item{N_BG_residue}{Nitrogen content of below-ground residue
#'     (kg N / kg DM).}
#'   \item{Source}{Literature source reference.}
#' }
#' @source IPCC (2006) Guidelines for National Greenhouse Gas Inventories,
#'   Vol.4, Ch.11, Table 11.2. Bolinder et al. (2007). Gan et al. (2009).
#' @name IPCC_residue_coefs
#' @docType data
#' @keywords datasets
NULL

#' IPCC Root Biomass Coefficients
#'
#' Root-to-shoot ratios and reference root biomass values for estimating
#' below-ground crop biomass, with adjustments for N input and irrigation.
#'
#' @format A data frame with columns:
#' \describe{
#'   \item{IPCC_crop}{IPCC crop category name.}
#'   \item{RS_default}{Default root-to-shoot ratio.}
#'   \item{RS_low_N}{RS ratio under low N input (<60 kg N/ha).}
#'   \item{RS_high_N}{RS ratio under high N input (>120 kg N/ha).}
#'   \item{RS_irrigated}{RS ratio under irrigation.}
#'   \item{RS_rainfed}{RS ratio under rainfed conditions.}
#'   \item{BG_ref_MgDMha}{Reference below-ground biomass (Mg DM / ha).}
#'   \item{Source}{Literature source reference.}
#' }
#' @source IPCC (2006) Guidelines for National Greenhouse Gas Inventories,
#'   Vol.4, Ch.11. Bolinder et al. (2007). Poorter & Nagel (2000).
#'   Benjamin et al. (2014).
#' @name IPCC_root_coefs
#' @docType data
#' @keywords datasets
NULL

#' IPCC Crop Name Mapping
#'
#' Maps Name_biomass crop classifications from Biomass_coefs to IPCC crop
#' categories used in IPCC_residue_coefs and IPCC_root_coefs.
#'
#' @format A data frame with columns:
#' \describe{
#'   \item{Name_biomass}{Crop name matching Biomass_coefs classification.}
#'   \item{IPCC_crop}{Corresponding IPCC crop category.}
#' }
#' @source Package-internal mapping based on IPCC crop categories.
#' @name IPCC_crop_mapping
#' @docType data
#' @keywords datasets
NULL

#' Modern Variety Adoption Timeline
#'
#' Regional time-series of modern (high-yielding) variety adoption shares
#' and their effect on harvest index. Used to correct residue:product ratios
#' for historical periods when traditional varieties dominated.
#'
#' @format A data frame with columns:
#' \describe{
#'   \item{region_HANPP}{World region name (8 regions).}
#'   \item{Year}{Year (decadal from 1900 to 2020).}
#'   \item{Modern_share}{Share of area under modern varieties (0-1).}
#'   \item{HI_correction_factor}{Multiplicative correction for
#'     residue:product ratio (>1 = more residue in traditional systems).}
#' }
#' @source Evenson & Gollin (2003) Science 300:758-762.
#'   Krausmann et al. (2013) Global Env Change 23:1170-1181.
#'   Pingali (2012) PNAS 109:12302-12308.
#' @name Modern_variety_adoption
#' @docType data
#' @keywords datasets
NULL

#' N Input Root:Shoot Adjustment Factors
#'
#' Adjustment factors for root-to-shoot ratios based on nitrogen input
#' levels. Higher N availability reduces root allocation (functional
#' equilibrium theory).
#'
#' @format A data frame with columns:
#' \describe{
#'   \item{N_input_class}{N application category name.}
#'   \item{N_input_min}{Lower bound of N rate (kg N/ha/yr).}
#'   \item{N_input_max}{Upper bound of N rate (kg N/ha/yr).}
#'   \item{RS_adjustment}{Multiplicative factor on default RS ratio.}
#'   \item{Source}{Literature source reference.}
#' }
#' @source Poorter & Nagel (2000) New Phytologist 147:135-147.
#' @name N_input_RS_adj
#' @docType data
#' @keywords datasets
NULL

#' Irrigation Adjustment Factors
#'
#' Adjustment factors for residue:product ratio and root:shoot ratio
#' under different water management regimes.
#'
#' @format A data frame with columns:
#' \describe{
#'   \item{Water_regime}{Water management (Irrigated, Rainfed, Mixed).}
#'   \item{Residue_ratio_factor}{Multiplicative factor for residue:product
#'     ratio (irrigated crops have higher HI, so less residue).}
#'   \item{RS_ratio_factor}{Multiplicative factor for root:shoot ratio
#'     (irrigated crops have shallower roots).}
#'   \item{Source}{Literature source reference.}
#' }
#' @source Sadras (2007) Field Crops Research 100:125-138.
#'   Zhang et al. (2019). Benjamin et al. (2014).
#' @name Irrigation_adj
#' @docType data
#' @keywords datasets
NULL

#' NPP Model Coefficients
#'
#' Numeric coefficients for potential NPP models used by
#' \code{calculate_potential_npp()}. Contains all parameters for the Miami,
#' NCEAS (tree and non-tree), and Rosenzweig models. Coefficients are
#' verified against published sources.
#'
#' @format A data frame with columns:
#' \describe{
#'   \item{Model}{Model name (Miami, NCEAS_tree_TNPP, NCEAS_tree_ANPP,
#'     NCEAS_nontree_TNPP, NCEAS_nontree_ANPP, Rosenzweig).}
#'   \item{Component}{Formula component (F_MAT, F_MAP, Saturating,
#'     Log_linear).}
#'   \item{Parameter}{Parameter name (Max_gCm2yr, Midpoint, Rate,
#'     Coefficient, Exponent, Exp_divisor, Slope, Intercept).}
#'   \item{Value}{Numeric parameter value.}
#'   \item{Unit}{Physical unit of the parameter.}
#'   \item{Formula_role}{Role of the parameter in the model formula.}
#'   \item{Climate_var}{Climate variable the parameter applies to
#'     (Temperature, Precipitation, AET).}
#'   \item{Source}{Literature source reference.}
#' }
#' @source Lieth (1975) Modeling Primary Productivity of the World.
#'   Del Grosso et al. (2008) Ecology 89:2117-2126.
#'   Rosenzweig (1968) Am Nat 102:67-74.
#' @name NPP_model_coefs
#' @docType data
#' @keywords datasets
NULL