#' Data Objects Loaded by load_general_data()
#'
#' This package contains 73+ data objects that are loaded into the environment
#' when you call \code{load_general_data()}. These include nomenclatures, 
#' biomass coefficients, global warming potentials, and other parameters
#' needed for environmental footprint calculations.
#'
#' @details
#' The data objects include:
#' \itemize{
#'   \item 35 nomenclature and classification objects
#'   \item 17 biomass coefficient objects  
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