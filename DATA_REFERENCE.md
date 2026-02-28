# afsetools Data Reference

This document describes the 80 data objects loaded by
[`load_general_data()`](https://eduaguilera.github.io/afsetools/reference/load_general_data.md).
All objects are automatically loaded into your R environment when you
call this function.

## Quick Reference

``` r
library(afsetools)
load_general_data()  # Loads all 80 objects below
```

## Nomenclatures and Classifications (35 objects)

### Core Classifications

- **`items_full`** - Complete item nomenclature with codes, names, and
  categories
- **`items_prod_full`** - Production-specific item classifications  
- **`items_cbs`** - Commodity Balance Sheet item codes
- **`regions_full`** - Country codes and names with regional
  aggregations
- **`regions_full_uISO3`** - Regional classifications with ISO3 codes

### Product Categories

- **`Cats`** - Primary product categories and groupings
- **`Cats_proc`** - Processed product categories
- **`Primary_double`** - Items with multiple primary products
  (co-products)
- **`Secondary_double`** - Items with multiple secondary products

### Processing and Trade

- **`Extraction_rates`** - Processing extraction rates by product
- **`Processing_shares`** - Allocation shares for processing chains
- **`Trade_codes`** - Trade classification codes and mappings
- **`Supply_util`** - Supply utilization categories

### Additional Classifications (25+ more objects)

Including specialized nomenclatures for crops, livestock, feed, and
regional aggregations.

## Biomass Coefficients (17 objects)

### Main Coefficients

- **`Biomass_coefs`** - Primary coefficient table containing:
  - Dry matter content (DM)
  - Nitrogen content (N)
  - Carbon content (C)
  - Energy content (energy_MJ_kg)
  - Digestible energy for livestock
  - Metabolizable energy values

### Root and Residue Parameters

- **`Root_ref`** - Reference root biomass values by crop type
- **`Residue_Shares`** - Management shares for crop residues (harvested
  vs. left in field)
- **`Root_Shoot_ratios`** - Root to shoot biomass ratios
- **`Harvest_indices`** - Harvest index values for different crops

### Scaling and Conversion Factors

- **`Weed_NPP_Scaling`** - Scaling factors for weed biomass relative to
  crops
- **`Fallow_cover`** - Vegetation cover fractions on fallow land
- **`Land_scaling`** - Land use intensity scaling factors
- **`Protein_digestibility`** - Protein digestibility coefficients

### Weed Biomass Constants (6 scalars)

- **`Root_Shoot_ratio_W`** - Root to shoot ratio for weeds
- **`Residue_kgDM_kgFM_W`** - Weed residue dry matter per fresh matter
- **`Residue_kgN_kgDM_W`** - Nitrogen content of weed residues (kg N /
  kg DM)
- **`Root_kgN_kgDM_W`** - Nitrogen content of weed roots (kg N / kg DM)
- **`Residue_kgC_kgDM_W`** - Carbon content of weed residues (kg C / kg
  DM)
- **`Root_kgC_kgDM_W`** - Carbon content of weed roots (kg C / kg DM)
- **`Rhizod_kgN_kgRootN_W`** - Rhizodeposition nitrogen coefficient for
  weeds
- **`Residue_kgC_kgDM_Wo`** - Carbon content of woody residues (kg C /
  kg DM)
- **`Root_kgC_kgDM_Wo`** - Carbon content of woody roots (kg C / kg DM)

## IPCC Crop Residue and Root Coefficients (6 objects)

Based on IPCC 2006 Guidelines, Vol. 4, Ch. 11, Table 11.2.

- **`IPCC_residue_coefs`** - Linear model coefficients for above-ground
  residue biomass (31 crops): Slope_AG, Intercept_AG_MgDMha,
  RS_ratio_IPCC, N_AG_residue, N_BG_residue
- **`IPCC_root_coefs`** - Root-to-shoot ratios with context-specific
  adjustments (31 crops): RS_default, RS_low_N, RS_high_N, RS_irrigated,
  RS_rainfed, BG_ref_MgDMha
- **`IPCC_crop_mapping`** - Maps 136 FAO items (Name_biomass) to 31 IPCC
  crop categories
- **`Modern_variety_adoption`** - Historical adoption shares of modern
  varieties by 8 world regions (1900-2020), with HI correction factors
  (Evenson & Gollin 2003; Krausmann et al. 2013)
- **`N_input_RS_adj`** - Nitrogen input adjustment factors for
  root:shoot ratios (5 classes from \<20 to \>200 kg N/ha) based on
  Poorter & Nagel 2000
- **`Irrigation_adj`** - Irrigation adjustment factors for
  residue:product and root:shoot ratios (Irrigated, Rainfed, Mixed)

## NPP Model Coefficients (1 object)

- **`NPP_model_coefs`** - All numeric coefficients for potential NPP
  models (23 parameters across 6 model variants):
  - **Miami** (Lieth 1975): Temperature and precipitation logistic
    curves
  - **NCEAS tree TNPP/ANPP** (Del Grosso et al. 2008, Table 1): Coupled
    temperature-precipitation models for tree-dominated systems
  - **NCEAS non-tree TNPP/ANPP** (Del Grosso et al. 2008, Table 2):
    Precipitation-only saturating models for grasslands
  - **Rosenzweig** (1968): AET-based log-linear model

## Global Warming Potentials (8 objects)

- **`GWP`** - Complete GWP table with multiple time horizons
- **`GWP_C`** - Carbon molecular weight ratio
- **`GWP_CO2`** - CO2 global warming potential (= 1)
- **`GWP_CH4`** - CH4 100-year global warming potential
- **`GWP_CH4_fossil`** - CH4 fossil 100-year GWP (higher than biogenic)
- **`GWP_N2O`** - N2O 100-year global warming potential
- **`GWP_N2ON`** - N2O-N to N2O conversion factor times GWP

## Biological Nitrogen Fixation (3 objects)

- **`BNF`** - BNF parameters (17 rows) including:
  - `Name_BNF` - BNF parameter category name
  - `Ndfa` - Proportion of N derived from atmosphere (0-1; NA for
    rice/sugarcane)
  - `NHI` - Nitrogen harvest index (NA for fallow/weeds)
  - `BGN` - Below-ground nitrogen factor (NA for fallow/weeds)
  - `kgNha` - Non-symbiotic BNF base rate (kg N/ha/yr; only rice=33,
    sugarcane=25)
  - `Leguminous_share` - Fraction of leguminous species (0-1)
  - `Source` - Literature reference for parameter values
- **`Names_BNF`** - Crop name mapping (38 rows): maps `Name_biomass` to
  `Name_BNF` categories; also includes `CB_Item` for commodity balance
  linking
- **`Pure_legs`** - Pure legume classification (10 rows): categorises
  BNF categories as `Grain` or `Fodder_pure` legumes

## Constants and Scalars (6 objects)

- **`toe`** - Tonnes of oil equivalent conversion factor
- **`IOM`** - Inert organic matter parameter
- **`SOM_C`** - Soil organic matter carbon content
- **`Soil_depth_carbon`** - Reference soil depth for carbon accounting
- **`Protein_N`** - Protein to nitrogen conversion factor
- **`Kcal_MJ`** - Kilocalorie to megajoule conversion factor

## Color Palettes and Vectors (8+ objects)

### Impact-Specific Colors

- **`Total_color`** - Color for total/overall impact categories
- **`SOM_color`** - Soil organic matter impact colors
- **`GHG_color`** - Greenhouse gas impact colors  
- **`N_color`** - Nitrogen-related impact colors
- **`P_color`** - Phosphorus-related impact colors
- **`Land_color`** - Land use impact colors
- **`Water_color`** - Water use impact colors
- **`Energy_color`** - Energy use impact colors

### Utility Vectors

- **`Month_names`** - Month name vectors for plotting
- **`Month_numbers`** - Month number sequences

## Data Sources

All coefficients and classifications are derived from harmonized
datasets including:

- **FAO Statistics** - Production, trade, and commodity balance data
- **IPCC 2006 Guidelines** - Crop residue/root coefficients (Vol.4,
  Ch.11, Table 11.2), emission factors, and GWP values
- **Del Grosso et al. (2008)** - NCEAS NPP model coefficients (Ecology
  89:2117-2126)
- **Lieth (1975)** - Miami NPP model coefficients
- **Rosenzweig (1968)** - AET-based NPP model (Am Nat 102:67-74)
- **Evenson & Gollin (2003)** - Green Revolution variety adoption data
  (Science 300:758)
- **Poorter & Nagel (2000)** - Root:shoot ratio response to N (New
  Phytologist 147:135)
- **Scientific Literature** - Biomass coefficients and BNF parameters
- **LUH2 Dataset** - Land use harmonization data
- **Nutrient Databases** - Food composition and nutritional data
- **Scientific Literature** - Biomass coefficients and BNF parameters
- **LUH2 Dataset** - Land use harmonization data
- **Nutrient Databases** - Food composition and nutritional data

## Usage Examples

``` r
# Load all data
load_general_data()

# Explore nomenclatures
head(items_full)
head(regions_full)

# Check available coefficients
head(Biomass_coefs)
head(GWP)

# Use in calculations
my_data %>%
  left_join(Biomass_coefs, by = "item") %>%
  left_join(GWP, by = "gas_type")

# Access constants
carbon_content <- Residue_kgC_kgDM_W
colors_for_plot <- GHG_color
```

## Object Relationships

Many objects are designed to work together:

- **Items** nomenclatures link production data to coefficients
- **Biomass_coefs** provides conversion factors for **NPP functions**
- **GWP** values work with **emission calculation functions**  
- **BNF** parameters integrate with **nitrogen cycling functions**
- **Color palettes** match the **impact categories** in results

For function documentation, use R’s help system: `?function_name` or
[`help(package = "afsetools")`](https://eduaguilera.github.io/afsetools/reference).
