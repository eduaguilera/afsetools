# afsetools Data Reference

This document describes the 73+ data objects loaded by `load_general_data()`. All objects are automatically loaded into your R environment when you call this function.

## Quick Reference

```r
library(afsetools)
load_general_data()  # Loads all 73+ objects below
```

## Nomenclatures and Classifications (35 objects)

### Core Classifications
- **`items_full`** - Complete item nomenclature with codes, names, and categories
- **`items_prod_full`** - Production-specific item classifications  
- **`items_cbs`** - Commodity Balance Sheet item codes
- **`regions_full`** - Country codes and names with regional aggregations
- **`regions_full_uISO3`** - Regional classifications with ISO3 codes

### Product Categories
- **`Cats`** - Primary product categories and groupings
- **`Cats_proc`** - Processed product categories
- **`Primary_double`** - Items with multiple primary products (co-products)
- **`Secondary_double`** - Items with multiple secondary products

### Processing and Trade
- **`Extraction_rates`** - Processing extraction rates by product
- **`Processing_shares`** - Allocation shares for processing chains
- **`Trade_codes`** - Trade classification codes and mappings
- **`Supply_util`** - Supply utilization categories

### Additional Classifications (25+ more objects)
Including specialized nomenclatures for crops, livestock, feed, and regional aggregations.

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
- **`Residue_Shares`** - Management shares for crop residues (harvested vs. left in field)
- **`Root_Shoot_ratios`** - Root to shoot biomass ratios
- **`Harvest_indices`** - Harvest index values for different crops

### Scaling and Conversion Factors
- **`Weed_NPP_Scaling`** - Scaling factors for weed biomass relative to crops
- **`Fallow_cover`** - Vegetation cover fractions on fallow land
- **`Land_scaling`** - Land use intensity scaling factors
- **`Protein_digestibility`** - Protein digestibility coefficients

### Additional Coefficients (12+ more objects)
Including specialized conversion factors for different biomass types and management systems.

## Global Warming Potentials (7 objects)

- **`GWP`** - Complete GWP table with multiple time horizons
- **`GWP_100`** - 100-year GWP values (most commonly used)
- **`GWP_20`** - 20-year GWP values  
- **`GWP_500`** - 500-year GWP values
- **`Gas_categories`** - GHG gas type classifications
- **`IPCC_factors`** - IPCC emission factors
- **`CH4_fossil_biogenic`** - Separate factors for fossil vs biogenic methane

## Biological Nitrogen Fixation (3 objects)

- **`BNF`** - BNF parameters including:
  - Ndfa (% of N derived from atmosphere)
  - Leguminous_share (fraction of legumes)
  - BGN (below-ground nitrogen)
  - NHI (nitrogen harvest index)
- **`Names_BNF`** - BNF nomenclature and crop mappings
- **`Ndfa_ref`** - Reference Ndfa values by crop and region

## Constants and Scalars (6 objects)

### Weed Biomass Constants
- **`Residue_kgC_kgDM_W`** - Carbon content of weed residues (kg C / kg DM)
- **`Root_kgC_kgDM_W`** - Carbon content of weed roots (kg C / kg DM)  
- **`Residue_kgN_kgDM_W`** - Nitrogen content of weed residues (kg N / kg DM)
- **`Root_kgN_kgDM_W`** - Nitrogen content of weed roots (kg N / kg DM)
- **`Root_Shoot_ratio_W`** - Root to shoot ratio for weeds
- **`Rhizod_kgN_kgRootN_W`** - Rhizodeposition nitrogen coefficient for weeds

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

All coefficients and classifications are derived from harmonized datasets including:

- **FAO Statistics** - Production, trade, and commodity balance data
- **IPCC Guidelines** - Emission factors and GWP values  
- **Scientific Literature** - Biomass coefficients and BNF parameters
- **LUH2 Dataset** - Land use harmonization data
- **Nutrient Databases** - Food composition and nutritional data

## Usage Examples

```r
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

For function documentation, use R's help system: `?function_name` or `help(package = "afsetools")`.