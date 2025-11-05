# afsetools: Agro-Food System and Environment Tools

<!-- badges: start -->
[![R-CMD-check](https://github.com/eduaguilera/afsetools/workflows/R-CMD-check/badge.svg)](https://github.com/eduaguilera/afsetools/actions)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->

Core functions and data for calculating environmental footprints in agro-food systems. This package provides standardized coefficients, classifications, and functions for tracing environmental impacts through global supply chains.

## Overview

`afsetools` contains:

- **73 data objects**: Biomass coefficients, codes, classifications, and conversion factors from harmonized datasets
- **35+ functions**: NPP calculation, impact allocation, supply chain tracing, biological N fixation, GHG emissions, and more
- **Workflow functions**: Complete footprint calculation pipelines
- **Standardized visualizations**: Color palettes and plotting themes for consistent figures

## Installation

```r
# Install from GitHub
# install.packages("devtools")
devtools::install_github("eduaguilera/afsetools")
```

## Quick Start

```r
library(afsetools)

# Load all coefficients and classification data (73 objects)
load_general_data()

# Now you have access to:
# - Biomass_coefs: Conversion factors for DM, N, C, energy
# - GWP: Global warming potentials for greenhouse gases  
# - BNF: Biological nitrogen fixation parameters
# - items_full, regions_full: Harmonized nomenclatures
# - And 60+ more data objects

# Calculate NPP from climate data
npp <- Calc_NPP_potentials(climate_data)

# Calculate crop NPP components
crop_npp <- Calculate_crop_NPP(crop_data, harvest_index)

# Trace impacts through supply chains
footprints <- calculate_footprints(
  CBS = commodity_balance_sheets,
  Primary_all = primary_production,
  Impact_prod = production_impacts,
  Crop_NPPr_NoFallow = crop_npp,
  trade_mode = "gt"  # or "dtm" for bilateral trade
)

# Access results
primary_fp <- footprints$FP_prim
final_fp <- footprints$FP_final
```

## Main Functions

### Data Loading

- `load_general_data()`: Load all 73 coefficient tables and classifications into the environment

### NPP Calculation

- `Calc_NPP_potentials()`: Calculate potential NPP using Miami, NCEAS, and Rosenzweig models
- `Calculate_crop_NPP()`: Calculate crop NPP components (product, residue, root biomass)
- `Calc_NPP_DM_C_N()`: Convert NPP to dry matter, carbon, and nitrogen
- `Calc_CropNPP_components()`: Complete cropland NPP including weed biomass

### Impact Tracing

- `Prepare_prim()`: Prepare primary production data for impact tracing
- `Allocate_impacts_to_products()`: Economic allocation of impacts to co-products
- `calc_avail_fp_gt()`: Calculate availability footprint using gross trade
- `calc_avail_fp_dtm()`: Calculate availability footprint using detailed trade matrix
- `Calc_impact_processed()`: Trace impacts through processing chains
- `Agg_primary()`: Aggregate primary products to CBS items
- `Agg_processed()`: Aggregate processed products by origin

### Comprehensive Workflow

- `calculate_footprints()`: Complete footprint calculation pipeline from primary production to final products
- `extract_luh2()`: Extract carbon stock and area data from Land-Use Harmonization 2 (LUH2) dataset

### Analysis Functions

- `Calc_N_fix()`: Calculate biological nitrogen fixation (crop, weed, non-symbiotic)
- `Gases_GWP()`: Classify GHG emissions and calculate GWP100 CO2 equivalents
- `Calc_diets()`: Calculate nutrient composition of diets
- `calculate_land_scaling()`: Calculate land scaling factors for cropping intensity
- `scale_land()`: Apply land scaling adjustments

### Utilities

- `Filling()`, `FillingProxy()`: Gap-fill time series data
- `%!in%`: Not-in operator
- `Arrange_dates()`: Sort data frames by dates
- `add_xlsx_sheet()`: Add sheets to Excel workbooks

### Visualization

- `theme_new()`: Clean ggplot2 theme for scientific plots
- `theme_nolabel()`: Theme without facet labels
- `ggplotRegression()`: Plot linear regression with statistics

## Package Data

After calling `load_general_data()`, you'll have access to:

**Nomenclatures and Classifications** (35 objects):
- `items_full`, `items_prod_full`, `items_cbs`: Item codes and names
- `regions_full`, `regions_full_uISO3`: Country codes and regional aggregations
- `Cats`, `Cats_proc`: Product categories
- `Primary_double`, `Secondary_double`: Multi-product processes
- And 25+ more classification tables

**Biomass Coefficients** (17 objects):
- `Biomass_coefs`: Main coefficient table (DM, N, C, energy content, etc.)
- `Root_ref`: Reference root biomass
- `Weed_NPP_Scaling`: Weed biomass scaling factors
- `Residue_Shares`: Crop residue management shares
- `Fallow_cover`: Fallow land cover fractions
- And 12+ more coefficient tables

**Global Warming Potentials** (7 objects):
- `GWP`: GWP values for different gases and time horizons
- `GWP_100`: 100-year GWP values
- And 5 more GWP-related objects

**Biological N Fixation** (3 objects):
- `BNF`: Nitrogen fixation parameters
- `Names_BNF`: BNF nomenclature
- `Ndfa_ref`: Reference Ndfa values

**Constants** (6 scalars):
- `Residue_kgC_kgDM_W`, `Root_kgC_kgDM_W`: Carbon content of weeds
- `Residue_kgN_kgDM_W`, `Root_kgN_kgDM_W`: Nitrogen content of weeds
- `Root_Shoot_ratio_W`: Root:shoot ratio for weeds
- `Rhizod_kgN_kgRootN_W`: Rhizodeposition N coefficient

**Color Palettes and Vectors**:
- `Total_color`, `SOM_color`, `GHG_color`, `N_color`, `P_color`, `Land_color`, `Water_color`, `Energy_color`
- `Month_names`, `Month_numbers`: Month utilities

## Data Files

The package bundles standardized input data in `inst/extdata/`:

- `Codes_coefs.xlsx`: 48 sheets with codes, coefficients, and classifications
- `Biomass_coefs.xlsx`: Biomass conversion coefficients  
- `BNF.xlsx`: Biological nitrogen fixation parameters
- `GWP.xlsx`: Global warming potentials

These are automatically loaded by `load_general_data()`.

## Example Workflow

```r
library(afsetools)
library(dplyr)

# 1. Load all data
load_general_data()

# 2. Prepare your input data
# (CBS, Primary_all, Impact_prod, Crop_NPPr_NoFallow)

# 3. Calculate complete footprints
results <- calculate_footprints(
  CBS = my_cbs_data,
  Primary_all = my_primary_data,
  Impact_prod = my_impact_data,
  Crop_NPPr_NoFallow = my_crop_npp,
  trade_mode = "gt"
)

# 4. Analyze results
library(ggplot2)

results$FP_final %>%
  filter(Impact == "GHG", Year == 2020) %>%
  ggplot(aes(x = area, y = Impact_u, fill = Product_group)) +
  geom_bar(stat = "identity") +
  theme_new() +
  labs(title = "GHG Footprint by Product Group, 2020")
```

## Citation

If you use this package in your research, please cite:

> Aguilera, E., et al. (2025). afsetools: Agro-Food System and Environment Tools. 
> R package version 0.1.0. https://github.com/eduaguilera/afsetools

## Related Repositories

This package provides the foundational tools used by:

- **Spain_Hist**: Historical environmental footprints of Spanish agro-food system

## License

MIT © Eduardo Aguilera

## Author

Eduardo Aguilera (eduardo.aguilera@upm.es)  
ORCID: [0000-0002-0429-0883](https://orcid.org/0000-0002-0429-0883)

## Contributing

Contributions are welcome! Please see [CONTRIBUTING.md](CONTRIBUTING.md) for guidelines.

## Development

To contribute to this package:

```r
# Clone the repository
git clone https://github.com/eduaguilera/afsetools.git
cd afsetools

# Install development dependencies
devtools::install_dev_deps()

# Run tests
devtools::test()

# Check package
devtools::check()
```
