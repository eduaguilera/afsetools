# afsetools Reference Manual

Complete reference for all functions and data objects in the `afsetools` package.

## Table of Contents

- [Data Loading Functions](#data-loading-functions)
- [NPP Calculation Functions](#npp-calculation-functions)
- [Impact Tracing Functions](#impact-tracing-functions)
- [Analysis Functions](#analysis-functions)
- [Utility Functions](#utility-functions)
- [Plotting Functions](#plotting-functions)
- [Main Workflow Functions](#main-workflow-functions)
- [Data Objects](#data-objects)

---

## Data Loading Functions

### `load_general_data(path = NULL)`

**Description:** Loads all foundational data objects from Excel files in inst/extdata/. This function creates 73 objects in the calling environment including codes, coefficients, and classifications.

**Parameters:**
- `path` - Optional character string with path to data directory. If NULL (default), uses system.file("extdata", package = "afsetools")

**Returns:** NULL (loads objects into parent environment)

**Details:** Loads 35 code/nomenclature objects, 17 biomass coefficient objects, 7 GWP objects, 3 BNF objects, and 6 miscellaneous coefficient scalars.

**Usage:**
```r
load_general_data()
# Now you have access to all 73 data objects
```

---

## NPP Calculation Functions

### `Calc_NPP_potentials(Dataset)`

**Description:** Calculates potential NPP using various models including Miami, NCEAS, and Rosenzweig. Estimates ecosystem productivity based on temperature and precipitation.

**Parameters:**
- `Dataset` - Data frame containing climate data with columns: TMP (temperature), MAP (precipitation), PET (potential evapotranspiration), AET (actual evapotranspiration)

**Returns:** Data frame with calculated NPP values from different models in MgDM/ha

**Models included:**
- Miami model (temperature and precipitation limited)
- NCEAS model (for tree and non-tree vegetation)  
- Rosenzweig model (based on AET)

**Usage:**
```r
climate_data <- data.frame(TMP = 15, MAP = 800, PET = 1000, AET = 700)
npp_results <- Calc_NPP_potentials(climate_data)
```

### `Calculate_crop_NPP(Dataset, HI, ...)`

**Description:** Calculates the components of crop NPP including product, residue, and root biomass in dry matter units.

**Parameters:**
- `Dataset` - Data frame with crop area and production data
- `HI` - Data frame with harvest index (HI) values
- `...` - Additional grouping variables to preserve in output

**Returns:** Data frame with NPP components (Prod_MgDM, Residue_MgDM, Root_MgDM)

**Usage:**
```r
crop_npp <- Calculate_crop_NPP(crop_data, harvest_index)
```

### `Calc_NPP_DM_C_N(AreaNPP)`

**Description:** Converts NPP values to dry matter, carbon, and nitrogen content using standard coefficients.

**Parameters:**
- `AreaNPP` - Data frame with NPP values per area

**Returns:** Data frame with NPP in dry matter, carbon, and nitrogen units

### `Calc_CropNPP_components(Crop_NPPpot)`

**Description:** Complete cropland NPP calculation including crop, residue, root, and weed biomass components.

**Parameters:**
- `Crop_NPPpot` - Data frame with potential crop NPP values

**Returns:** Data frame with all NPP components for cropland systems

---

## Impact Tracing Functions

### `Prepare_prim(Prim_all)`

**Description:** Prepares primary production data for impact tracing by standardizing formats and adding necessary classifications.

**Parameters:**
- `Prim_all` - Primary production data frame

**Returns:** Standardized primary production data frame ready for impact allocation

### `Allocate_impacts_to_products(df)`

**Description:** Applies economic allocation to distribute environmental impacts among co-products from the same production process.

**Parameters:**
- `df` - Data frame with production impacts and economic values

**Returns:** Data frame with impacts allocated to individual products

### `calc_avail_fp_gt(filtered_cbs, df)`

**Description:** Calculates availability footprint using gross trade data. Traces impacts through international trade flows.

**Parameters:**
- `filtered_cbs` - Commodity Balance Sheets data
- `df` - Impact data frame

**Returns:** Data frame with trade-adjusted footprints

### `calc_avail_fp_dtm(filtered_cbs, df)`

**Description:** Calculates availability footprint using detailed trade matrix (bilateral trade flows).

**Parameters:**
- `filtered_cbs` - Commodity Balance Sheets data  
- `df` - Impact data frame

**Returns:** Data frame with bilateral trade-adjusted footprints

### `Calc_impact_processed(df)`

**Description:** Traces impacts through processing chains from primary to processed products.

**Parameters:**
- `df` - Primary product impact data frame

**Returns:** Data frame with processed product impacts

### `Agg_primary(df)`

**Description:** Aggregates primary products to CBS (Commodity Balance Sheet) item level.

**Parameters:**
- `df` - Primary product data frame

**Returns:** Aggregated data frame at CBS item level

### `Agg_processed(df)`

**Description:** Aggregates processed products by country/region of origin.

**Parameters:**
- `df` - Processed product data frame

**Returns:** Aggregated processed product data frame

---

## Analysis Functions

### `Calc_N_fix(x)`

**Description:** Calculate biological nitrogen fixation from crops, weeds, and non-symbiotic sources.

**Parameters:**
- `x` - Data frame with crop NPP data including columns: Crop_NPP_MgN, Prod_MgN, Weeds_NPP_MgN, LandUse, Area_ygpit_ha

**Returns:** Data frame with BNF calculations: CropBNF, WeedsBNF, NSBNF, and total BNF

**Components calculated:**
- Crop BNF (from leguminous crops)
- Weeds BNF (from nitrogen-fixing weeds)
- Non-symbiotic BNF (from free-living bacteria)

**Usage:**
```r
bnf_results <- Calc_N_fix(npp_data)
```

### `Gases_GWP(x)`

**Description:** Classify GHG emissions and calculate GWP100 CO2 equivalents for different greenhouse gases.

**Parameters:**
- `x` - Data frame with GHG emission data

**Returns:** Data frame with GWP100 values and classifications

### `Calc_diets(PIE_dest_df, Pop)`

**Description:** Calculate nutrient composition of diets from food supply data.

**Parameters:**
- `PIE_dest_df` - Food supply data frame
- `Pop` - Population data frame

**Returns:** Data frame with dietary nutrient composition

### `calculate_land_scaling()`

**Description:** Calculate land scaling factors for cropping intensity adjustments.

**Returns:** Data frame with land scaling coefficients

### `scale_land()`

**Description:** Apply land scaling adjustments to account for multiple cropping and fallow periods.

**Returns:** Scaled land use data frame

### `get_herbwoody_fao()`

**Description:** Extract herbaceous and woody vegetation data from FAO sources.

**Returns:** Data frame with vegetation classifications

---

## Utility Functions

### `%!in%`

**Description:** Negation of the `%in%` operator for logical operations.

**Usage:**
```r
c(1, 2, 3) %!in% c(2, 4, 6) # Returns TRUE FALSE TRUE
```

### `drop_cols(df, ...)`

**Description:** Drops an undefined number of columns from a data frame.

**Parameters:**
- `df` - Data frame
- `...` - Column names to drop

**Returns:** Data frame without the specified columns

### `is_empty(df)`

**Description:** Checks if a data frame has no rows.

**Parameters:**
- `df` - Data frame to check

**Returns:** Logical indicating if data frame has zero rows

### `Arrange_dates(x)`

**Description:** Sort data frames by date columns, handling various date formats.

**Parameters:**
- `x` - Data frame with date columns

**Returns:** Data frame sorted by dates

### `add_xlsx_sheet(wb_location, newsheet_name, newsheet_data)`

**Description:** Add sheets to Excel workbooks programmatically.

**Parameters:**
- `wb_location` - Path to Excel workbook
- `newsheet_name` - Name for new sheet
- `newsheet_data` - Data to write to sheet

**Returns:** Updated Excel workbook

### `Filling(data, var, Index)`

**Description:** Gap-fill time series data using interpolation and extrapolation methods.

**Parameters:**
- `data` - Data frame with missing values
- `var` - Variable name to fill
- `Index` - Grouping variables for filling

**Returns:** Data frame with filled values

### `FillingProxy(data, var, proxyvar, Index)`

**Description:** Gap-fill data using proxy variables when direct interpolation is not possible.

**Parameters:**
- `data` - Data frame with missing values
- `var` - Variable to fill
- `proxyvar` - Proxy variable for estimation
- `Index` - Grouping variables

**Returns:** Data frame with proxy-filled values

### `fill_na_with_sum(col1, col2)`

**Description:** Fill NA values in one column with sum of two other columns.

**Parameters:**
- `col1`, `col2` - Columns to sum for filling

**Returns:** Vector with filled values

---

## Plotting Functions

### `theme_new(base_size = 7, base_family = "")`

**Description:** Clean ggplot2 theme for scientific plots with minimal ink and clear typography.

**Parameters:**
- `base_size` - Base font size (default: 7)
- `base_family` - Font family (default: "")

**Returns:** ggplot2 theme object

**Features:**
- Minimal background and gridlines
- Clean axis formatting
- Optimized for scientific publications

**Usage:**
```r
ggplot(data, aes(x, y)) + 
  geom_point() + 
  theme_new()
```

### `theme_nolabel(base_size = 7, base_family = "")`

**Description:** Theme variant without facet labels for multi-panel plots.

**Parameters:**
- `base_size` - Base font size (default: 7)
- `base_family` - Font family (default: "")

**Returns:** ggplot2 theme object without facet labels

### `ggplotRegression(fit)`

**Description:** Plot linear regression with statistics and confidence intervals.

**Parameters:**
- `fit` - Linear model object (lm)

**Returns:** ggplot2 plot with regression line and statistics

**Features:**
- Automatic R² and p-value display
- Confidence intervals
- Standardized formatting

---

## Main Workflow Functions

### `calculate_footprints(CBS, Primary_all, Impact_prod, Crop_NPPr_NoFallow, DTM = NULL, trade_mode = "gt")`

**Description:** Main workflow function that traces environmental footprints through global supply chains, from primary production through processing, feed, and final products.

**Parameters:**
- `CBS` - Commodity Balance Sheets data frame
- `Primary_all` - Primary production data frame  
- `Impact_prod` - Impact data at production level
- `Crop_NPPr_NoFallow` - Crop NPP data excluding fallow periods
- `DTM` - Detailed Trade Matrix data (optional, depending on trade_mode)
- `trade_mode` - Character string: "gt" for gross trade or "dtm" for detailed trade matrix

**Returns:** Named list containing all footprint data frames:
- `FP_prim` - Primary production footprints with economic allocation
- `FP_prim_ds` - Primary product footprints including domestic supply
- `FP_processed_raw` - Processed product footprints (raw calculation)
- `FP_processed_ds` - Processed product footprints with domestic supply
- `FP_feed` - Feed product footprints
- `FP_feed_ds` - Feed product footprints with domestic supply
- `FP_final` - Final comprehensive footprint data frame
- `Seed_share` - Calculated seed shares by crop
- `draught_shares` - Draught animal allocation shares

**Process:**
1. Calculates seed shares and removes them from production
2. Allocates impacts to co-products using economic allocation
3. Traces impacts through processing chains
4. Accounts for international trade
5. Handles feed products and livestock production
6. Allocates draught animal services to crop production

**Usage:**
```r
footprints <- calculate_footprints(
  CBS = my_cbs_data,
  Primary_all = my_primary_data,
  Impact_prod = my_impact_data,
  Crop_NPPr_NoFallow = my_npp_data,
  trade_mode = "gt"
)
```

### `extract_luh2(L_files_path, RegionNames, studied_period)`

**Description:** Extract carbon stock and area data from Land-Use Harmonization 2 (LUH2) dataset.

**Parameters:**
- `L_files_path` - Path to LUH2 NetCDF files
- `RegionNames` - Vector of region names to extract
- `studied_period` - Time period vector for extraction

**Returns:** Data frame with LUH2 land use and carbon data

**Features:**
- NetCDF file handling
- Multi-region extraction
- Time series processing
- Carbon stock calculations

---

## Data Objects

After calling `load_general_data()`, you'll have access to 73 data objects organized in the following categories:

### Nomenclatures and Classifications (35 objects)

**Core Classifications:**
- `items_full` - Complete item codes and names
- `items_prod_full` - Production item nomenclature
- `items_cbs` - CBS item codes
- `regions_full` - Country codes and regional aggregations
- `regions_full_uISO3` - ISO3 country codes
- `Cats` - Product categories
- `Primary_double` - Multi-product processes
- `Secondary_double` - Secondary product processes

**Specific Classifications:**
- `Animals_codes` - Livestock species codes
- `Names_Liv_Prod` - Livestock product names
- `CBS_Trade_codes` - Trade classification codes
- `CB_processing` - Processing chains
- `items_proc_fabio` - FABIO processed items
- `FishStat_items` - Fish and seafood items
- `Crops_Eurostat` - European crop classifications
- `Names_biomass` - Biomass type names
- `Crop_Names` - Crop-specific names

**Regional Classifications:**
- `region_krausmann` - Krausmann regional groupings
- `polities_whep` - WHEP political entities
- `regions_fabio` - FABIO regional classifications

### Biomass Coefficients (17 objects)

**Main Coefficient Table:**
- `Biomass_coefs` - Primary coefficient table with DM, N, C, energy content, prices, harvest indices, etc.

**Specific Coefficients:**
- `Root_ref` - Reference root biomass values
- `Weed_NPP_Scaling` - Weed biomass scaling factors
- `Residue_Shares` - Crop residue management shares
- `Fallow_cover` - Fallow land cover fractions
- `HI_changes_krausmann` - Harvest index changes over time
- `residue_krausmann` - Residue coefficients
- `Liv_LU_coefs` - Livestock land use coefficients
- `conv_bouwman` - Bouwman conversion factors
- `conv_krausmann` - Krausmann conversion factors

**Weed Parameters:**
- `Residue_kgDM_kgFM_W` - Weed residue dry matter content
- `Root_Shoot_ratio_W` - Weed root:shoot ratios
- `Residue_kgN_kgDM_W` - Weed residue nitrogen content
- `Root_kgN_kgDM_W` - Weed root nitrogen content
- `Residue_kgC_kgDM_W` - Weed residue carbon content
- `Root_kgC_kgDM_W` - Weed root carbon content

### Global Warming Potentials (7 objects)

- `GWP` - Complete GWP table with different time horizons
- `GWP_100` - 100-year GWP values (most commonly used)
- `GWP_C` - Carbon-based GWP values
- `GWP_CO2` - CO₂ GWP values
- `GWP_CH4` - Methane GWP values
- `GWP_CH4_fossil` - Fossil methane GWP values
- `GWP_N2O` - Nitrous oxide GWP values

### Biological N Fixation (3 objects)

- `BNF` - Nitrogen fixation parameters and coefficients
- `Names_BNF` - BNF nomenclature and classifications
- `Ndfa_ref` - Reference Nitrogen derived from atmosphere (Ndfa) values

### Constants (6 scalars)

**Weed Carbon Content:**
- `Residue_kgC_kgDM_W` - Carbon content of weed residues (kg C/kg DM)
- `Root_kgC_kgDM_W` - Carbon content of weed roots (kg C/kg DM)

**Weed Nitrogen Content:**
- `Residue_kgN_kgDM_W` - Nitrogen content of weed residues (kg N/kg DM)
- `Root_kgN_kgDM_W` - Nitrogen content of weed roots (kg N/kg DM)

**Weed Ratios:**
- `Root_Shoot_ratio_W` - Root:shoot ratio for weeds
- `Rhizod_kgN_kgRootN_W` - Rhizodeposition N coefficient (kg N/kg root N)

### Color Palettes and Vectors

**Impact-Specific Colors:**
- `Total_color` - Color for total impact visualizations
- `SOM_color` - Soil organic matter colors
- `GHG_color` - Greenhouse gas colors  
- `N_color` - Nitrogen colors
- `P_color` - Phosphorus colors
- `Land_color` - Land use colors
- `Water_color` - Water use colors
- `Energy_color` - Energy colors

**Utility Vectors:**
- `Month_names` - Month name vector
- `Month_numbers` - Month number vector

### Derived Objects (3 objects)

These are created during the data loading process:

- `regions_full_uISO3` - Unified ISO3 regional classification
- `Names_biomass_cats` - Biomass name categories
- `items_prim` - Primary item classifications

---

## File Structure and Data Sources

The package data comes from four main Excel files in `inst/extdata/`:

1. **`Codes_coefs.xlsx`** (48 sheets) - Codes, coefficients, and classifications
2. **`Biomass_coefs.xlsx`** - Biomass conversion coefficients
3. **`BNF.xlsx`** - Biological nitrogen fixation parameters  
4. **`GWP.xlsx`** - Global warming potentials

These files contain harmonized datasets from multiple sources including FAO, IPCC, scientific literature, and specialized databases for environmental footprint analysis.

---

## Dependencies and Requirements

**Required R packages:**
- `dplyr` - Data manipulation
- `tidyr` - Data tidying
- `ggplot2` - Plotting
- `openxlsx` - Excel file handling
- `purrr` - Functional programming
- `rlang` - Non-standard evaluation

**Suggested packages:**
- `ncdf4` - NetCDF file handling (for LUH2 extraction)

**Minimum R version:** R (≥ 3.5.0)

---

## Citation

When using this package, please cite:

> Aguilera, E., et al. (2025). afsetools: Agro-Food System and Environment Tools. 
> R package version 0.1.0. https://github.com/eduaguilera/afsetools

## Author

Eduardo Aguilera (eduardo.aguilera@upm.es)  
ORCID: [0000-0002-0429-0883](https://orcid.org/0000-0002-0429-0883)