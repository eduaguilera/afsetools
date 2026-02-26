# Package index

## Data Loading

Load coefficients, classifications, and foundational data objects.

- [`load_general_data()`](https://eduaguilera.github.io/afsetools/reference/load_general_data.md)
  : Load General Data, Codes, Coefficients, and Vectors
- [`load_vectors()`](https://eduaguilera.github.io/afsetools/reference/load_vectors.md)
  : Load Color Palettes, Factor Levels, and Categorical Vectors

## NPP Calculation Functions

Net Primary Productivity calculation and conversion functions.

- [`calculate_potential_npp()`](https://eduaguilera.github.io/afsetools/reference/calculate_potential_npp.md)
  : Calculate Potential Net Primary Production (NPP)
- [`calculate_crop_npp()`](https://eduaguilera.github.io/afsetools/reference/calculate_crop_npp.md)
  : Calculate Crop Net Primary Production Components
- [`calculate_npp_dm_c_n()`](https://eduaguilera.github.io/afsetools/reference/calculate_npp_dm_c_n.md)
  : Calculate NPP in Dry Matter, Carbon, and Nitrogen
- [`calculate_crop_npp_components()`](https://eduaguilera.github.io/afsetools/reference/calculate_crop_npp_components.md)
  : Calculate Cropland NPP Components Including Weeds

## Impact Tracing Functions

Supply chain impact allocation and tracing functions.

- [`Prepare_prim()`](https://eduaguilera.github.io/afsetools/reference/Prepare_prim.md)
  : Impact Tracing Functions
- [`Allocate_impacts_to_products()`](https://eduaguilera.github.io/afsetools/reference/Allocate_impacts_to_products.md)
  : Allocate Impacts to Products Based on Economic Value
- [`get_global_export_footprint()`](https://eduaguilera.github.io/afsetools/reference/get_global_export_footprint.md)
  : Get Global Average Export Footprint
- [`calc_avail_fp_gt()`](https://eduaguilera.github.io/afsetools/reference/calc_avail_fp_gt.md)
  : Calculate Available Footprint Using Gross Trade
- [`calc_avail_fp_dtm()`](https://eduaguilera.github.io/afsetools/reference/calc_avail_fp_dtm.md)
  : Calculate Available Footprint Using Detailed Trade Matrix
- [`Calc_impact_processed()`](https://eduaguilera.github.io/afsetools/reference/Calc_impact_processed.md)
  : Calculate Embodied Impact of Processed Products
- [`Agg_primary()`](https://eduaguilera.github.io/afsetools/reference/Agg_primary.md)
  : Aggregate Primary Products to CBS Items
- [`Agg_processed()`](https://eduaguilera.github.io/afsetools/reference/Agg_processed.md)
  : Aggregate Processed Products from Countries to Origins

## Analysis Functions

Specialized environmental impact calculations.

- [`Calc_N_fix()`](https://eduaguilera.github.io/afsetools/reference/Calc_N_fix.md)
  : Biological Nitrogen Fixation Functions
- [`Gases_GWP()`](https://eduaguilera.github.io/afsetools/reference/Gases_GWP.md)
  : Classify GHG Emissions and Calculate Global Warming Potential
- [`Calc_diets()`](https://eduaguilera.github.io/afsetools/reference/Calc_diets.md)
  : Calculate Nutrient Composition of Diets
- [`filter_areas()`](https://eduaguilera.github.io/afsetools/reference/filter_areas.md)
  : Filter and Aggregate Areas by Polity
- [`get_herbwoody_fao()`](https://eduaguilera.github.io/afsetools/reference/get_herbwoody_fao.md)
  : Get Herbaceous and Woody Land from FAO
- [`calculate_land_scaling()`](https://eduaguilera.github.io/afsetools/reference/calculate_land_scaling.md)
  : Calculate Land Scaling Factors
- [`scale_land()`](https://eduaguilera.github.io/afsetools/reference/scale_land.md)
  : Scale Land Area by Cropping Intensity

## Main Workflow Functions

Complete footprint calculation pipelines.

- [`calculate_footprints()`](https://eduaguilera.github.io/afsetools/reference/calculate_footprints.md)
  : Calculate Complete Environmental Footprints Along Supply Chains
- [`extract_luh2()`](https://eduaguilera.github.io/afsetools/reference/extract_luh2.md)
  : Extract Data from Land-Use Harmonization 2 (LUH2) Dataset

## Feed Distribution Functions

Livestock feed demand redistribution and allocation.

- [`redistribute_feed()`](https://eduaguilera.github.io/afsetools/reference/redistribute_feed.md)
  : Redistribute available supply among Livestock_cat based on their
  demand

## Biomass Processing Functions

Functions for biomass data processing including fallow integration and
residue handling.

- [`biomass_functions`](https://eduaguilera.github.io/afsetools/reference/biomass_functions.md)
  : Biomass Processing Functions
- [`integrate_fallow()`](https://eduaguilera.github.io/afsetools/reference/integrate_fallow.md)
  : Integrate Fallow Area into Cropland Area
- [`residues_as_items()`](https://eduaguilera.github.io/afsetools/reference/residues_as_items.md)
  : Classify Residues as CBS Items
- [`residue_use()`](https://eduaguilera.github.io/afsetools/reference/residue_use.md)
  : Calculate Residue Use from Crop NPP Data

## Utility Functions

Helper functions for data manipulation and gap-filling.

- [`utility_functions`](https://eduaguilera.github.io/afsetools/reference/utility_functions.md)
  : Utility Functions for Data Manipulation
- [`` `%!in%` ``](https://eduaguilera.github.io/afsetools/reference/grapes-not-in-grapes.md)
  : Exclude operator
- [`drop_cols()`](https://eduaguilera.github.io/afsetools/reference/drop_cols.md)
  : Drop columns from data frame
- [`is_empty()`](https://eduaguilera.github.io/afsetools/reference/is_empty.md)
  : Check if dataset is empty
- [`Arrange_dates()`](https://eduaguilera.github.io/afsetools/reference/Arrange_dates.md)
  : Arrange data by dates
- [`add_xlsx_sheet()`](https://eduaguilera.github.io/afsetools/reference/add_xlsx_sheet.md)
  : Add sheet to existing Excel workbook
- [`Filling()`](https://eduaguilera.github.io/afsetools/reference/Filling.md)
  : Fill gaps in time series
- [`FillingProxy()`](https://eduaguilera.github.io/afsetools/reference/FillingProxy.md)
  : Fill gaps using proxy variable
- [`fill_na_with_sum()`](https://eduaguilera.github.io/afsetools/reference/fill_na_with_sum.md)
  : Fill gaps with accumulated sum

## Visualization Functions

Plotting themes and visualization utilities.

- [`theme_new()`](https://eduaguilera.github.io/afsetools/reference/theme_new.md)
  : ggplot2 Themes for Scientific Plots
- [`theme_nolabel()`](https://eduaguilera.github.io/afsetools/reference/theme_nolabel.md)
  : ggplot2 Theme Without Labels
- [`ggplotRegression()`](https://eduaguilera.github.io/afsetools/reference/ggplotRegression.md)
  : Plot Linear Regression with Statistics

## Data Objects

Data objects loaded by load_general_data(). See DATA_REFERENCE.md for
details.

- [`afsetools-data`](https://eduaguilera.github.io/afsetools/reference/afsetools-data.md)
  : Data Objects Loaded by load_general_data()
- [`Biomass_coefs`](https://eduaguilera.github.io/afsetools/reference/Biomass_coefs.md)
  : Biomass Coefficients
- [`GWP`](https://eduaguilera.github.io/afsetools/reference/GWP.md) :
  Global Warming Potentials
- [`BNF`](https://eduaguilera.github.io/afsetools/reference/BNF.md) :
  Biological Nitrogen Fixation Parameters
- [`items_full`](https://eduaguilera.github.io/afsetools/reference/items_full.md)
  : Item Classifications
- [`regions_full`](https://eduaguilera.github.io/afsetools/reference/regions_full.md)
  : Regional Classifications
