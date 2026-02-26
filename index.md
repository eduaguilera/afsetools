# afsetools: Agro-Food System and Environment Tools

`afsetools` (**A**gro-**F**ood **S**ystem and **E**nvironment **Tools**)
is an R package that bundles standardized coefficients, classifications,
and functions for calculating environmental footprints of agro-food
systems and tracing them through global supply chains. It is the shared
analytical backbone for several research projects on global and national
food-system sustainability.

## Overview

| Component              | Description                                                                                                         |
|------------------------|---------------------------------------------------------------------------------------------------------------------|
| **73+ data objects**   | Biomass coefficients, FAO commodity codes, regional classifications, conversion factors, GWP values, BNF parameters |
| **35+ functions**      | NPP calculation, impact allocation, supply-chain tracing, biological N fixation, GHG accounting, diet analysis      |
| **Workflow functions** | End-to-end footprint calculation pipelines (primary production to final consumption)                                |
| **Visualization**      | Consistent ggplot2 themes and color palettes for scientific figures                                                 |

## Installation

### From GitHub (recommended)

``` r
# install.packages("devtools")
devtools::install_github("eduaguilera/afsetools")
```

### From source (development)

``` r
git clone https://github.com/eduaguilera/afsetools.git
# Then in R, from the parent directory:
devtools::install("afsetools")
```

## Architecture

Unlike most R packages, `afsetools` follows a **data-driven,
environment-loading** design. Understanding this pattern is essential
for using the package correctly.

### How it works

    inst/extdata/*.xlsx          load_general_data()        Your analysis code
    ┌──────────────────┐    ┌──────────────────────┐    ┌──────────────────────┐
    │ Codes_coefs.xlsx │───>│ Reads Excel files     │───>│ Biomass_coefs, GWP,  │
    │ Biomass_coefs.xlsx│   │ Creates 73+ objects   │    │ BNF, items_full, ... │
    │ BNF.xlsx         │   │ in calling environment│    │ available as objects  │
    │ GWP.xlsx         │   └──────────────────────┘    │ in your environment   │
    └──────────────────┘                                └──────────────────────┘
                                                                │
                                                                ▼
                                                        ┌──────────────────────┐
                                                        │ Functions reference   │
                                                        │ these objects by name │
                                                        │ (implicit dependency) │
                                                        └──────────────────────┘

1.  **Data files** in `inst/extdata/` contain Excel tables with all
    coefficients and classifications.
2.  **[`load_general_data()`](https://eduaguilera.github.io/afsetools/reference/load_general_data.md)**
    reads those files and creates 73+ named objects **in the calling
    environment** (not in the package namespace).
3.  **Functions** expect those objects to exist in the environment where
    they are called. They are implicit dependencies — not passed as
    arguments.

This means you must always call
[`load_general_data()`](https://eduaguilera.github.io/afsetools/reference/load_general_data.md)
before using most functions:

``` r
library(afsetools)
load_general_data()          # Populates your environment with 73+ objects
load_general_data(load_vectors = TRUE)  # Also loads color palettes and month vectors
```

## Quick Start

``` r
library(afsetools)

# Load all coefficients and classification data
load_general_data()

# Now you have access to:
# - Biomass_coefs: Conversion factors for DM, N, C, energy
# - GWP: Global warming potentials for greenhouse gases
# - BNF: Biological nitrogen fixation parameters
# - items_full, regions_full: Harmonized nomenclatures
# - And 60+ more data objects

# Calculate potential NPP from climate data
npp <- calculate_potential_npp(climate_data)

# Calculate crop NPP components
crop_npp <- calculate_crop_npp(crop_data, harvest_index)

# Run the full footprint pipeline
# (requires workflow objects in environment: CBS, Primary_all, Impact_prod, etc.)
footprints <- calculate_footprints()

# Access results
primary_fp <- footprints$FP_prim
final_fp   <- footprints$FP_final
```

## Documentation

**Online reference**:
[eduaguilera.github.io/afsetools](https://eduaguilera.github.io/afsetools/reference/index.html)

**In R**:

``` r
?load_general_data
?calculate_footprints
?calculate_potential_npp
help(package = "afsetools")
```

**Data objects**: See
[DATA_REFERENCE.md](https://eduaguilera.github.io/afsetools/DATA_REFERENCE.md)
for the full catalogue of the 73+ objects created by
[`load_general_data()`](https://eduaguilera.github.io/afsetools/reference/load_general_data.md).

## Functions

### Data Loading

| Function                                                                                        | Description                                                              |
|-------------------------------------------------------------------------------------------------|--------------------------------------------------------------------------|
| [`load_general_data()`](https://eduaguilera.github.io/afsetools/reference/load_general_data.md) | Load all 73+ coefficient tables and classifications into the environment |
| [`load_vectors()`](https://eduaguilera.github.io/afsetools/reference/load_vectors.md)           | Load color palettes and month-name vectors                               |

### NPP Calculation

| Function                                                                                                                | Aliases                                                                                                           | Description                                          |
|-------------------------------------------------------------------------------------------------------------------------|-------------------------------------------------------------------------------------------------------------------|------------------------------------------------------|
| [`calculate_potential_npp()`](https://eduaguilera.github.io/afsetools/reference/calculate_potential_npp.md)             | [`Calc_NPP_potentials()`](https://eduaguilera.github.io/afsetools/reference/calculate_potential_npp.md)           | Potential NPP (Miami, NCEAS, Rosenzweig models)      |
| [`calculate_crop_npp()`](https://eduaguilera.github.io/afsetools/reference/calculate_crop_npp.md)                       | [`Calculate_crop_NPP()`](https://eduaguilera.github.io/afsetools/reference/calculate_crop_npp.md)                 | Crop NPP components (product, residue, root biomass) |
| [`calculate_npp_dm_c_n()`](https://eduaguilera.github.io/afsetools/reference/calculate_npp_dm_c_n.md)                   | [`Calc_NPP_DM_C_N()`](https://eduaguilera.github.io/afsetools/reference/calculate_npp_dm_c_n.md)                  | Convert NPP to dry matter, carbon, and nitrogen      |
| [`calculate_crop_npp_components()`](https://eduaguilera.github.io/afsetools/reference/calculate_crop_npp_components.md) | [`Calc_CropNPP_components()`](https://eduaguilera.github.io/afsetools/reference/calculate_crop_npp_components.md) | Complete cropland NPP including weed biomass         |

> **Note on naming**: NPP functions were recently renamed to
> `snake_case`. The old PascalCase names are kept as exported aliases
> for backward compatibility.

### Impact Tracing

| Function                                                                                                              | Description                                        |
|-----------------------------------------------------------------------------------------------------------------------|----------------------------------------------------|
| [`Prepare_prim()`](https://eduaguilera.github.io/afsetools/reference/Prepare_prim.md)                                 | Prepare primary production data for impact tracing |
| [`Allocate_impacts_to_products()`](https://eduaguilera.github.io/afsetools/reference/Allocate_impacts_to_products.md) | Economic allocation of impacts to co-products      |
| [`calc_avail_fp_gt()`](https://eduaguilera.github.io/afsetools/reference/calc_avail_fp_gt.md)                         | Availability footprint using gross trade           |
| [`calc_avail_fp_dtm()`](https://eduaguilera.github.io/afsetools/reference/calc_avail_fp_dtm.md)                       | Availability footprint using detailed trade matrix |
| [`Calc_impact_processed()`](https://eduaguilera.github.io/afsetools/reference/Calc_impact_processed.md)               | Trace impacts through processing chains            |
| [`Agg_primary()`](https://eduaguilera.github.io/afsetools/reference/Agg_primary.md)                                   | Aggregate primary products to CBS items            |
| [`Agg_processed()`](https://eduaguilera.github.io/afsetools/reference/Agg_processed.md)                               | Aggregate processed products by origin             |
| [`get_global_export_footprint()`](https://eduaguilera.github.io/afsetools/reference/get_global_export_footprint.md)   | Global export-weighted footprint                   |

### End-to-End Workflows

| Function                                                                                              | Description                                                                            |
|-------------------------------------------------------------------------------------------------------|----------------------------------------------------------------------------------------|
| [`calculate_footprints()`](https://eduaguilera.github.io/afsetools/reference/calculate_footprints.md) | Full pipeline: primary production → processing → trade → final availability footprints |
| [`extract_luh2()`](https://eduaguilera.github.io/afsetools/reference/extract_luh2.md)                 | Extract carbon stock and area data from LUH2 rasters                                   |

### Analysis

| Function                                                                                                  | Description                                                  |
|-----------------------------------------------------------------------------------------------------------|--------------------------------------------------------------|
| [`Calc_N_fix()`](https://eduaguilera.github.io/afsetools/reference/Calc_N_fix.md)                         | Biological nitrogen fixation (crop, weed, non-symbiotic)     |
| [`Gases_GWP()`](https://eduaguilera.github.io/afsetools/reference/Gases_GWP.md)                           | Classify GHG emissions and calculate GWP-100 CO₂ equivalents |
| [`Calc_diets()`](https://eduaguilera.github.io/afsetools/reference/Calc_diets.md)                         | Nutrient composition of diets                                |
| [`calculate_land_scaling()`](https://eduaguilera.github.io/afsetools/reference/calculate_land_scaling.md) | Land scaling factors for cropping intensity                  |
| [`scale_land()`](https://eduaguilera.github.io/afsetools/reference/scale_land.md)                         | Apply land scaling adjustments                               |
| [`filter_areas()`](https://eduaguilera.github.io/afsetools/reference/filter_areas.md)                     | Filter and reshape FAO area/production data                  |
| [`get_herbwoody_fao()`](https://eduaguilera.github.io/afsetools/reference/get_herbwoody_fao.md)           | Extract herbaceous/woody cover shares from FAO data          |

### Biomass Processing

| Function                                                                                        | Description                              |
|-------------------------------------------------------------------------------------------------|------------------------------------------|
| [`integrate_fallow()`](https://eduaguilera.github.io/afsetools/reference/integrate_fallow.md)   | Integrate fallow land into cropland data |
| [`residues_as_items()`](https://eduaguilera.github.io/afsetools/reference/residues_as_items.md) | Convert crop residues to item-level data |
| [`residue_use()`](https://eduaguilera.github.io/afsetools/reference/residue_use.md)             | Calculate residue management shares      |

### Feed Distribution

| Function                                                                                        | Description                                                    |
|-------------------------------------------------------------------------------------------------|----------------------------------------------------------------|
| [`redistribute_feed()`](https://eduaguilera.github.io/afsetools/reference/redistribute_feed.md) | Redistribute livestock feed demand across products and regions |

### Utilities

| Function                                                                                      | Description                                                 |
|-----------------------------------------------------------------------------------------------|-------------------------------------------------------------|
| [`Filling()`](https://eduaguilera.github.io/afsetools/reference/Filling.md)                   | Gap-fill time series using last-observation-carried-forward |
| [`FillingProxy()`](https://eduaguilera.github.io/afsetools/reference/FillingProxy.md)         | Gap-fill using proxy series                                 |
| [`fill_na_with_sum()`](https://eduaguilera.github.io/afsetools/reference/fill_na_with_sum.md) | Replace NA with sum of other columns                        |
| `%!in%`                                                                                       | Not-in operator (inverse of `%in%`)                         |
| [`drop_cols()`](https://eduaguilera.github.io/afsetools/reference/drop_cols.md)               | Drop columns by name                                        |
| [`is_empty()`](https://eduaguilera.github.io/afsetools/reference/is_empty.md)                 | Test if object is empty                                     |
| [`Arrange_dates()`](https://eduaguilera.github.io/afsetools/reference/Arrange_dates.md)       | Sort data frames by date columns                            |
| [`add_xlsx_sheet()`](https://eduaguilera.github.io/afsetools/reference/add_xlsx_sheet.md)     | Add sheets to Excel workbooks                               |

### Visualization

| Function                                                                                      | Description                                           |
|-----------------------------------------------------------------------------------------------|-------------------------------------------------------|
| [`theme_new()`](https://eduaguilera.github.io/afsetools/reference/theme_new.md)               | Clean ggplot2 theme for scientific publications       |
| [`theme_nolabel()`](https://eduaguilera.github.io/afsetools/reference/theme_nolabel.md)       | Theme variant without facet strip labels              |
| [`ggplotRegression()`](https://eduaguilera.github.io/afsetools/reference/ggplotRegression.md) | Plot linear regression with R², p-value, and equation |

## Package Data

After calling
[`load_general_data()`](https://eduaguilera.github.io/afsetools/reference/load_general_data.md),
you’ll have access to:

**Nomenclatures and Classifications** (~35 objects): `items_full`,
`items_prod_full`, `items_cbs`, `regions_full`, `regions_full_uISO3`,
`Cats`, `Cats_proc`, `Primary_double`, `Secondary_double`, and 25+ more
classification tables.

**Biomass Coefficients** (~17 objects): `Biomass_coefs` (main
coefficient table: DM, N, C, energy content, root:shoot ratios, harvest
indices), `Root_ref`, `Weed_NPP_Scaling`, `Residue_Shares`,
`Fallow_cover`, and 12+ more.

**Global Warming Potentials** (~7 objects): `GWP`, `GWP_100`, and 5
gas-specific GWP tables.

**Biological N Fixation** (3 objects): `BNF`, `Names_BNF`, `Ndfa_ref`.

**Derived Constants** (6 scalars): `Residue_kgC_kgDM_W`,
`Root_kgC_kgDM_W`, `Residue_kgN_kgDM_W`, `Root_kgN_kgDM_W`,
`Root_Shoot_ratio_W`, `Rhizod_kgN_kgRootN_W` — all derived from the
Grass row in `Biomass_coefs`.

**Color Palettes** (loaded with `load_vectors = TRUE`): `Total_color`,
`SOM_color`, `GHG_color`, `N_color`, `P_color`, `Land_color`,
`Water_color`, `Energy_color`, `Month_names`, `Month_numbers`.

### Data Files

The raw input data shipped in `inst/extdata/`:

| File                 | Contents                                                                                     |
|----------------------|----------------------------------------------------------------------------------------------|
| `Codes_coefs.xlsx`   | 48 sheets: FAO commodity codes, regional mappings, processing coefficients, price references |
| `Biomass_coefs.xlsx` | Crop-specific DM, N, C, energy, root:shoot ratios, harvest indices                           |
| `BNF.xlsx`           | Biological nitrogen fixation parameters (Ndfa, leguminous shares)                            |
| `GWP.xlsx`           | IPCC GWP values (AR5/AR6) for multiple time horizons                                         |

## Example Workflow

``` r
library(afsetools)
library(dplyr)

# 1. Load all data
load_general_data()

# 2. Prepare your workflow objects in the environment:
#    CBS, Primary_all, Impact_prod, Crop_NPPr_NoFallow, Feed_intake,
#    Primary_prices, CBS_item_prices, Processing_coefs, Relative_residue_price

# 3. Calculate complete footprints
#    Omit `dtm` for gross trade; pass dtm = your_trade_matrix for bilateral trade
results <- calculate_footprints()

# 4. Visualize results
library(ggplot2)

results$FP_final |>
  dplyr::filter(Impact == "GHG", Year == 2020) |>
  ggplot(aes(x = area, y = Impact_u, fill = Element)) +
  geom_bar(stat = "identity") +
  theme_new() +
  labs(title = "GHG Footprint by Product Group, 2020")
```

## Related Repositories

`afsetools` provides the shared analytical core for these projects:

| Repository                                                  | Description                                                                                                      |
|-------------------------------------------------------------|------------------------------------------------------------------------------------------------------------------|
| [**Global**](https://github.com/eduaguilera/Global)         | Global agro-food system environmental footprints — cropland NPP, supply-chain impacts, trade-adjusted footprints |
| [**Spain_Hist**](https://github.com/eduaguilera/Spain_Hist) | Historical environmental footprints of the Spanish agro-food system                                              |

Both repositories call
[`library(afsetools)`](https://eduaguilera.github.io/afsetools) followed
by `load_general_data(load_vectors = TRUE)` and rely on functions and
data objects exported by this package.

## Dependencies

**Runtime** (Imports): `data.table`, `dplyr`, `ggplot2`, `rlang`,
`readxl`, `openxlsx`, `tibble`, `tidyr`, `zoo`

**Optional** (Suggests): `methods`, `raster`, `sf`, `testthat`, `knitr`,
`rmarkdown`, `pkgdown`

Requires **R ≥ 4.1.0** (for the native pipe `|>`).

## Citation

If you use this package in your research, please cite:

> Aguilera, E., et al. (2025). afsetools: Agro-Food System and
> Environment Tools. R package version 0.1.4.
> <https://github.com/eduaguilera/afsetools>

## License

MIT © Eduardo Aguilera

## Author

Eduardo Aguilera (<eduardo.aguilera@upm.es>)  
ORCID: [0000-0002-0429-0883](https://orcid.org/0000-0002-0429-0883)

## Contributing

Contributions are welcome! Please see
[CONTRIBUTING.md](https://eduaguilera.github.io/afsetools/CONTRIBUTING.md)
for guidelines.

## Development

``` r
# Clone the repository
git clone https://github.com/eduaguilera/afsetools.git
cd afsetools

# Install development dependencies
devtools::install_dev_deps()

# Run tests
devtools::test()

# Regenerate documentation
devtools::document()

# Full R CMD check
devtools::check()
```
