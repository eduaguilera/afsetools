# GitHub Copilot Instructions for afsetools Package

## Package Purpose

`afsetools` (Agro-Food System and Environment Tools) is an R package
providing core functions and data for calculating environmental
footprints in agro-food systems. It contains standardized coefficients,
classifications, and functions for tracing environmental impacts through
global supply chains.

## Core Principles

- **Explicit package names**: Always prefix functions (e.g.,
  [`dplyr::mutate()`](https://dplyr.tidyverse.org/reference/mutate.html),
  [`tidyr::pivot_longer()`](https://tidyr.tidyverse.org/reference/pivot_longer.html))
- **Native pipe**: Use `|>` instead of `%>%` (requires R \>= 4.1.0)
- **Type-safe conditionals**: Use
  [`dplyr::if_else()`](https://dplyr.tidyverse.org/reference/if_else.html)
  or
  [`dplyr::case_when()`](https://dplyr.tidyverse.org/reference/case-and-replace-when.html)
  instead of base [`ifelse()`](https://rdrr.io/r/base/ifelse.html)
- **roxygen2 documentation**: All exported functions must have complete
  roxygen2 documentation
- **Package structure**: Follow standard R package conventions (R/,
  man/, tests/, inst/extdata/)
- **Keep data loading flexible**:
  [`load_general_data()`](https://eduaguilera.github.io/afsetools/reference/load_general_data.md)
  should work both as installed package and during development
- **Documentation location**: NEVER create documentation files in
  package root - use `additional_files/documentation/` structure

## Architecture: Data-Driven Design

**CRITICAL**: This package is fundamentally data-driven. The
architecture flows from Excel coefficients → loaded objects →
calculation functions:

1.  **Data files** in `inst/extdata/`: Excel files contain 73+
    coefficient tables and classifications
2.  **[`load_general_data()`](https://eduaguilera.github.io/afsetools/reference/load_general_data.md)**
    in `R/load_data.R`: Loads all objects into user’s environment (NOT
    package namespace)
3.  **Functions** expect these objects to exist in calling environment
    (implicit dependencies)
4.  **Workflow functions** orchestrate complete calculations using
    multiple data objects

**Key insight**: Functions don’t pass data objects as parameters - they
expect them in environment. Always call
[`load_general_data()`](https://eduaguilera.github.io/afsetools/reference/load_general_data.md)
first.

## Environment Object Pattern

Unlike typical R packages, afsetools loads 73+ objects directly into the
user’s environment:

``` r
library(afsetools)
load_general_data()  # Creates Biomass_coefs, GWP, BNF, etc. in environment
npp <- Calc_NPP_potentials(climate_data)  # Functions use environment objects
```

**Implications for development**: - Functions have implicit dependencies
on environment objects - Test files must call
[`load_general_data()`](https://eduaguilera.github.io/afsetools/reference/load_general_data.md)
in setup (see `tests/testthat/setup-afsetools.R`) - Adding new data:
Update
[`load_general_data()`](https://eduaguilera.github.io/afsetools/reference/load_general_data.md),
document in `R/data.R`, list in `DATA_REFERENCE.md`

## Documentation Standards

### Function Documentation (roxygen2)

All exported functions MUST include these tags: - `@description` - Brief
description of what the function does - `@param` (for each parameter) -
Parameter name and description - `@return` - What the function returns -
`@export` - Mark for export in NAMESPACE - `@examples` - Usage examples
(wrap in `\dontrun{}` if requires external data)

**Format requirements**: - One space after `#'` - Maximum line width: 80
characters - Multi-line descriptions indented two spaces - End all
sentences with period - Use proper markdown formatting

**Example**:

``` r
#' Calculate Net Primary Productivity
#'
#' Calculates potential NPP using Miami, NCEAS, and Rosenzweig models from
#' climate data.
#'
#' @param Dataset Data frame containing climate variables: TMP (temperature),
#'   MAP (precipitation), PET (potential evapotranspiration), AET (actual
#'   evapotranspiration)
#'
#' @return Data frame with NPP estimates from three models (Miami, NCEAS,
#'   Rosenzweig) in Mg C/ha/year
#' @export
#'
#' @examples
#' \dontrun{
#' npp_results <- Calc_NPP_potentials(climate_data)
#' }
```

### Data Object Documentation

Data objects loaded by
[`load_general_data()`](https://eduaguilera.github.io/afsetools/reference/load_general_data.md)
should be documented in `R/data.R`: - Use `@format` to describe
structure with `\describe{}` - Include `@source` for data origin - Add
`@docType data` and `@keywords datasets` - Keep descriptions concise but
informative

## Code Style

### Naming Conventions

- **Functions**: `snake_case` or `PascalCase` (be consistent within
  file)
- **Variables/Columns**: `snake_case` preferred
- **Data objects**: Match loaded object names (varies by source)
- **File names**: lowercase with underscores (e.g., `load_data.R`,
  `npp_functions.R`)

### Style Guide

- Follow [Tidyverse style guide](https://style.tidyverse.org/)
- Maximum line width: 80 characters
- Use `.by` for inline grouping:
  `mutate(.by = Year, total = sum(value))`
- Avoid `group_by()` unless required by specific functions
- **CRITICAL**: Always `ungroup()` after `group_by()` operations

### Grouping Pattern (Important!)

``` r
# CORRECT: Use .by for simple operations
df |> mutate(.by = Year, total = sum(value))

# For Filling() and FillingProxy(): Must use group_by/ungroup
df |>
  group_by(Region, Item) |>
  Filling(Production, Year) |>
  ungroup()  # REQUIRED - prevents grouped data flowing downstream
```

## Package Data Management

### Loading Data Objects

- **Input files location**: `inst/extdata/` (Excel files with
  coefficients)
- **Loading function**:
  [`load_general_data()`](https://eduaguilera.github.io/afsetools/reference/load_general_data.md)
  creates 73+ objects in parent environment
- **Access pattern**: Users call
  [`load_general_data()`](https://eduaguilera.github.io/afsetools/reference/load_general_data.md)
  after [`library(afsetools)`](https://eduaguilera.github.io/afsetools)
- **No data in package namespace**: Objects are loaded to user
  environment, not attached to package

### Data File Requirements

- Store coefficient tables in Excel format in `inst/extdata/`
- Use consistent sheet names and column headers
- Document all data sources and references
- Include metadata sheets where appropriate

## Function Organization

### Function Categories

1.  **Data Loading** (`load_data.R`):
    [`load_general_data()`](https://eduaguilera.github.io/afsetools/reference/load_general_data.md) -
    loads 73+ objects
2.  **NPP Calculation** (`npp_functions.R`):
    [`Calc_NPP_potentials()`](https://eduaguilera.github.io/afsetools/reference/calculate_potential_npp.md),
    [`Calculate_crop_NPP()`](https://eduaguilera.github.io/afsetools/reference/calculate_crop_npp.md),
    [`Calc_NPP_DM_C_N()`](https://eduaguilera.github.io/afsetools/reference/calculate_npp_dm_c_n.md),
    [`Calc_CropNPP_components()`](https://eduaguilera.github.io/afsetools/reference/calculate_crop_npp_components.md)
3.  **Impact Tracing** (`impact_functions.R`):
    [`Prepare_prim()`](https://eduaguilera.github.io/afsetools/reference/Prepare_prim.md),
    [`Allocate_impacts_to_products()`](https://eduaguilera.github.io/afsetools/reference/Allocate_impacts_to_products.md),
    [`calc_avail_fp_gt()`](https://eduaguilera.github.io/afsetools/reference/calc_avail_fp_gt.md),
    [`Calc_impact_processed()`](https://eduaguilera.github.io/afsetools/reference/Calc_impact_processed.md)
4.  **Analysis** (`analysis_functions.R`):
    [`Calc_N_fix()`](https://eduaguilera.github.io/afsetools/reference/Calc_N_fix.md),
    [`Gases_GWP()`](https://eduaguilera.github.io/afsetools/reference/Gases_GWP.md),
    [`Calc_diets()`](https://eduaguilera.github.io/afsetools/reference/Calc_diets.md),
    [`calculate_land_scaling()`](https://eduaguilera.github.io/afsetools/reference/calculate_land_scaling.md)
5.  **Utilities** (`utility_functions.R`): `Filling()`,
    `FillingProxy()`, `%!in%`,
    [`drop_cols()`](https://eduaguilera.github.io/afsetools/reference/drop_cols.md)
6.  **Visualization** (`plotting.R`):
    [`theme_new()`](https://eduaguilera.github.io/afsetools/reference/theme_new.md),
    [`theme_nolabel()`](https://eduaguilera.github.io/afsetools/reference/theme_nolabel.md),
    [`ggplotRegression()`](https://eduaguilera.github.io/afsetools/reference/ggplotRegression.md)
7.  **Workflows** (`calculate_footprints.R`): Complete calculation
    pipelines

### Function Design Patterns

**Pipe-friendly functions** (return modified data frame):

``` r
Calculate_crop_NPP <- function(Dataset, HI, ...) {
  Dataset |>
    dplyr::left_join(Biomass_coefs, by = "Name_biomass") |>  # Uses environment object
    dplyr::mutate(Prod_MgDM = Prod_ygpit_Mg * Product_kgDM_kgFM) |>
    dplyr::select(...)
}
```

**Environment object usage** (implicit dependencies):

``` r
Calc_N_fix <- function(x) {
  x |>
    dplyr::left_join(Names_BNF |>  # Names_BNF from environment
      dplyr::left_join(BNF, by = "Name_BNF"), by = "Name_biomass") |>  # BNF from environment
    dplyr::mutate(CropBNF = Crop_NPP_MgN * Ndfa * Leguminous_share)
}
```

## Testing

**Location**: `tests/testthat/` - name as `test-[script-name].R`

**Critical setup pattern**:

``` r
# tests/testthat/setup-afsetools.R
library(afsetools)
load_general_data()  # Makes all 73+ objects available to tests
```

**Testing approach**: - One test file per R script (e.g.,
`test-load_data.R` for `load_data.R`) - Test that
[`load_general_data()`](https://eduaguilera.github.io/afsetools/reference/load_general_data.md)
creates expected objects - Verify coefficient values are realistic
(e.g., DM content 0-1) - Check data structure and required columns

**Useful test patterns**:

``` r
# Test environment loading
test_that("load_general_data creates all required data objects", {
  test_env <- new.env()
  with(test_env, load_general_data())
  expect_true(exists("Biomass_coefs", envir = test_env))
})

# Test coefficient realism
expect_true(all(Biomass_coefs$Product_kgDM_kgFM >= 0 & 
               Biomass_coefs$Product_kgDM_kgFM <= 1, na.rm = TRUE))
```

## Dependencies

**Core tidyverse**: dplyr, tidyr, ggplot2  
**Data I/O**: readxl, openxlsx  
**Utilities**: zoo (for `Filling()` functions)

**Dependency pattern**: Always use explicit `package::function()` syntax
in function code.

## Package Development Workflow

### Making Changes

1.  **Modify R functions**: Update code in `R/` directory
2.  **Update documentation**: Modify roxygen2 comments
3.  **Run document**: `devtools::document()` to update man/ files
4.  **Run tests**: `devtools::test()` to verify nothing breaks
5.  **Check package**: `devtools::check()` for R CMD check
6.  **Build site**: GitHub Actions will rebuild pkgdown site
    automatically

### Before Committing

- Run `devtools::check()` and fix all errors/warnings
- Ensure all tests pass
- Update NEWS.md if adding features or fixing bugs
- Update DESCRIPTION (URL, BugReports) and \_pkgdown.yml reference index
- Document any new data objects in DATA_REFERENCE.md

### pkgdown Configuration

**Two critical files must stay in sync**: 1. **DESCRIPTION**: Must
include `URL:` and `BugReports:` fields 2. \*\*\_pkgdown.yml\*\*: All
documented functions must be listed in `reference:` section

``` yaml
# _pkgdown.yml structure
reference:
- title: Category Name
  contents:
  - function_name  # Must match .Rd filename in man/
```

## Coefficient Management

**CRITICAL: Never hardcode coefficients** - always load from data files
or calculate from source data:

**Sources (in order of preference)**: 1. **Load from inst/extdata
files**: Biomass_coefs.xlsx, GWP.xlsx, BNF.xlsx, etc. 2. **Calculate
from source data**: Document methodology in comments 3. **Scientific
literature**: Always cite sources in comments

**Forbidden patterns**: - ❌ Hardcoded numeric values without source -
❌ Made-up coefficients in code - ❌ Undocumented constants

**Correct patterns**: - ✅
`left_join(Biomass_coefs, by = "Name_biomass")` - ✅ Load from Excel
with documented source - ✅ Calculate with referenced methodology

**Example from code**:

``` r
# Weed coefficients derived from grass biomass coefficients
assign("Root_Shoot_ratio_W", 
       as.numeric(Biomass_coefs |> 
                    dplyr::filter(Name_biomass == "Grass") |> 
                    dplyr::select(Root_Shoot_ratio)),
       envir = env)
```

## Version Control

### Commit Messages

- Use clear, descriptive commit messages
- Reference issues/PRs when applicable
- Group related changes in single commit

### Branch Strategy

- `main` branch should always be stable
- Create feature branches for development
- Use pull requests for review

## Documentation Website

- **Automatic deployment**: GitHub Actions builds pkgdown site on push
- **Site URL**: <https://eduaguilera.github.io/afsetools/>
- **Reference page**:
  <https://eduaguilera.github.io/afsetools/reference/index.html>
- \*\*Update \_pkgdown.yml\*\*: Organize functions by category
- **No manual building needed**: Workflow handles everything

## Additional Files Structure

**CRITICAL: Never create documentation or working files in the package
root!**

Use the `additional_files/` folder for all non-package content. This
folder is excluded from Git via `.gitignore`.

### Folder Organization

    additional_files/
    ├── documentation/
    │   ├── fixes/           # Bug fixes and code changes documentation
    │   ├── development/     # Development notes, design decisions
    │   ├── analysis/        # Analysis documentation, research notes
    │   └── meetings/        # Meeting notes, project discussions
    ├── data/
    │   ├── raw/            # Raw data files (not in inst/extdata/)
    │   ├── processed/      # Intermediate processed data
    │   └── output/         # Analysis outputs, results
    └── scripts/
        ├── development/    # Development/testing scripts
        ├── analysis/       # Analysis scripts
        └── utils/          # Utility scripts

### Documentation Guidelines

**Where to document:** - ❌ **NEVER in root**: No markdown files except
README.md, NEWS.md, CITATION, LICENSE - ✅ **Code changes**:
`additional_files/documentation/fixes/` - ✅ **Development notes**:
`additional_files/documentation/development/` - ✅ **Analysis results**:
`additional_files/documentation/analysis/` - ✅ **Function docs**: `R/`
files using roxygen2 comments - ✅ **Package docs**: `man/`
(auto-generated from roxygen2)

**What goes where:** - **Bug fixes and patches** →
`additional_files/documentation/fixes/` - **Feature development notes**
→ `additional_files/documentation/development/` - **Test results and
debugging** → `additional_files/documentation/fixes/` - **Research and
analysis** → `additional_files/documentation/analysis/` - **Temporary
scripts** → `additional_files/scripts/development/`

**Naming conventions:** - Use descriptive names:
`fix_bnf_calculation_2025-11-06.md` - Include dates for time-sensitive
docs: `YYYY-MM-DD` format - Use lowercase with hyphens:
`migration-summary.md` - Group related files in subdirectories

## Common Tasks

### Adding a New Function

1.  Write function in appropriate R/ file (see function categories
    above)
2.  Add complete roxygen2 documentation
3.  Export if needed (`@export`)
4.  \*\*Add to \_pkgdown.yml reference index\*\* under appropriate
    category
5.  Write tests in tests/testthat/
6.  Run `devtools::document()` and `devtools::check()`
7.  Document development process in
    `additional_files/documentation/development/` if complex

### Adding New Data Objects

1.  Add data file to `inst/extdata/` (Excel format preferred)
2.  Update
    [`load_general_data()`](https://eduaguilera.github.io/afsetools/reference/load_general_data.md)
    to load the new data
3.  Document in `R/data.R` with roxygen2
4.  Add description to `DATA_REFERENCE.md`
5.  \*\*List in \_pkgdown.yml under “Data Objects”\*\*

### Updating Coefficients

1.  Update source file in inst/extdata/
2.  Document changes in comments
3.  Verify downstream functions still work
4.  Update tests if coefficient values changed
5.  Note changes in NEWS.md

## Resources

- **Package website**: <https://eduaguilera.github.io/afsetools/>
- **Tidyverse style guide**: <https://style.tidyverse.org/>
- **R Packages book**: <https://r-pkgs.org/>
- **roxygen2 documentation**: <https://roxygen2.r-lib.org/>
- **pkgdown**: <https://pkgdown.r-lib.org/>
