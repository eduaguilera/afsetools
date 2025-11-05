# GitHub Copilot Instructions for afsetools Package

## Package Purpose

`afsetools` (Agro-Food System and Environment Tools) is an R package providing core functions and data for calculating environmental footprints in agro-food systems. It contains standardized coefficients, classifications, and functions for tracing environmental impacts through global supply chains.

## Core Principles

- **Explicit package names**: Always prefix functions (e.g., `dplyr::mutate()`, `tidyr::pivot_longer()`)
- **Native pipe**: Use `|>` instead of `%>%` (requires R >= 4.1.0)
- **Type-safe conditionals**: Use `dplyr::if_else()` or `dplyr::case_when()` instead of base `ifelse()`
- **roxygen2 documentation**: All exported functions must have complete roxygen2 documentation
- **Package structure**: Follow standard R package conventions (R/, man/, tests/, inst/extdata/)
- **Keep data loading flexible**: `load_general_data()` should work both as installed package and during development

## Documentation Standards

### Function Documentation (roxygen2)

All exported functions MUST include these tags:
- `@description` - Brief description of what the function does
- `@param` (for each parameter) - Parameter name and description
- `@return` - What the function returns
- `@export` - Mark for export in NAMESPACE
- `@examples` - Usage examples (wrap in `\dontrun{}` if requires external data)

**Format requirements**:
- One space after `#'`
- Maximum line width: 80 characters
- Multi-line descriptions indented two spaces
- End all sentences with period
- Use proper markdown formatting

**Example**:
```r
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

Data objects loaded by `load_general_data()` should be documented in `R/data.R`:
- Use `@format` to describe structure with `\describe{}`
- Include `@source` for data origin
- Add `@docType data` and `@keywords datasets`
- Keep descriptions concise but informative

## Code Style

### Naming Conventions
- **Functions**: `snake_case` or `PascalCase` (be consistent within file)
- **Variables/Columns**: `snake_case` preferred
- **Data objects**: Match loaded object names (varies by source)
- **File names**: lowercase with underscores (e.g., `load_data.R`, `npp_functions.R`)

### Style Guide
- Follow [Tidyverse style guide](https://style.tidyverse.org/)
- Maximum line width: 80 characters
- Use `.by` for inline grouping: `mutate(.by = Year, total = sum(value))`
- Avoid `group_by()` unless required by specific functions
- Always `ungroup()` after `group_by()` operations

## Package Data Management

### Loading Data Objects
- **Input files location**: `inst/extdata/` (Excel files with coefficients)
- **Loading function**: `load_general_data()` creates 73+ objects in parent environment
- **Access pattern**: Users call `load_general_data()` after `library(afsetools)`
- **No data in package namespace**: Objects are loaded to user environment, not attached to package

### Data File Requirements
- Store coefficient tables in Excel format in `inst/extdata/`
- Use consistent sheet names and column headers
- Document all data sources and references
- Include metadata sheets where appropriate

## Function Organization

### Function Categories
1. **Data Loading** (`load_data.R`): Load coefficients and classifications
2. **NPP Calculation** (`npp_functions.R`): Net Primary Productivity calculations
3. **Impact Tracing** (`impact_functions.R`): Supply chain impact allocation
4. **Analysis** (`analysis_functions.R`): Specialized calculations (N fixation, GHG, etc.)
5. **Utilities** (`utility_functions.R`): Helper functions, gap-filling
6. **Visualization** (`plotting.R`): Themes and plotting functions
7. **Workflows** (`calculate_footprints.R`): Complete calculation pipelines

### Function Design
- **Single responsibility**: Each function should do one thing well
- **Clear inputs/outputs**: Document expected data structure
- **Vectorized operations**: Use tidyverse/data.table for efficiency
- **Fail gracefully**: Provide informative error messages
- **Return tidy data**: Prefer long format over wide format

## Testing

**Location**: `tests/testthat/` - name as `test-[script-name].R`

**Testing approach**:
- One test file per R script (e.g., `test-load_data.R` for `load_data.R`)
- Use meaningful test names that describe what is being tested
- Test functionality and edge cases
- Use `testthat` framework

**Useful test patterns**:
```r
# Use tibble::tribble() for sample data
test_data <- tibble::tribble(
  ~Year, ~Value,
  2020, 100,
  2021, 150
)

# Test column existence and properties
expect_true("Year" %in% names(result))
expect_type(result$Value, "double")

# Test function behavior
expect_error(my_function(invalid_input))
expect_equal(calculate_sum(c(1, 2, 3)), 6)
```

## Dependencies

### Package Dependencies
- **Core tidyverse**: dplyr, tidyr, ggplot2
- **Data I/O**: readxl, openxlsx
- **Other**: Add to DESCRIPTION file under `Imports` or `Suggests`

### Dependency Management
- Minimize dependencies where possible
- Use package imports explicitly in functions
- Document why each dependency is needed

## Package Development Workflow

### Making Changes
1. **Modify R functions**: Update code in `R/` directory
2. **Update documentation**: Modify roxygen2 comments
3. **Run document**: `devtools::document()` to update man/ files
4. **Run tests**: `devtools::test()` to verify nothing breaks
5. **Check package**: `devtools::check()` for R CMD check
6. **Build site**: GitHub Actions will rebuild pkgdown site automatically

### Before Committing
- Run `devtools::check()` and fix all errors/warnings
- Ensure all tests pass
- Update NEWS.md if adding features or fixing bugs
- Document any new data objects in DATA_REFERENCE.md

## Coefficient Management

**CRITICAL: Never hardcode coefficients** - always load from data files or calculate from source data:

**Sources (in order of preference)**:
1. **Load from inst/extdata files**: Biomass_coefs.xlsx, GWP.xlsx, BNF.xlsx, etc.
2. **Calculate from source data**: Document methodology in comments
3. **Scientific literature**: Always cite sources in comments

**Forbidden patterns**:
- ❌ Hardcoded numeric values without source
- ❌ Made-up coefficients in code
- ❌ Undocumented constants

**Correct patterns**:
- ✅ `left_join(Biomass_coefs, by = "Name_biomass")`
- ✅ Load from Excel with documented source
- ✅ Calculate with referenced methodology

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
- **Site URL**: https://eduaguilera.github.io/afsetools/
- **Reference page**: https://eduaguilera.github.io/afsetools/reference/index.html
- **Update _pkgdown.yml**: Organize functions by category
- **No manual building needed**: Workflow handles everything

## Common Tasks

### Adding a New Function
1. Write function in appropriate R/ file
2. Add complete roxygen2 documentation
3. Export if needed (`@export`)
4. Add to appropriate category in _pkgdown.yml
5. Write tests in tests/testthat/
6. Run `devtools::document()` and `devtools::check()`

### Adding New Data Objects
1. Add data file to inst/extdata/
2. Update `load_general_data()` to load the new data
3. Document in R/data.R with roxygen2
4. Add description to DATA_REFERENCE.md
5. List in _pkgdown.yml under "Data Objects"

### Updating Coefficients
1. Update source file in inst/extdata/
2. Document changes in comments
3. Verify downstream functions still work
4. Update tests if coefficient values changed
5. Note changes in NEWS.md

## Publication and Citation

When preparing for publication:
- Ensure all data sources are properly cited
- Update DESCRIPTION with accurate version number
- Create DOI via Zenodo or similar
- Update CITATION file with publication details
- Add publication info to README.md

## Resources

- **Tidyverse style guide**: https://style.tidyverse.org/
- **R Packages book**: https://r-pkgs.org/
- **roxygen2 documentation**: https://roxygen2.r-lib.org/
- **pkgdown**: https://pkgdown.r-lib.org/
- **Package website**: https://eduaguilera.github.io/afsetools/