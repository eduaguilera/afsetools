# Load Color Palettes, Factor Levels, and Categorical Vectors

Creates 300+ vector objects in the calling environment for data
visualization and analysis, including: - Month-related vectors and
dataframes - Color palettes for different categories - Factor level
orderings - Named color vectors for ggplot2 scale functions -
Categorical dataframes for NPP and C input categorization

This function is called internally by \`load_general_data()\` when
\`load_vectors = TRUE\`.

## Usage

``` r
load_vectors(env = parent.frame())
```

## Arguments

- env:

  Environment to load vectors into. Defaults to parent.frame().

## Value

NULL (loads objects into specified environment)

## Details

All vectors are created using assign() to ensure they are available in
the specified environment without needing to source external files.

## Examples

``` r
if (FALSE) { # \dontrun{
# Load vectors into current environment
load_vectors()

# Check what was loaded
ls()
} # }
```
