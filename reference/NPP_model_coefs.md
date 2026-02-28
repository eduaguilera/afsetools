# NPP Model Coefficients

Numeric coefficients for potential NPP models used by
[`calculate_potential_npp()`](https://eduaguilera.github.io/afsetools/reference/calculate_potential_npp.md).
Contains all parameters for the Miami, NCEAS (tree and non-tree), and
Rosenzweig models. Coefficients are verified against published sources.

## Format

A data frame with columns:

- Model:

  Model name (Miami, NCEAS_tree_TNPP, NCEAS_tree_ANPP,
  NCEAS_nontree_TNPP, NCEAS_nontree_ANPP, Rosenzweig).

- Component:

  Formula component (F_MAT, F_MAP, Saturating, Log_linear).

- Parameter:

  Parameter name (Max_gCm2yr, Midpoint, Rate, Coefficient, Exponent,
  Exp_divisor, Slope, Intercept).

- Value:

  Numeric parameter value.

- Unit:

  Physical unit of the parameter.

- Formula_role:

  Role of the parameter in the model formula.

- Climate_var:

  Climate variable the parameter applies to (Temperature, Precipitation,
  AET).

- Source:

  Literature source reference.

## Source

Lieth (1975) Modeling Primary Productivity of the World. Del Grosso et
al. (2008) Ecology 89:2117-2126. Rosenzweig (1968) Am Nat 102:67-74.
