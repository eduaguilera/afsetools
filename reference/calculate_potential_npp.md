# Calculate Potential Net Primary Production (NPP)

Calculates potential NPP using various models including Miami, NCEAS,
and Rosenzweig. All model coefficients are read from \`NPP_model_coefs\`
(loaded by \`load_general_data()\`) rather than hardcoded in the
function.

## Usage

``` r
calculate_potential_npp(Dataset)
```

## Arguments

- Dataset:

  A data frame containing climate data with columns:

  TMP

  :   Mean annual temperature (degrees C).

  WaterInput_mm

  :   Total water input: precipitation + irrigation (mm).

  AET_mm

  :   Actual evapotranspiration (mm).

## Value

A data frame with calculated NPP values from different models in Mg
DM/ha.

## Details

Requires the following objects from \`load_general_data()\`: -
\`NPP_model_coefs\` — model parameters (Miami, NCEAS, Rosenzweig) -
\`Residue_kgC_kgDM_Wo\` — C content of woody residues for DM
conversion - \`Residue_kgC_kgDM_W\` — C content of non-tree residues for
DM conversion

Models implemented: - \*\*Miami\*\* (Lieth 1975): min of temperature and
precipitation limits - \*\*NCEAS tree\*\* (Del Grosso et al. 2008, Table
1): min(F_MAP, F_MAT) for both TNPP and ANPP - \*\*NCEAS non-tree\*\*
(Del Grosso et al. 2008, Table 2): precipitation-only saturating
functions for TNPP and ANPP - \*\*Rosenzweig\*\* (1968): log-linear
model based on AET

## References

Lieth, H. (1975) Modeling the Primary Productivity of the World. In:
Primary Productivity of the Biosphere, Springer.

Del Grosso, S. et al. (2008) Global potential net primary production
predicted from vegetation class, precipitation, and temperature. Ecology
89:2117-2126.

Rosenzweig, M.L. (1968) Net Primary Productivity of Terrestrial
Communities: Prediction from Climatological Data. Am Nat 102:67-74.

## Examples

``` r
if (FALSE) { # \dontrun{
load_general_data()
climate_data <- data.frame(TMP = 15, WaterInput_mm = 800, AET_mm = 700)
npp_results <- calculate_potential_npp(climate_data)
} # }
```
