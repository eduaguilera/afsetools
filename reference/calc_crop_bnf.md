# Calculate Crop Legume Symbiotic BNF

Two estimation methods are calculated simultaneously:

1\. \*\*NPP method\*\* (\`CropBNF\`): Based on total crop nitrogen from
NPP estimation: \$\$CropBNF = Crop\\NPP\\MgN \times Ndfa\_{adj} \times
Leg\\share\$\$

2\. \*\*Anglade method\*\* (\`CropBNF2\`): Based on product nitrogen,
below-ground N (BGN), and nitrogen harvest index (NHI) following Anglade
et al. (2015) and Lassaletta et al. (2014): \$\$CropBNF2 = Prod\\MgN
\times Leg\\share \times Ndfa\_{adj} \times BGN / NHI\$\$

When environmental columns are available, the reference Ndfa is
adjusted: \$\$Ndfa\_{adj} = Ndfa\_{ref} \times f_N \times f_T \times
f_W\$\$

## Usage

``` r
calc_crop_bnf(x, k_n_synth = 0.0035, k_n_org = 0.0018, t_opt = 25, t_sigma = 8)
```

## Arguments

- x:

  Data frame with crop NPP data. Required columns:

  Name_biomass

  :   Crop name matching Names_BNF classification.

  Crop_NPP_MgN

  :   Crop NPP in Mg N.

  Prod_MgN

  :   Product nitrogen in Mg.

  Optional environmental columns (if absent, no adjustment applied):

  N_synth_kgha

  :   Synthetic N fertilizer (kg N/ha).

  N_org_kgha

  :   Organic N inputs (kg N/ha).

  TMP

  :   Mean temperature (degrees C).

  WaterInput_mm

  :   Precipitation + irrigation (mm). Computed from precip_mm +
      irrig_mm if absent.

  precip_mm

  :   Precipitation (mm).

  irrig_mm

  :   Irrigation water applied (mm).

  PET_mm

  :   Potential evapotranspiration (mm).

- k_n_synth:

  Numeric. Rate constant for N inhibition by synthetic N (default
  0.0035). At 200 kg synthetic N/ha, Ndfa reduces to ~50 percent of
  reference.

- k_n_org:

  Numeric. Rate constant for N inhibition by organic N (default 0.0018).
  Weaker than synthetic because organic N mineralizes slowly (Peoples et
  al. 2009).

- t_opt:

  Numeric. Optimal temperature for nitrogenase (default 25 degrees C).

- t_sigma:

  Numeric. Width of temperature Gaussian (default 8).

## Value

Data frame with added columns:

- Ndfa, NHI, BGN, Leguminous_share:

  BNF parameters.

- N_total_kgha:

  Total N input (synthetic + organic).

- f_N_symb:

  N inhibition factor (0-1).

- f_temp_symb:

  Temperature factor (0-1).

- f_water_symb:

  Water availability factor (0-1).

- f_env_symb:

  Combined environmental factor.

- Ndfa_adj:

  Adjusted Ndfa after environmental correction.

- CropBNF:

  Crop BNF via NPP method (Mg N).

- CropBNF2:

  Crop BNF via Anglade method (Mg N).

- Alpha1, Alpha2:

  BNF per unit product N.

## Details

Estimates symbiotic biological nitrogen fixation by crop legumes using
two complementary methods, with optional environmental adjustments for
nitrogen fertilization, temperature, and water availability.

\*\*N fertilizer inhibition\*\*: Mineral N inhibits nitrogenase and
nodule formation (Salvagiotti et al. 2008; Streeter & Wong 1988).
Synthetic N inhibits more strongly than organic N: \$\$f_N =
\exp(-k\_{synth} \times N\_{synth} - k\_{org} \times N\_{org})\$\$

\*\*Temperature\*\*: Gaussian centred at T_opt, reflecting thermal
sensitivity of nitrogenase (Hungria & Vargas 2000): \$\$f_T =
\exp\left(-\frac{(T - T\_{opt})^2}{2 \sigma^2}\right)\$\$

\*\*Water stress\*\*: BNF declines under drought via carbon and oxygen
effects on nodules (Serraj et al. 1999). Modelled via aridity index:
\$\$f_W = \min(1, \text{AI} / 0.65)\$\$

Requires \`Names_BNF\` and \`BNF\` objects from \`load_general_data()\`.

## References

Anglade J et al. (2015) Nutrient Cycling in Agroecosystems 103:37-56.

Hungria M, Vargas MAT (2000) Field Crops Research 65:151-164.

Lassaletta L et al. (2014) Biogeosciences 11:2889-2907.

Salvagiotti F et al. (2008) Field Crops Research 108:1-13.

Serraj R et al. (1999) Plant Physiology 120:577-586.

## Examples

``` r
if (FALSE) { # \dontrun{
load_general_data()
# Basic usage (no environmental adjustment):
crop_data |> calc_crop_bnf()

# With environmental data:
crop_data |>
  dplyr::mutate(
    N_synth_kgha = 80, TMP = 18,
    precip_mm = 600, PET_mm = 900
  ) |>
  calc_crop_bnf()
} # }
```
