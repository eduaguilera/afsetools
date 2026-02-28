# Calculate Non-Symbiotic BNF

Non-symbiotic BNF (NSBNF) is performed by free-living soil bacteria
(e.g. Azotobacter, Clostridium), associative bacteria in the rhizosphere
(e.g. Azospirillum), cyanobacteria in paddy soils, and endophytic
bacteria (e.g. Gluconacetobacter in sugarcane).

The model calculates: \$\$NSBNF = NSBNF\_{base} \times f_N \times f_T
\times f_W \times f\_{SOM} \times f\_{pH} \times Area / 1000\$\$

Where \\NSBNF\_{base}\\ comes from the BNF data table (crop-specific:
rice 33, sugarcane 25 kg N/ha/yr) or a default rate for general
cropland.

## Usage

``` r
calc_nonsymbiotic_bnf(
  x,
  nsbnf_default_kgha = 5,
  k_n_ns_synth = 0.005,
  k_n_ns_org = 0.0025,
  t_opt_ns = 25,
  t_sigma_ns = 10,
  k_som = 2,
  som_ref = 2.5,
  ph_opt = 6.8,
  ph_sigma = 1.5,
  k_clay = 20,
  clay_ref = 25
)
```

## Arguments

- x:

  Data frame. Required columns:

  Area_ygpit_ha

  :   Harvested area in hectares.

  Optional columns (if absent, modifier = 1):

  N_synth_kgha

  :   Synthetic N fertilizer (kg N/ha).

  N_org_kgha

  :   Organic N inputs (kg N/ha).

  TMP

  :   Mean temperature (degrees C).

  WaterInput_mm

  :   Precipitation + irrigation (mm).

  precip_mm

  :   Precipitation (mm).

  irrig_mm

  :   Irrigation (mm).

  PET_mm

  :   Potential evapotranspiration (mm).

  SOM_pct

  :   Soil organic matter content (percent).

  soil_pH

  :   Soil pH.

  kgNha

  :   Crop-specific NSBNF base rate from BNF table. If absent and
      Name_biomass exists, joined automatically.

- nsbnf_default_kgha:

  Numeric. Default NSBNF base rate for crops without a specific value in
  the BNF table (default 5 kg N/ha/yr). Based on Cleveland et al. (1999)
  for temperate agricultural soils.

- k_n_ns_synth:

  Numeric. Rate constant for N inhibition of NSBNF by synthetic N
  (default 0.005). Stronger than symbiotic inhibition since free-living
  fixers avoid the energy cost of fixation when mineral N is available.

- k_n_ns_org:

  Numeric. Rate constant for N inhibition of NSBNF by organic N (default
  0.0025). Weaker than synthetic because organic N mineralizes slowly.

- t_opt_ns:

  Numeric. Optimal temperature for non-symbiotic fixation (default 25).

- t_sigma_ns:

  Numeric. Temperature Gaussian width (default 10). Broader than
  symbiotic (8) because diverse microbial communities provide thermal
  buffering.

- k_som:

  Numeric. SOM half-saturation constant (default 2.0 percent).

- som_ref:

  Numeric. Reference SOM for normalization (default 2.5 percent).

- ph_opt:

  Numeric. Optimal pH (default 6.8).

- ph_sigma:

  Numeric. pH Gaussian width (default 1.5).

- k_clay:

  Numeric. Half-saturation constant for clay effect on NSBNF (default 20
  percent clay).

- clay_ref:

  Numeric. Reference clay content for normalization (default 25 percent,
  typical loam).

## Value

Data frame with added columns:

- NSBNF_base_kgha:

  Base rate before adjustment (kg N/ha).

- f_N_ns:

  N inhibition factor.

- f_temp_ns:

  Temperature factor.

- f_water_ns:

  Water factor.

- f_SOM_ns:

  SOM factor (normalized at som_ref).

- f_pH_ns:

  pH factor.

- f_clay_ns:

  Clay texture factor (normalized at clay_ref).

- f_env_ns:

  Combined environmental factor.

- NSBNF:

  Non-symbiotic BNF in Mg N.

## Details

Estimates free-living and associative BNF in agricultural soils using
crop-specific base rates and environmental modifiers for temperature,
water availability, N inputs, soil organic matter, and soil pH.

\*\*Base rates\*\* from literature:

- Rice paddies: 33 kg N/ha/yr (Ladha et al. 2016) - cyanobacteria and
  heterotrophic anaerobic fixation

- Sugarcane: 25 kg N/ha/yr (Urquiaga et al. 2012) - endophytic fixation
  (Gluconacetobacter diazotrophicus)

- General cropland: 5 kg N/ha/yr default (Cleveland et al. 1999)

\*\*N inhibition\*\* (Dynarski & Houlton 2018): At 100 kg N/ha, NSBNF
reduces to ~61 percent of base. Free-living fixers avoid the energy cost
of fixation when mineral N is available.

\*\*SOM effect\*\* (Reed et al. 2011; Dynarski & Houlton 2018):
Heterotrophic fixers require carbon as energy source. Michaelis- Menten
kinetics normalized at typical agricultural SOM (2.5 percent). High-SOM
soils show enhanced NSBNF.

\*\*pH effect\*\* (Belnap 2002): Nitrogenase activity and microbial
communities are sensitive to soil pH. Gaussian around optimal 6.8 with
moderate decline in very acidic (pH \< 5) or alkaline (pH \> 8) soils.

## References

Cleveland CC et al. (1999) Global Biogeochemical Cycles 13:623-645.

Dynarski KA, Houlton BZ (2018) New Phytologist 217:68-85.

Ladha JK et al. (2016) Scientific Reports 6:19355.

Reed SC et al. (2011) Annual Review of Ecology, Evolution, and
Systematics 42:489-512.

Urquiaga S et al. (2012) Plant and Soil 356:5-21.

## Examples

``` r
if (FALSE) { # \dontrun{
load_general_data()

# Basic: only area required, uses defaults
df |> calc_nonsymbiotic_bnf()

# With environmental data:
df |>
  dplyr::mutate(
    N_synth_kgha = 120, TMP = 22,
    precip_mm = 700, PET_mm = 850,
    SOM_pct = 3.2, soil_pH = 6.4
  ) |>
  calc_nonsymbiotic_bnf()
} # }
```
