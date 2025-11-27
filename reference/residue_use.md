# Calculate Residue Use from Crop NPP Data

Converts crop residues into separate items based on use shares, creating
both product and residue entries with appropriate classifications.

## Usage

``` r
residue_use(df)
```

## Arguments

- df:

  Data frame from Crop_AreaNPP workflow with columns: item_cbs,
  Name_biomass, Use_Share, Residue_MgDM, Prod_ygpit_Mg, Area_ygpit_ha,
  and other crop NPP variables

## Value

Data frame with separate rows for products and residues, where: -
Product rows have Product_residue = "Product" - Residue rows have
Product_residue = "Residue" - Residues are converted from DM to FM using
Residue_kgDM_kgFM - Production (Prod_ygpit_Mg) represents used residue
mass - Residues classified using Residues_as_items()

## Details

This function: 1. Preserves original item_cbs as item_cbs_crop 2. Joins
Biomass_coefs for DM/FM conversion factors 3. Calculates used residue
mass in fresh matter (FM) units 4. Creates residue entries with
Product_residue = "Residue" 5. Sets Area_ygpit_ha to NA for residues
(not area-based) 6. Binds product and residue rows together 7. Applies
residue classification via Residues_as_items()

Requires Biomass_coefs object from load_general_data() with: -
Name_biomass, Residue_kgDM_kgFM columns

Used in Spain_Hist (Crop_AreaNPP.R and Scenarios.R) and Global
(Crop_NPP.R) workflows.

## Examples

``` r
if (FALSE) { # \dontrun{
crop_npp_data %>%
  residue_use()
} # }
```
