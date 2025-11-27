# Classify Residues as CBS Items

Reclassifies biomass residues and grazed weeds into standardized
Commodity Balance Sheet (CBS) items based on product type and land use.

## Usage

``` r
residues_as_items(df)
```

## Arguments

- df:

  Data frame containing columns: Product_residue, Cat_1, LandUse,
  Herb_Woody, item_cbs

## Value

Data frame with item_cbs column updated to classify residues as: -
"Straw" for cereal and pulse residues - "Firewood" for woody crops or
non-cropland herbaceous residues - "Other crop residues" for other
cropland herbaceous residues - "Grassland" for grazed weeds

## Details

Classification logic: - Residues from Cereals/Pulses → "Straw" -
Residues from woody crops or non-cropland → "Firewood" - Other cropland
residues → "Other crop residues" - Grazed weeds → "Grassland" - Products
retain their original item_cbs value

## Examples

``` r
if (FALSE) { # \dontrun{
biomass_data |>
  residues_as_items()
} # }
```
