# Extract Data from Land-Use Harmonization 2 (LUH2) Dataset

Extracts carbon stock and area data from the Land Use Harmonization 2
(LUH2) dataset v2h. Processes land use states, carbon densities, and
harvest data from netCDF files and aggregates to specified regions.

## Usage

``` r
extract_luh2(L_files_path, RegionNames, studied_period)
```

## Arguments

- L_files_path:

  Character string with path to LUH2 data files directory

- RegionNames:

  Spatial object (sf) with region polygons for extraction

- studied_period:

  Numeric vector with years to extract (e.g., 1700:2022)

## Value

Data frame Stock_area with columns: - Region_name: Region identifier -
Year: Year - Land_Use: Specific land use category (Primary_forested,
Secondary_nonforested, etc.) - LandUse: Aggregated land use (Forest,
Grassland_schrubland, Pasture, Cropland, Urban) - Disturbance:
Disturbance level (Primary, Secondary, Managed_pasture, Rangeland,
Cropland, Urban) - Area_Mha: Area in million hectares - C_stock_Tg:
Carbon stock in teragrams - C_density_Mg_ha: Carbon density in megagrams
per hectare

## Details

The function processes the following LUH2 layers: - Primary forested and
non-forested lands - Secondary forested and non-forested lands - Managed
pasture and rangeland - Cropland (C3 annual, C3 perennial, C4 annual, C4
perennial, C3 N-fixing) - Urban areas

Carbon stocks are calculated using: - Potential carbon density (ptbio)
for primary lands - Secondary biomass carbon density (secmb) for
secondary lands

## References

Hurtt, G. C. et al. (2011) doi:10.1007/s10584-011-0153-2
https://luh.umd.edu/data.shtml LUH2 v2h data:
http://gsweb1vh2.umd.edu/LUH2/LUH2_v2h/

## Examples

``` r
if (FALSE) { # \dontrun{
library(sf)
library(raster)

# Load region polygons
regions <- st_read("path/to/regions.shp")

# Extract LUH2 data for 1900-2020
stock_area <- extract_luh2(
  L_files_path = "path/to/LUH2/",
  RegionNames = regions,
  studied_period = 1900:2020
)
} # }
```
