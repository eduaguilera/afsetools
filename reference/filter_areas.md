# Filter and Aggregate Areas by Polity

Harmonizes country-level data by joining with regions_full polity codes
and aggregating values by polity groupings. Used internally to map FAO
area codes to standardized polity areas.

## Usage

``` r
filter_areas(df, ...)
```

## Arguments

- df:

  Data frame with an area_code column and columns Year, unit, Element,
  Value

- ...:

  Additional columns to include in grouping (bare column names)

## Value

Data frame aggregated by polity with renamed area/area_code columns

## Details

Requires regions_full object from load_general_data() with columns:
code, polity_code, polity_name.
