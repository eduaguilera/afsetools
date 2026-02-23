# Impact Tracing Functions

Functions to trace environmental impacts through supply chains from
primary production to processed products, accounting for trade and
economic allocation. Prepare Primary Production Database

## Usage

``` r
Prepare_prim(Prim_all)
```

## Arguments

- Prim_all:

  Primary production data frame with columns: unit, item_prod,
  item_code_prod, Value

## Value

A data frame with item_code_impact added for joining with impact data

## Details

Prepares the primary production database by adding impact codes and
filtering multi-products (cotton, oil palm, etc.).

## Examples

``` r
if (FALSE) { # \dontrun{
primary_data <- Prepare_prim(Primary_all)
} # }
```
