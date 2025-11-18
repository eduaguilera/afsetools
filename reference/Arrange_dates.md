# Arrange data by dates

Creates dates based on years and months, and arranges by date. Requires
Month_numbers object with Month_names and Month_number columns.

## Usage

``` r
Arrange_dates(x)
```

## Arguments

- x:

  Data frame with Year and Month_names columns

## Value

Data frame arranged by date with additional date columns

## Details

Remember to group data when needed before using this function. Requires
Month_numbers and Month_order objects from load_general_data().
