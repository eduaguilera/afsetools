# Fill gaps with accumulated sum

Fills gaps in a column by accumulating values from another column. The
values are accumulated along the series.

## Usage

``` r
fill_na_with_sum(col1, col2)
```

## Arguments

- col1:

  Column to fill (will be modified)

- col2:

  Column with values to add

## Value

Vector with filled values

## Details

For each NA in col1, fills with col1\[i-1\] + col2\[i\]
