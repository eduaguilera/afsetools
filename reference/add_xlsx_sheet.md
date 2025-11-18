# Add sheet to existing Excel workbook

Adds a new sheet to an existing Excel workbook. The sheet must not exist
previously.

## Usage

``` r
add_xlsx_sheet(wb_location, newsheet_name, newsheet_data)
```

## Arguments

- wb_location:

  Character string with path to Excel workbook

- newsheet_name:

  Character string with name for new sheet

- newsheet_data:

  Data frame to write to new sheet

## Value

NULL (modifies workbook file in place)
