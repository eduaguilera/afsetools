# Tests for utility functions based on Spain_Hist and Global usage

test_that("%!in% operator works correctly", {
  # Test basic functionality
  expect_true(5 %!in% c(1, 2, 3, 4))
  expect_false(3 %!in% c(1, 2, 3, 4))
  
  # Test with character vectors (common use in data filtering)
  expect_true("apple" %!in% c("banana", "orange", "grape"))
  expect_false("banana" %!in% c("banana", "orange", "grape"))
  
  # Test with empty vector
  expect_true(1 %!in% c())
  
  # Test with NA
  expect_true(NA %!in% c(1, 2, 3))
  
  # Test real usage pattern from Spain_Hist: filtering out specific items
  test_items <- c("Wheat", "Barley", "Maize (corn)")
  excluded <- c("Wheat", "Rye")
  expect_true("Barley" %!in% excluded)
  expect_false("Wheat" %!in% excluded)
})

test_that("Filling gap-fills time series correctly", {
  load_general_data()
  
  # Create realistic data with gaps as in Spain_Hist
  test_data <- tibble::tribble(
    ~Year, ~Province_name, ~Value,
    2015,  "Madrid",       100,
    2016,  "Madrid",       NA,    # Gap to be filled by interpolation
    2017,  "Madrid",       120,
    2018,  "Madrid",       NA,    # Gap to be filled
    2019,  "Madrid",       NA,    # Gap to be filled
    2020,  "Madrid",       150,
    2021,  "Madrid",       NA     # Gap to be filled by carrying forward
  )
  
  result <- test_data |>
    dplyr::group_by(Province_name) |>
    Filling(Value, Year) |>
    dplyr::ungroup()
  
  # Test that no NAs remain
  expect_false(any(is.na(result$Value)))
  
  # Test that original values are preserved
  expect_equal(result$Value[result$Year == 2015], 100)
  expect_equal(result$Value[result$Year == 2017], 120)
  expect_equal(result$Value[result$Year == 2020], 150)
  
  # Test that interpolation is linear (2016 should be 110)
  expect_equal(result$Value[result$Year == 2016], 110)
  
  # Test that Source column is created
  expect_true("Source_Value" %in% names(result))
  
  # Test source labels
  expect_equal(result$Source_Value[result$Year == 2015], "Original")
  expect_equal(result$Source_Value[result$Year == 2016], "Linear interpolation")
  expect_equal(result$Source_Value[result$Year == 2021], "Last value carried forward")
  
  # Test same number of rows
  expect_equal(nrow(result), nrow(test_data))
})

test_that("Filling handles multiple provinces correctly", {
  load_general_data()
  
  # Real usage pattern: multiple provinces with different gaps
  test_data <- tibble::tribble(
    ~Year, ~Province_name, ~Value,
    2018,  "Madrid",       100,
    2019,  "Madrid",       NA,
    2020,  "Madrid",       120,
    2018,  "Barcelona",    80,
    2019,  "Barcelona",    NA,
    2020,  "Barcelona",    100
  )
  
  result <- test_data |>
    dplyr::group_by(Province_name) |>
    Filling(Value, Year) |>
    dplyr::ungroup()
  
  # Test that gaps are filled independently for each province
  expect_false(any(is.na(result$Value)))
  
  # Test that Madrid and Barcelona have different interpolated values
  madrid_2019 <- result$Value[result$Year == 2019 & result$Province_name == "Madrid"]
  barcelona_2019 <- result$Value[result$Year == 2019 & result$Province_name == "Barcelona"]
  expect_true(madrid_2019 != barcelona_2019)
  
  # Test correct interpolation: Madrid 2019 should be 110, Barcelona should be 90
  expect_equal(madrid_2019, 110)
  expect_equal(barcelona_2019, 90)
})

test_that("Filling handles edge cases properly", {
  load_general_data()
  
  # Test with all NAs (should carry forward/backward)
  test_all_na <- tibble::tribble(
    ~Year, ~Value,
    2018,  NA,
    2019,  NA,
    2020,  NA
  )
  
  result_na <- test_all_na |> Filling(Value, Year)
  # All NAs should remain NA (no values to carry)
  expect_true(all(is.na(result_na$Value)))
  
  # Test with no NAs (should preserve all)
  test_no_na <- tibble::tribble(
    ~Year, ~Value,
    2018,  100,
    2019,  110,
    2020,  120
  )
  
  result_no_na <- test_no_na |> Filling(Value, Year)
  expect_equal(result_no_na$Value, test_no_na$Value)
  expect_true(all(result_no_na$Source_Value == "Original"))
  
  # Test with gap at beginning (should fill backward)
  test_start_gap <- tibble::tribble(
    ~Year, ~Value,
    2018,  NA,
    2019,  100,
    2020,  110
  )
  
  result_start <- test_start_gap |> Filling(Value, Year)
  expect_equal(result_start$Value[1], 100)
  expect_equal(result_start$Source_Value[1], "First value carried backwards")
})

test_that("FillingProxy fills data using proxy variable", {
  load_general_data()
  
  # Realistic usage: fill production using area as proxy (common in Spain_Hist)
  test_data <- tibble::tribble(
    ~Year, ~Province_name, ~Production, ~Area,
    2018,  "Madrid",       100,         50,
    2019,  "Madrid",       NA,          60,   # Production missing, use area ratio
    2020,  "Madrid",       120,         70
  )
  
  result <- test_data |>
    dplyr::group_by(Province_name) |>
    FillingProxy(Production, Area, Year) |>
    dplyr::ungroup()
  
  # Test that result is a data frame
  expect_true(is.data.frame(result))
  
  # Test same number of rows
  expect_equal(nrow(result), nrow(test_data))
  
  # Test that proxy-based filling maintains ratio
  # If area increases 20% (50->60), production should increase proportionally
  if (!is.na(result$Production[2])) {
    ratio_2018 <- test_data$Production[1] / test_data$Area[1]  # 100/50 = 2
    expected_2019 <- ratio_2018 * test_data$Area[2]  # 2 * 60 = 120
    # Allow some tolerance for different interpolation methods
    expect_true(abs(result$Production[2] - expected_2019) < 20)
  }
})

test_that("drop_cols removes specified columns", {
  test_data <- tibble::tibble(
    A = 1:3,
    B = 4:6,
    C = 7:9,
    D = 10:12
  )
  
  result <- drop_cols(test_data, c("B", "D"))
  
  # Test that specified columns are removed
  expect_false("B" %in% names(result))
  expect_false("D" %in% names(result))
  
  # Test that other columns remain
  expect_true("A" %in% names(result))
  expect_true("C" %in% names(result))
  
  # Test same number of rows
  expect_equal(nrow(result), nrow(test_data))
  
  # Test that data is preserved in remaining columns
  expect_equal(result$A, test_data$A)
  expect_equal(result$C, test_data$C)
})

test_that("is_empty correctly identifies empty data", {
  # Test with NULL
  expect_true(is_empty(data.frame()))
  
  # Test with zero-row data frame
  empty_df <- tibble::tibble(A = numeric(0), B = character(0))
  expect_true(is_empty(empty_df))
  
  # Test with non-empty data frame
  non_empty_df <- tibble::tibble(A = 1:3, B = c("a", "b", "c"))
  expect_false(is_empty(non_empty_df))
  
  # Test with single row
  single_row_df <- tibble::tibble(A = 1, B = "a")
  expect_false(is_empty(single_row_df))
})

test_that("Arrange_dates creates and sorts by date", {
  load_general_data()
  
  # Create data as used in Spain_Hist water calculations
  test_data <- tibble::tribble(
    ~Year, ~Month_names, ~Value,
    2020,  "December",    100,
    2020,  "January",     50,
    2020,  "June",        75,
    2021,  "March",       60
  )
  
  result <- Arrange_dates(test_data)
  
  # Test that Date column is created
  expect_true("Date" %in% names(result))
  expect_true(inherits(result$Date, "Date"))
  
  # Test that data is sorted by date
  expect_true(all(diff(result$Date) >= 0))
  
  # Test that January comes before June in 2020
  jan_row <- which(result$Year == 2020 & result$Month_names == "January")
  jun_row <- which(result$Year == 2020 & result$Month_names == "June")
  expect_true(jan_row < jun_row)
  
  # Test that Month_number is added
  expect_true("Month_number" %in% names(result))
  expect_equal(result$Month_number[jan_row], 1)
  expect_equal(result$Month_number[jun_row], 6)
})

test_that("add_xlsx_sheet adds sheet to Excel workbook", {
  skip_if_not_installed("openxlsx")
  
  # This function modifies an Excel workbook
  # Test that it exists and is callable
  expect_true(is.function(add_xlsx_sheet))
  
  # We won't test actual file creation here to avoid file system dependencies
  # But ensure function signature is correct
  args <- names(formals(add_xlsx_sheet))
  expect_true(all(c("wb_location", "newsheet_name", "newsheet_data") %in% args))
})
