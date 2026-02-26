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

test_that("drop_cols silently ignores non-existent columns", {
  test_data <- tibble::tibble(
    A = 1:3,
    B = 4:6,
    C = 7:9
  )
  
  # Test with non-existent column mixed with existing
 expect_no_error(result <- drop_cols(test_data, "A", "nonexistent"))
  expect_false("A" %in% names(result))
  expect_true("B" %in% names(result))
  expect_true("C" %in% names(result))
  
  # Test with character vector containing non-existent columns
  expect_no_error(result2 <- drop_cols(test_data, c("A", "B", "nonexistent", "also_missing")))
  expect_equal(names(result2), "C")
  
  # Test with all non-existent columns (returns original df)
  expect_no_error(result3 <- drop_cols(test_data, "x", "y", "z"))
  expect_equal(names(result3), names(test_data))
  expect_equal(nrow(result3), nrow(test_data))
})

test_that("drop_cols works with multiple argument styles", {
  test_data <- tibble::tibble(
    A = 1:3,
    B = 4:6,
    C = 7:9
  )
  
  # Test with separate character arguments
  result1 <- drop_cols(test_data, "A", "B")
  expect_equal(names(result1), "C")
  
  # Test with character vector
  result2 <- drop_cols(test_data, c("A", "B"))
  expect_equal(names(result2), "C")
  
  # Test with single column
  result3 <- drop_cols(test_data, "A")
  expect_equal(names(result3), c("B", "C"))
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
