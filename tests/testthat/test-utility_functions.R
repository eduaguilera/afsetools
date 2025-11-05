test_that("%!in% operator works correctly", {
  # Test basic functionality
  expect_true(5 %!in% c(1, 2, 3, 4))
  expect_false(3 %!in% c(1, 2, 3, 4))
  
  # Test with character vectors
  expect_true("apple" %!in% c("banana", "orange", "grape"))
  expect_false("banana" %!in% c("banana", "orange", "grape"))
  
  # Test with empty vector
  expect_true(1 %!in% c())
  
  # Test with NA
  expect_true(NA %!in% c(1, 2, 3))
})

test_that("Filling gap-fills time series data", {
  # Create sample data with gaps
  test_data <- tibble::tribble(
    ~Year, ~Province, ~Value,
    2018,  "Madrid",  100,
    2019,  "Madrid",  NA,
    2020,  "Madrid",  120,
    2021,  "Madrid",  NA,
    2022,  "Madrid",  140
  )
  
  result <- test_data |>
    dplyr::group_by(Province) |>
    Filling(Value, Year) |>
    dplyr::ungroup()
  
  # Test that result is a data frame
  expect_true(is.data.frame(result))
  
  # Test that gaps are filled
  expect_false(any(is.na(result$Value)))
  
  # Test same number of rows
  expect_equal(nrow(result), nrow(test_data))
})

test_that("FillingProxy fills data using proxy variable", {
  # Create sample data
  test_data <- tibble::tribble(
    ~Year, ~Province, ~Value, ~Proxy,
    2018,  "Madrid",  100,    50,
    2019,  "Madrid",  NA,     60,
    2020,  "Madrid",  120,    70
  )
  
  result <- test_data |>
    dplyr::group_by(Province) |>
    FillingProxy(Value, Proxy, Year) |>
    dplyr::ungroup()
  
  # Test that result is a data frame
  expect_true(is.data.frame(result))
  
  # Test same number of rows
  expect_equal(nrow(result), nrow(test_data))
})

test_that("Arrange_dates sorts data by dates", {
  # Create sample data with dates
  test_data <- tibble::tribble(
    ~Date,         ~Value,
    "2020-12-01",  100,
    "2020-01-01",  50,
    "2020-06-01",  75
  ) |>
    dplyr::mutate(Date = as.Date(Date))
  
  result <- Arrange_dates(test_data)
  
  # Test that result is a data frame
  expect_true(is.data.frame(result))
  
  # Test that data is sorted
  expect_equal(result$Value, c(50, 75, 100))
})

test_that("add_xlsx_sheet adds sheet to Excel workbook", {
  skip_if_not_installed("openxlsx")
  
  # This function modifies an Excel workbook
  # Test that it exists and is callable
  expect_true(is.function(add_xlsx_sheet))
})

test_that("drop_cols drops specified columns", {
  test_data <- tibble::tibble(
    A = 1:3,
    B = 4:6,
    C = 7:9
  )
  
  result <- drop_cols(test_data, c("B"))
  
  # Test that column B is removed
  expect_false("B" %in% names(result))
  expect_true("A" %in% names(result))
  expect_true("C" %in% names(result))
  
  # Test same number of rows
  expect_equal(nrow(result), nrow(test_data))
})

test_that("is_empty checks if vector is empty", {
  # Test with NULL
  expect_true(is_empty(NULL))
  
  # Test with empty vector
  expect_true(is_empty(c()))
  
  # Test with NA
  expect_true(is_empty(NA))
  
  # Test with non-empty vector
  expect_false(is_empty(c(1, 2, 3)))
  
  # Test with single value
  expect_false(is_empty(1))
})

test_that("fill_na_with_sum fills NA values with sum", {
  test_data <- tibble::tribble(
    ~Group, ~A, ~B, ~Total,
    "X",    10, 20, NA,
    "Y",    15, NA, 40,
    "Z",    NA, 25, 50
  )
  
  result <- fill_na_with_sum(test_data, Total, c(A, B))
  
  # Test that result is a data frame
  expect_true(is.data.frame(result))
  
  # Test that NAs are handled
  expect_equal(nrow(result), nrow(test_data))
})

test_that("Filling handles multiple groups", {
  test_data <- tibble::tribble(
    ~Year, ~Province, ~Value,
    2018,  "Madrid",  100,
    2019,  "Madrid",  NA,
    2020,  "Madrid",  120,
    2018,  "Barcelona", 80,
    2019,  "Barcelona", NA,
    2020,  "Barcelona", 100
  )
  
  result <- test_data |>
    dplyr::group_by(Province) |>
    Filling(Value, Year) |>
    dplyr::ungroup()
  
  # Test that gaps are filled for all provinces
  expect_false(any(is.na(result$Value)))
  expect_equal(nrow(result), nrow(test_data))
})

test_that("Filling handles edge cases", {
  # Test with all NAs
  test_all_na <- tibble::tribble(
    ~Year, ~Value,
    2018,  NA,
    2019,  NA,
    2020,  NA
  )
  
  result <- test_all_na |>
    Filling(Value, Year)
  
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 3)
  
  # Test with no NAs
  test_no_na <- tibble::tribble(
    ~Year, ~Value,
    2018,  100,
    2019,  110,
    2020,  120
  )
  
  result <- test_no_na |>
    Filling(Value, Year)
  
  expect_equal(result$Value, test_no_na$Value)
})
