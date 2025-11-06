#' Utility Functions for Data Manipulation
#'
#' @description
#' Collection of general-purpose utility functions for data manipulation,
#' gap-filling, date handling, and Excel operations.
#'
#' @name utility_functions
NULL

#' Exclude operator
#'
#' @description Negation of the `%in%` operator
#'
#' @param x Vector or NULL: the values to be matched
#' @param y Vector or NULL: the values to be matched against
#'
#' @return A logical vector indicating if there is no match for each element of x
#'
#' @examples
#' c(1, 2, 3) %!in% c(2, 4, 6) # Returns TRUE FALSE TRUE
#'
#' @export
"%!in%" <- function(x, y) !("%in%"(x, y))

#' Drop columns from data frame
#'
#' @description Drops an undefined number of columns from a data frame
#'
#' @param df Data frame
#' @param ... Column names to drop (can be unquoted names or character vector)
#'
#' @return Data frame without the specified columns
#'
#' @export
drop_cols <- function(df, ...) {
  dots <- rlang::list2(...)
  
  # Handle both c("col1", "col2") and col1, col2 syntax
  if (length(dots) == 1 && is.character(dots[[1]])) {
    # Character vector passed
    col_names <- dots[[1]]
  } else {
    # Individual symbols passed
    cols <- rlang::ensyms(...)
    col_names <- purrr::map_chr(cols, rlang::as_string)
  }
  
  df |>
    dplyr::select(-dplyr::any_of(col_names))
}

#' Check if dataset is empty
#'
#' @description Checks if a data frame has no rows
#'
#' @param df Data frame to check
#'
#' @return Logical indicating if data frame has zero rows
#'
#' @export
is_empty <- function(df) {
  return(nrow(df) == 0)
}

#' Arrange data by dates
#'
#' @description Creates dates based on years and months, and arranges by date.
#' Requires Month_numbers object with Month_names and Month_number columns.
#'
#' @param x Data frame with Year and Month_names columns
#'
#' @return Data frame arranged by date with additional date columns
#'
#' @details Remember to group data when needed before using this function.
#' Requires Month_numbers and Month_order objects from load_general_data().
#'
#' @export
Arrange_dates <- function(x) {
  x |>
    dplyr::left_join(Month_numbers, by = "Month_names") |>
    dplyr::mutate(
      Date_str = paste("01", Month_names, Year, sep = "."),
      Date = base::as.Date(paste(Year, Month_number, "01", sep = "-"))
    ) |>
    dplyr::arrange(Date, .by_group = TRUE) |>
    dplyr::mutate(Month_names = factor(Month_names, levels = Month_order)) |>
    dplyr::ungroup()
}

#' Add sheet to existing Excel workbook
#'
#' @description Adds a new sheet to an existing Excel workbook. The sheet must not exist previously.
#'
#' @param wb_location Character string with path to Excel workbook
#' @param newsheet_name Character string with name for new sheet
#' @param newsheet_data Data frame to write to new sheet
#'
#' @return NULL (modifies workbook file in place)
#'
#' @export
add_xlsx_sheet <- function(wb_location, newsheet_name, newsheet_data) {
  wb <- openxlsx::loadWorkbook(wb_location)
  openxlsx::addWorksheet(wb, newsheet_name)
  openxlsx::writeData(wb, sheet = newsheet_name, newsheet_data)
  openxlsx::saveWorkbook(wb, wb_location, overwrite = TRUE)
}

#' Fill gaps in time series
#'
#' @description Fills gaps by linear interpolation when possible, or carrying
#' forward/backward when not possible. Labels output according to filling method.
#'
#' @param data Data frame (grouped if needed)
#' @param var Variable to be filled (unquoted column name)
#' @param Index Time index (usually year)
#'
#' @return Data frame with filled variable and source column indicating filling method
#'
#' @details
#' Remember to use `group_by()` and `ungroup()` when needed (do NOT use `.by` with this function).
#' Always add `dplyr::ungroup()` immediately after using this function to prevent
#' grouped data from flowing downstream.
#'
#' The function creates a Source_* column indicating:
#' - "Original": Value was present in original data
#' - "Linear interpolation": Value filled by linear interpolation
#' - "Last value carried forward": Value filled by carrying last known value forward
#' - "First value carried backwards": Value filled by carrying first known value backward
#'
#' @examples
#' \dontrun{
#' data |>
#'   group_by(Region, Item) |>
#'   Filling(Production, Year) |>
#'   ungroup()
#' }
#'
#' @export
Filling <- function(data, var, Index) {
  data |>
    dplyr::mutate(
      Value_interpfilled = zoo::na.approx({{ var }},
        x = {{ Index }},
        na.rm = FALSE
      ),
      Value_CarriedBackward = zoo::na.locf0({{ var }}),
      Value_CarriedForward = zoo::na.locf0({{ var }}, fromLast = TRUE),
      "Source_{{var}}" := ifelse(!is.na({{ var }}),
        "Original",
        ifelse(!is.na(Value_interpfilled),
          "Linear interpolation",
          ifelse(!is.na(Value_CarriedBackward),
            "Last value carried forward",
            "First value carried backwards"
          )
        )
      ),
      "{{var}}" := ifelse(!is.na({{ var }}),
        {{ var }},
        ifelse(!is.na(Value_interpfilled),
          Value_interpfilled,
          ifelse(!is.na(Value_CarriedBackward),
            Value_CarriedBackward,
            Value_CarriedForward
          )
        )
      )
    ) |>
    dplyr::select(-Value_interpfilled, -Value_CarriedForward, -Value_CarriedBackward)
}

#' Fill gaps using proxy variable
#'
#' @description Fills gaps by using changes in a proxy variable, using ratios
#' between filled variable and proxy variable. Labels output according to filling method.
#'
#' @param data Data frame (grouped if needed)
#' @param var Variable to be filled (unquoted column name)
#' @param proxyvar Variable used as proxy (unquoted column name)
#' @param Index Time index (usually year)
#'
#' @return Data frame with filled variable and source column indicating filling method
#'
#' @details
#' Remember to use `group_by()` and `ungroup()` when needed (do NOT use `.by` with this function).
#' Always add `dplyr::ungroup()` immediately after using this function to prevent
#' grouped data from flowing downstream.
#'
#' The function creates a Source_* column indicating:
#' - "Original": Value was present in original data
#' - "Proxy interpolated": Value filled using interpolated proxy ratio
#' - "Proxy carried forward": Value filled using proxy ratio carried forward
#' - "Proxy carried backwards": Value filled using proxy ratio carried backward
#'
#' @examples
#' \dontrun{
#' data |>
#'   group_by(Region, Item) |>
#'   FillingProxy(Production, Area, Year) |>
#'   ungroup()
#' }
#'
#' @export
FillingProxy <- function(data, var, proxyvar, Index) {
  data |>
    dplyr::mutate(Proxy_ratio = {{ var }} / {{ proxyvar }}) |>
    Filling(Proxy_ratio, Year) |>
    dplyr::mutate(
      "Source_{{var}}" := ifelse(!is.na({{ var }}),
        "Original",
        ifelse(Source_Proxy_ratio == "Linear interpolation",
          "Proxy interpolated",
          ifelse(Source_Proxy_ratio == "Last value carried forward",
            "Proxy carried forward",
            ifelse(Source_Proxy_ratio == "First value carried backwards",
              "Proxy carried backwards",
              NA
            )
          )
        )
      ),
      "{{var}}" := ifelse(!is.na({{ var }}),
        {{ var }},
        Proxy_ratio * {{ proxyvar }}
      )
    )
}

#' Fill gaps with accumulated sum
#'
#' @description Fills gaps in a column by accumulating values from another column.
#' The values are accumulated along the series.
#'
#' @param col1 Column to fill (will be modified)
#' @param col2 Column with values to add
#'
#' @return Vector with filled values
#'
#' @details For each NA in col1, fills with col1[i-1] + col2[i]
#'
#' @export
fill_na_with_sum <- function(col1, col2) {
  for (i in seq_along(col1)) {
    if (is.na(col1[i]) && i > 1) {
      col1[i] <- col1[i - 1] + col2[i]
    }
  }
  return(col1)
}
