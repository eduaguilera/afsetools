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
#' @description Drops an undefined number of columns from a data frame.
#' Silently ignores columns that don't exist in the data frame.
#'
#' @param df Data frame
#' @param ... Column names to drop (character strings or character vector)
#'
#' @return Data frame without the specified columns
#'
#' @examples
#' df <- data.frame(a = 1:3, b = 4:6, c = 7:9)
#' drop_cols(df, "a", "b")
#' drop_cols(df, c("a", "nonexistent"))  
#'
#' @export
drop_cols <- function(df, ...) {
 col_names <- unlist(list(...))
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


