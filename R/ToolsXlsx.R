#' @title Rename the columns of a table with the elements of the first row of the table.
#' @description
#' Some excel files have a header row with two title column values irrelevant to the AR Log.
#' The first row contains the actual column names. This function takes the first data row,
#' sets it as column names and then removes this row of column names from the returned table.
#'
#' @param data The table extracted from an excel file.
#'
#' @return The table from the excel file with the renamed columns.
#' @importFrom openxlsx read.xlsx
#' @export
#'
#' @examples
#' # Use system.file to to get the path to the example document
#'   excel_file_path <- system.file("extdata", "excel_example_document.xlsx", package = "arlog")
#' # Use openxlsx to read in an excel file
#'   exl <- openxlsx::read.xlsx(excel_file_path, colNames=TRUE)
#'
#' # Use the function to return the correctly formatted table.
#'   excel_table <- excel_set_second_row_as_colnames(exl)
#'   print(excel_table)
#'
excel_set_second_row_as_colnames <- function(data) {
  # Check if the data has at least two rows
  if (nrow(data) < 2) {
    stop("Data must have at least two rows to set column names.")
  }

  # Set the second row as column names
  colnames(data) <- data[1, ]

  # Remove the first row
  data <- data[-c(1,1), , drop = FALSE]

  # Return the cleaned data
  return(data)
}
