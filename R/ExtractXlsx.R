#' @title Full process for some excel files used to create the AR Log.
#' @description
#' This function is called through the main function in the program. It
#' is the full extraction process for excel cpo files, those found in
#' in ExcelCpo folder. The function calls its functions from
#' ToolsXlsx.R.
#'
#' @param excel_file_path The path for an Excel CPO file.
#'
#' @return The dataframe created from reading the CPO excel file and setting
#' the correct column names.
#'
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
excel_cpo_extractor <- function(excel_file_path) {
  exl <- openxlsx::read.xlsx(excel_file_path, colNames=TRUE)
  df <- excel_set_second_row_as_colnames(exl)
  return(df)
}
