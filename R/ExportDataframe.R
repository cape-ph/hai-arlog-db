#' @title Create a Table from 'PdfArln' Directory Files
#'
#' @description
#' This function processes PDF files located in the 'PdfArln' directory inside
#' the package's 'extdata' folder. It checks whether the directory exists and
#' contains any files. If files are found, they are processed and a table is
#' created from the data in those files. If the directory doesn't exist or is
#' empty, an error is raised.
#'
#' @details
#' The function checks if the 'PdfArln' directory exists within the package's
#' 'extdata' folder. If the directory is missing or contains no files, it will
#' raise an error with an appropriate message. If files are present, the function
#' will read the files and process them using the `data_rows` and
#' `create_table_arln` functions to generate a final output table.
#'
#' @param PDF_ARLN_DIR File directory for ARLN pdf files.
#'
#' @return A data frame containing the processed table generated from the files
#'   in the 'PdfArln' directory. If the directory doesn't exist or is empty,
#'   an error is raised.
#'
#'
#' @importFrom tools file_ext
#'
#'
#' @export
create_tenn_arln <- function(PDF_ARLN_DIR) {
  # Define the path to the 'PdfArln' directory inside the 'extdata' folder
  dir_path <- PDF_ARLN_DIR

  # Check if the directory exists and is not empty
  if (!dir.exists(dir_path)) {
    stop("The 'PdfArln' directory does not exist: ", dir_path)
  }

  file_list <- list.files(dir_path, full.names = TRUE)

  # Check if the folder is empty
  if (length(file_list) == 0) {
    stop("The 'PdfArln' folder is empty. Please add files before proceeding.")
  }

  # Proceed with processing the files if the folder is not empty
  rows <- data_rows(file_list)
  arln_table <- create_table_arln(rows)

  return(arln_table)
}


#' @title Process Files in the 'WordAlert' Directory
#'
#' @description
#' This function checks for the presence of files in the 'WordAlert' directory,
#' located inside the package's 'extdata' folder. If files are found, it
#' processes each file using the `word_extraction` function and returns a
#' combined data frame with the results. If the directory doesn't exist or is
#' empty, the function raises
#' an error.
#'
#' @details
#' The function first checks if the 'WordAlert' directory exists within the
#' package's 'extdata' folder. If the directory is missing or contains no files,
#' it raises an error with an appropriate message. If files are present, it uses
#' the `word_extraction` function to process each file, and combines the results
#' into a single data frame using `bind_rows`.
#'
#' @param WORD_ALERT_DIR Directory for CRE word alerts.
#'
#' @return A data frame that combines the results of processing all files in the
#'   'WordAlert' directory. If the directory doesn't exist or is empty, an error
#'   will be raised.
#'
#' @importFrom dplyr bind_rows
#' @importFrom tools file_ext
#'
#'
#' @export
create_word_alert <- function(WORD_ALERT_DIR){
  dir_path <- WORD_ALERT_DIR

  # Check if the directory exists and is not empty
  if (!dir.exists(dir_path)) {
    stop("The 'WordAlert' directory does not exist: ", dir_path)
  }

  file_list <- list.files(dir_path, full.names = TRUE)

  # Check if the folder is empty
  if (length(file_list) == 0) {
    stop("The 'WordAlert' folder is empty. Please add files before proceeding.")
  }

  results <- lapply(file_list, word_extraction)
  results_bind <- bind_rows(results)
  return(results_bind)
}


#' @title Process Files in the 'ExcelCpo' Directory
#'
#' @description
#' This function checks if the 'ExcelCpo' directory exists within the package's
#' 'extdata' folder and if it contains any files. If files are found, it processes
#' them using the `excel_cpo_extractor` function and combines the results into a
#' single data frame. If the directory does not exist or is empty, the function
#' raises an error.
#'
#' @details
#' The function first checks if the 'ExcelCpo' directory exists within the package's
#' 'extdata' folder. If the directory is missing or contains no files, it raises an
#' error with an appropriate message. If files are present, it processes each file
#' using the `excel_cpo_extractor` function and combines the results into a single
#' data frame using `bind_rows`.
#'
#' @return A data frame that combines the results of processing all files in the
#'   'ExcelCpo' directory. If the directory doesn't exist or is empty, an error
#'   will be raised.
#'
#' @param EXCEL_CPO_DIR Directory for excel cpo files.
#'
#' @importFrom dplyr bind_rows
#'
#' @export
create_excel_cpo <- function(EXCEL_CPO_DIR){
  dir_path <- EXCEL_CPO_DIR

  # Check if the directory exists and is not empty
  if (!dir.exists(dir_path)) {
    stop("The 'ExcelCpo' directory does not exist: ", dir_path)
  }

  file_list <- list.files(dir_path, full.names = TRUE)

  # Check if the folder is empty
  if (length(file_list) == 0) {
    stop("The 'ExcelCpo' folder is empty. Please add files before proceeding.")
  }

  results <- lapply(file_list, excel_cpo_extractor)
  results_bind <- bind_rows(results)
  result_df <- as.data.frame(results_bind)
  return(result_df)
}


#' @title Process Files in the 'ExcelSentinel' Directory
#'
#' @description
#' This function checks if the 'ExcelSentinel' directory exists inside the
#' package's 'extdata' folder and if it contains any files. If files are found,
#' it reads them using the `read_excel` function and combines the results into a
#' single data frame. If the directory does not exist or is empty, the function
#' raises an error.
#'
#' @details
#' The function first checks if the 'ExcelSentinel' directory exists within the
#' package's 'extdata' folder. If the directory is missing or contains no files,
#' it raises an error with an appropriate message. If files are present, it
#' processes each file using the `read_excel` function and combines the results
#' into a single data frame using `bind_rows`.
#'
#' @param EXCEL_SENTINEL_DIR description
#'
#' @return A data frame that combines the results of processing all files in the
#'   'ExcelSentinel' directory. If the directory doesn't exist or is empty, an
#'   error will be raised.
#'
#' @importFrom readxl read_excel
#' @importFrom dplyr bind_rows
#'
#' @export
create_sentinel <- function(EXCEL_SENTINEL_DIR){
  dir_path <- EXCEL_SENTINEL_DIR

  # Check if the directory exists and is not empty
  if (!dir.exists(dir_path)) {
    stop("The 'ExcelSentinel' directory does not exist: ", dir_path)
  }

  file_list <- list.files(dir_path, full.names = TRUE)

  # Check if the folder is empty
  if (length(file_list) == 0) {
    stop("The 'ExcelSentinel' folder is empty. Please add files before proceeding.")
  }

  results <- lapply(file_list, read_excel)
  results_bind <- bind_rows(results)
  return(results_bind)
}


#' @title Process Files in the 'PdfCpoSeq' Directory
#'
#' @description
#' This function checks if the 'PdfCpoSeq' directory exists inside the package's
#' 'extdata' folder and if it contains any files. If files are found, it
#' processes them by extracting text from PDFs and then extracting specific
#' information from those texts using the `tenn_cpo_seq_extractor` function. The
#' results are then combined into a single data frame. If the directory does not
#' exist or is empty, an error is raised.
#'
#' @details
#' The function first checks if the 'PdfCpoSeq' directory exists within the
#' package's 'extdata' folder. If the directory is missing or contains no files,
#' it raises an error with an appropriate message. If files are present, it
#' processes each file using the `pdf_text` function from the `pdftools` package
#' to extract the text from the PDFs. Then, the `tenn_cpo_seq_extractor`
#' function is applied to the extracted text. Finally, the results are combined
#' using `bind_rows`.
#'
#' @param PDF_CPO_SEQ_DIR Directory for cpo seq pdf files.
#'
#' @return A data frame that combines the results of processing all files in the
#'   'PdfCpoSeq' directory. If the directory doesn't exist or is empty, an error
#'   will be raised.
#'
#' @importFrom pdftools pdf_text
#' @importFrom dplyr bind_rows
#'
#' @export
create_cpo_seq <- function(PDF_CPO_SEQ_DIR){
  dir_path <- PDF_CPO_SEQ_DIR

  # Check if the directory exists and is not empty
  if (!dir.exists(dir_path)) {
    stop("The 'PdfCpoSeq' directory does not exist: ", dir_path)
  }

  file_list <- list.files(dir_path, full.names = TRUE)

  # Check if the folder is empty
  if (length(file_list) == 0) {
    stop("The 'PdfCpoSeq' folder is empty. Please add files before proceeding.")
  }

  pdf_list <- lapply(file_list, pdf_text)
  pdf_tables <- lapply(pdf_list, tenn_cpo_seq_extractor)
  results_bind <- bind_rows(pdf_tables)
  return(results_bind)
}


#' @title Process Files in the 'ExcelArlnWebPortal' Directory
#'
#' @description
#' This function checks if the 'ExcelArlnWebPortal' directory exists inside the
#' package's 'extdata' folder and if it contains any files. If files are found,
#' it processes them by reading CSV files and then combines the results into a
#' single data frame.
#' If the directory does not exist or is empty, an error is raised.
#'
#' @details
#' The function first checks if the 'ExcelArlnWebPortal' directory exists within
#' the package's 'extdata' folder. If the directory is missing or contains no
#' files, it raises an error with an appropriate message. If files are present,
#' it reads each CSV file using `read.csv` and combines the results using
#' `bind_rows` from the `dplyr` package. This allows the user to work with the
#' data in a combined data frame format.
#'
#' @return A data frame that combines the results of processing all CSV files in
#'.  the 'ExcelArlnWebPortal' directory. If the directory doesn't exist or is
#'   empty, an error will be raised.
#'
#' @param EXCEL_ARLN_WEB_PORTAL_DIR Directory for ARLN web portal excel files.
#'
#' @importFrom dplyr bind_rows
#' @importFrom utils read.csv
#'
#' @export
create_web_portal <- function(EXCEL_ARLN_WEB_PORTAL_DIR){
  dir_path <- EXCEL_ARLN_WEB_PORTAL_DIR

  # Check if the directory exists and is not empty
  if (!dir.exists(dir_path)) {
    stop("The 'ExcelArlnWebPortal' directory does not exist: ", dir_path)
  }

  file_list <- list.files(dir_path, full.names = TRUE)

  # Check if the folder is empty
  if (length(file_list) == 0) {
    stop("The 'ExcelArlnWebPortal' folder is empty. Please add files before proceeding.")
  }

  xl_list <- lapply(file_list, read.csv)
  results_bind <- bind_rows(xl_list)
  return(results_bind)
}
