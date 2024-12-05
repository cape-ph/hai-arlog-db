#' @title Extract the first table from a word document.
#' @description
#'
#' Used in the package to extract word documents. Test using system.file.
#'
#' @param file_path The file path for a CRE Alert word document containing one table.
#'
#' @return The CRE Alert table from the word document.
#' @importFrom docxtractr read_docx docx_extract_all_tbls
#' @export
#'
#' @examples
#'  # Use system.file to get the path to the example document
#'   word_file_path <- system.file("extdata", "word_example_document.docx", package = "arlog")
#'
#'  # Read the document and extract the first table
#'   doc_table <- docx_extract_table(word_file_path)
#'   print(doc_table)
#'
docx_extract_table <- function(file_path) {
  # Read the Word document
  doc <- docxtractr::read_docx(file_path)

  # Extract tables from the document
  tables <- docxtractr::docx_extract_all_tbls(doc)

  # Check if there are tables and return the first one
  if (length(tables) == 0) {
    stop("No tables found in the document.")
  }

  # Return the first table as a data frame
  return(tables[[1]])
}



#' @title Rename the columns of a table with the elements of the first row of the table.
#' @description
#' Some docx files have a header row with two title column values irrelevant to the AR Log.
#' The first row contains the actual column names. This function takes the first data row, sets
#' it as column names and then removes this row of column names from the returned table.
#'
#' @param table The table extracted from a CRE alert by docx_extract_table.
#'
#' @return The table from the CRE alert with renamed columns.
#' @export
#'
#' @examples
#  # Use system.file to get the path to the example document
#'   word_file_path <- system.file("extdata", "word_example_document.docx", package = "arlog")
#' # Use docx_extract_table to read the document.
#'   doc_table <- docx_extract_table(word_file_path)
#'
#' # Read the document and extract the first table
#'   renamed_table <- docx_rename_columns_with_first_row(doc_table)
#'   print(renamed_table)
docx_rename_columns_with_first_row <- function(table) {
  # Check if the table has at least one row
  if (nrow(table) < 1) {
    stop("The table does not have enough rows to rename columns.")
  }

  # Set the first row as column names
  colnames(table) <- table[1, ]

  # Remove that first row after setting column names
  table <- table[-1, ]

  # Reset row names
  rownames(table) <- NULL

  return(table)
}
