#' @title Full process for extracting a table correctly from a CRE alert.
#' @description
#' This function is called through the main function in the program. It
#' is the full extraction process for a CRE Alert that comes in as a word
#' document, in the WordAlert folder. The function calls its functions from
#' ToolsDocx.R.
#'
#' @param word_file_path The file path for a CRE Alert word document containing one table.
#'
#' @return The table from a CRE alert with columns headers extracted from the first data row.
#' @export
#'
#' @examples
#' # Use system.file to to get the path to the example document
#'   word_file_path <- system.file("extdata", "word_example_document.docx", package = "arlog")
#'   word_table <- word_extraction(word_file_path)
#'   print(word_table)
#'
word_extraction <- function(word_file_path) {
  doc <- docx_extract_table(word_file_path)
  cre_table <- docx_rename_columns_with_first_row(doc)
  return(cre_table)
}
