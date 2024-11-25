source('~/Projects/haiarlogdb/R/Tools/ToolsDocx.R')

library(openxlsx)

word_extraction <- function(word_file_path) {
  doc <- docx_extract_table(word_file_path)
  edited_doc <- docx_rename_columns_with_first_row(doc)
return(edited_doc)
}
