source("~/Projects/hai_arlog/WrdExclExtractTable.R")
library(openxlsx)

word_extraction <- function(word_file_path) {
  doc <- docx_extract_table(word_file_path)
  edited_doc <- docx_rename_columns_with_first_row(doc)

return(edited_doc)
}

excel_cpo_extractor <- function(excel_file_path) {
  exl <- read.xlsx(excel_file_path, colNames=TRUE)
  df <- excel_set_second_row_as_colnames(exl)
    
return(df)
}


