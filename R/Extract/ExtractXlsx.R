source("~/Projects/haiarlogdb/R/Tools/ToolsXlsx.R")
library(openxlsx)

excel_cpo_extractor <- function(excel_file_path) {
  exl <- read.xlsx(excel_file_path, colNames=TRUE)
  df <- excel_set_second_row_as_colnames(exl)
  return(df)
}
