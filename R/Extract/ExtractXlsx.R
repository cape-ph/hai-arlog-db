source("~/Projects/hai-arlog-db/R/Tools/ToolsXlsx.R")
library(openxlsx)

excel_cpo_extractor <- function(excel_file_path) {
  exl <- read.xlsx(excel_file_path, colNames=TRUE)

  print('s1 cpo extract')
  print(colnames(exl))
  df <- excel_set_second_row_as_colnames(exl)

  print('s2 cpo extract')
  print(colnames(df))

  return(df)
}
