source("~/Projects/hai-arlog-db/R/Extract/ExtractXlsx.R")
source("~/Projects/hai-arlog-db/R/Extract/ExtractDocx.R")
source("~/Projects/hai-arlog-db/R/Extract/ExtractCpoSeqReport.R")
source("~/Projects/hai-arlog-db/R/Extract/ExtractTennPdfData.R")
source("~/Projects/hai-arlog-db/R/Extract/CombineTennPdfData.R")

#CsvOutputDir <- "~/Projects/hai-arlog-db/test_db/"

create_tenn_arln <- function(){
  PdfArlnRaw <- "~/Projects/hai-arlog-db/data/PdfArln"
  
  setwd(PdfArlnRaw)
  file_list <- list.files()
  rows <- data_rows(file_list)
  
  arln_table <- create_table_arln(rows)
  
  #write_csv(arln_table, paste0(CsvOutputDir,"tenn_arln.csv"))
  return(arln_table)
}

create_word_alert <- function(){
  WordAlertRaw <- "~/Projects/hai-arlog-db/data/WordAlert"
  
  setwd(WordAlertRaw)
  file_list <- list.files()
  
  results <- lapply(file_list, word_extraction)
  results_bind <- bind_rows(results)
  
  #write_csv(results_bind, paste0(CsvOutputDir,"word_alert.csv"))
  return(results_bind)
}

create_excel_cpo <- function(){
  ExcelCpoRaw <- "~/Projects/hai-arlog-db/data/ExcelCpo"
  
  setwd(ExcelCpoRaw)
  file_list <- list.files()
  
  results <- lapply(file_list, excel_cpo_extractor)
  # no bind here, only one file
  
  result_df <- as.data.frame(results)
  
  #write_csv(result_df, paste0(CsvOutputDir,"excel_cpo.csv"))
  return(result_df)
}

create_sentinel <- function(){
  ExcelSentinelRaw <- "~/Projects/hai-arlog-db/data/ExcelSentinel"
  
  setwd(ExcelSentinelRaw)
  file_list <- list.files()
  
  results <- lapply(file_list, read_excel)
  
  results_bind <- bind_rows(results)
  #write_csv(results_bind, paste0(CsvOutputDir,"excel_sentinel.csv"))
  return(results_bind)
}

create_cpo_seq <- function(){
  PdfCpoSeqRaw <- "~/Projects/hai-arlog-db/data/PdfCPOSeq"
  
  setwd(PdfCpoSeqRaw)
  file_list <- list.files()
  pdf_file_path <- file_list
  
  pdf <- pdf_text(pdf_file_path)
  df <- tenn_cpo_seq_extractor(pdf)
  
  #write_csv(df, paste0(CsvOutputDir,"cpo_seq.csv"))
  return(df)
}

create_web_portal <- function(){
  xl <- "~/Projects/hai-arlog-db/data/ExcelArlnWebPortal/ARLN Web Portal Example Linked.csv"
  exl <- read.csv(xl)
  return(exl)
}

#provisional_csv_writer <- function() {
#  create_tenn_arln_csv()
#  create_word_alert_csv()
#  create_excel_cpo_csv()
#  create_sentinel_csv()
#  create_cpo_seq_csv()
#}