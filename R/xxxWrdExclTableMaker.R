source("~/Projects/hai_arlog/WordExcelExtractor.R")

create_word_alert_csv <- function(){
  WordAlertRaw <- "~/Projects/hai_arlog/data/WordAlert"
  WordAlertProc <- "~/Projects/hai_arlog/processed_data/WordAlert/"
  
  setwd(WordAlertRaw)
  file_list <- list.files()
  
  results <- lapply(file_list, word_extraction)
  results_bind <- bind_rows(results)
  
  
  write_csv(results_bind, paste0(WordAlertProc,"word_alert.csv"))
}

create_excel_cpo_csv <- function(){
  ExcelCpoRaw <- "~/Projects/hai_arlog/data/ExcelCpo"
  ExcelCpoProc <- "~/Projects/hai_arlog/processed_data/ExcelCpo/"
  
  setwd(ExcelCpoRaw)
  file_list <- list.files()
  
  results <- lapply(file_list, excel_cpo_extractor)
  # no bind here, only one file
  
  result_df <- as.data.frame(results)
  
  
  
  write_csv(result_df, paste0(ExcelCpoProc,"excel_cpo.csv"))
}


create_sentinel_csv <- function(){
  ExcelSentinelRaw <- "~/Projects/hai_arlog/data/ExcelSentinel"
  ExcelSentinelProc <- "~/Projects/hai_arlog/processed_data/ExcelSentinel/"
  
  setwd(ExcelSentinelRaw)
  file_list <- list.files()
  
  results <- lapply(file_list, read_excel)
  
  results_bind <- bind_rows(results)
  write_csv(results_bind, paste0(ExcelSentinelProc,"excel_sentinel.csv"))
}

