source("~/Projects/hai_arlog/PostCsvProcessingTools.R")

CsvOutputDir <- "~/Projects/hai_arlog/test_db_preproc/"

process_tenn_arln <- function(tenn_arln_df) {
  s1 <- replace_spaces_with_underscores(tenn_arln_df)
  s2 <- replace_hash_with_num(s1)
  s3 <- capitalize_column_names(s2)
  
  return(s3)
  #write.csv(s3, paste0(CsvOutputDir,"tenn_arln.csv"), row.names = FALSE)
}

process_word_alert <- function(word_alert_df) {
  date_columns <- c("DATE_OF_BIRTH", "DATE_OF_COLLECTION")
  s1 <- replace_spaces_with_underscores(word_alert_df)
  s2 <- capitalize_column_names(s1)
  s3 <- replace_dob_with_date_of_birth(s2)
  s4 <- replace_hyphens_with_na(s3)
  s5 <- convert_date_columns(s4,date_columns, date_format = "%m-%d-%Y")
  return(s5)
  #write.csv(s5, paste0(CsvOutputDir,"word_alert.csv"), row.names = FALSE)
}

process_excel_cpo <- function(cpo_df) {
  date_columns <- c("DATE_SPECIMEN_RECEIVED", "DATE_REPORTED", 
                    "DATE_OF_BIRTH", "DATE_OF_COLLECTION_MM_DD_YYYY_")
  s1 <- clean_excel_column_names(cpo_df)
  s2 <- capitalize_column_names(s1)
  s3 <- replace_dob_with_date_of_birth(s2)
  s4 <- replace_double_underscore(s3)
  s5 <- replace_spaces_with_underscores(s4)
  s6 <- replace_excel_dots_with_underscores(s5)
  s7 <- convert_excel_dates(s6, date_columns)
  return(s7)
  #write.csv(s7, paste0(CsvOutputDir,"excel_cpo.csv"), row.names = FALSE)
}

process_excel_sentinel <- function(df){
  date_columns <- c("DATE_OF_BIRTH", "CULTUREDT","COLLECTIONDT")
  s1 <- replace_dob_with_date_of_birth(df)
  s2 <- convert_date_columns(s1, date_columns, date_format = "%m/%d/%Y")
  return(s2)
  #write.csv(s2, paste0(CsvOutputDir,"excel_sent.csv"), row.names = FALSE)
}

process_pdf_cpo_seq <- function(df){
  date_columns <- c("COLLECTION_DATE")
  s1 <- capitalize_column_names(df)
  s2 <- convert_date_columns(s1, date_columns, date_format = "%m/%d/%Y")
  return(s2)
  #write.csv(s2, paste0(CsvOutputDir,"pdf_cpo.csv"), row.names = FALSE)
}

process_web_portal <- function(df){
  datetime_columns <- c("DATE_COLLECTED","DATE_RECEIVED", "DATE_RELEASED")
  
  s1 <- capitalize_column_names(df)
  s2 <- replace_spaces_with_underscores(s1)
  s3 <- replace_excel_dots_with_underscores(s2)
  s4 <- cut_off_time(s3, datetime_columns)
  
  return(s4)
  #write.csv(s4, paste0(CsvOutputDir,"web_portal.csv"), row.names = FALSE)
}


# for testing purposes only
provisional_postproc_csv_writer <- function() {
  print("tenn arln")
  process_tenn_arln(create_tenn_arln_csv())
  print("word alert")
  process_word_alert(create_word_alert_csv())
  print("excel cpo")
  process_excel_cpo(create_excel_cpo_csv())
  print("excel sentinel")
  process_excel_sentinel(create_sentinel_csv())
  print("pdf cpo")
  process_pdf_cpo_seq(create_cpo_seq_csv())
  print("process web portal")
  process_web_portal(create_web_portal_csv())
}

