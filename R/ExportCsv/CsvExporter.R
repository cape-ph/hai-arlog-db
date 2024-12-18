source('~/Projects/haiarlogdb/R/Tools/ToolsDataProcessing.R')

CsvOutputDir <- "~/Projects/haiarlogdb/processed/"

#' Title
#'
#' @param tenn_arln_df
#'
#' @return
#' @export
#'
#' @examples
process_tenn_arln <- function(tenn_arln_df) {
  s1 <- replace_spaces_with_underscores(tenn_arln_df)
  s2 <- replace_hash_with_num(s1)
  s3 <- capitalize_column_names(s2)

  write.csv(s3, paste0(CsvOutputDir,"tenn_arln.csv"), row.names = FALSE)
  return(s3)
}

process_word_alert <- function(word_alert_df) {
  date_columns <- c("DATE_OF_BIRTH", "DATE_OF_COLLECTION")
  s1 <- replace_spaces_with_underscores(word_alert_df)
  s2 <- capitalize_column_names(s1)
  s3 <- replace_dob_with_date_of_birth(s2)
  s4 <- replace_hyphens_with_na(s3)
  s5 <- convert_date_columns(s4,date_columns, date_format = "%m-%d-%Y")

  write.csv(s5, paste0(CsvOutputDir,"word_alert.csv"), row.names = FALSE)
  return(s5)
}

process_excel_cpo <- function(cpo_df) {
  date_columns <- c("DATE_SPECIMEN_RECEIVED", "DATE_REPORTED",
                    "DATE_OF_BIRTH", "DATE_OF_COLLECTION_(MM/DD/YYYY)")

  s1 <- clean_excel_column_names(cpo_df)
  s2 <- capitalize_column_names(s1)
  s3 <- replace_dob_with_date_of_birth(s2)
  s4 <- replace_double_underscore(s3)
  s5 <- replace_spaces_with_underscores(s4)
  s6 <- replace_excel_dots_with_underscores(s5)
  s7 <- convert_excel_dates(s6, date_columns)

  write.csv(s7, paste0(CsvOutputDir,"excel_cpo.csv"), row.names = FALSE)
  return(s7)
}

process_excel_sentinel <- function(df){
  date_columns <- c("DATE_OF_BIRTH", "CULTUREDT","COLLECTIONDT")
  s1 <- replace_dob_with_date_of_birth(df)
  s2 <- convert_date_columns(s1, date_columns, date_format = "%m/%d/%Y")

  write.csv(s2, paste0(CsvOutputDir,"excel_sent.csv"), row.names = FALSE)
  return(s2)
}

process_pdf_cpo_seq <- function(df){
  date_columns <- c("COLLECTION_DATE")
  s1 <- capitalize_column_names(df)
  s2 <- convert_date_columns(s1, date_columns, date_format = "%m/%d/%Y")

  write.csv(s2, paste0(CsvOutputDir,"pdf_cpo.csv"), row.names = FALSE)
  return(s2)
}

process_web_portal <- function(df){
  datetime_columns <- c("DATE_COLLECTED","DATE_RECEIVED", "DATE_RELEASED")

  s1 <- capitalize_column_names(df)
  s2 <- replace_spaces_with_underscores(s1)
  s3 <- replace_excel_dots_with_underscores(s2)
  s4 <- cut_off_time(s3, datetime_columns)

  write.csv(s4, paste0(CsvOutputDir,"web_portal.csv"), row.names = FALSE)
  return(s4)
}


