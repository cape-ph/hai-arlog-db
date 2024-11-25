source("~/Projects/haiarlogdb/R/Extract/ExtractXlsx.R")
source("~/Projects/haiarlogdb/R/Extract/ExtractDocx.R")
source("~/Projects/haiarlogdb/R/Extract/ExtractCpoSeqReport.R")
source("~/Projects/haiarlogdb/R/Extract/ExtractTennPdfData.R")
source("~/Projects/haiarlogdb/R/Extract/CombineTennPdfData.R")

create_tenn_arln <- function(){
  PdfArlnRaw <- "~/Projects/haiarlogdb/data/PdfArln"
  setwd(PdfArlnRaw)
  file_list <- list.files()
  rows <- data_rows(file_list)
  arln_table <- create_table_arln(rows)
  return(arln_table)
}

create_word_alert <- function(){
  WordAlertRaw <- "~/Projects/haiarlogdb/data/WordAlert"
  setwd(WordAlertRaw)
  file_list <- list.files()
  results <- lapply(file_list, word_extraction)
  results_bind <- bind_rows(results)
  return(results_bind)
}

# when working with excel files on mac, an opened excel file leaves a trace
# hidden file that will interfere with the execution of the code
# close all files in the folders you are going to be operating on
create_excel_cpo <- function(){
  ExcelCpoRaw <- "~/Projects/haiarlogdb/data/ExcelCpo"
  setwd(ExcelCpoRaw)
  file_list <- list.files()
  results <- lapply(file_list, excel_cpo_extractor)
  results_bind <- bind_rows(results)
  result_df <- as.data.frame(results_bind)
  return(result_df)
}

create_sentinel <- function(){
  ExcelSentinelRaw <- "~/Projects/haiarlogdb/data/ExcelSentinel"
  setwd(ExcelSentinelRaw)
  file_list <- list.files()
  results <- lapply(file_list, read_excel)
  results_bind <- bind_rows(results)
  return(results_bind)
}

create_cpo_seq <- function(){
  PdfCpoSeqRaw <- "~/Projects/haiarlogdb/data/PdfCPOSeq"
  setwd(PdfCpoSeqRaw)
  file_list <- list.files()
  pdf_list <- lapply(file_list, pdf_text)
  pdf_tables <- lapply(pdf_list, tenn_cpo_seq_extractor)
  results_bind <- bind_rows(pdf_tables)
  return(results_bind)
}

create_web_portal <- function(){
  WebPortalRaw <- "~/Projects/haiarlogdb/data/ExcelArlnWebPortal/"
  setwd(WebPortalRaw)
  file_list <- list.files()
  xl_list <- lapply(file_list, read.csv)
  results_bind <- bind_rows(xl_list)
  return(results_bind)
}
