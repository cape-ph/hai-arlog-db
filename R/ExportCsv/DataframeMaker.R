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

# when working with excel files on mac, an opened excel file leaves a trace
# hidden file that will interfere with the execution of the code
# close all files in the folders you are going to be operating on
create_excel_cpo <- function(){
  ExcelCpoRaw <- "~/Projects/hai-arlog-db/data/ExcelCpo"

  setwd(ExcelCpoRaw)
  file_list <- list.files()

  results <- lapply(file_list, excel_cpo_extractor)

  results_bind <- bind_rows(results)
  result_df <- as.data.frame(results_bind)

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
  pdf_list <- lapply(file_list, pdf_text)
  pdf_tables <- lapply(pdf_list, tenn_cpo_seq_extractor)
  results_bind <- bind_rows(pdf_tables)

  #write_csv(df, paste0(CsvOutputDir,"cpo_seq.csv"))
  return(results_bind)
}

create_web_portal <- function(){
  WebPortalRaw <- "~/Projects/hai-arlog-db/data/ExcelArlnWebPortal/"

  setwd(WebPortalRaw)
  file_list <- list.files()
  xl_list <- lapply(file_list, read.csv)
  results_bind <- bind_rows(xl_list)
  return(results_bind)
}
