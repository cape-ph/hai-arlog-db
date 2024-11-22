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

  print("length of results")
  print(length(results))
  print('column name of results 1')
  print(colnames(results[[1]]))
  print("****************")
  print('column name of results 2')
  print(colnames(results[[2]]))

  # no bind here, only one file

  all_identical <- TRUE

  # Compare each element of the results list to the first element
  for (i in 2:length(results)) {
    # Compare the current element with the first one
    comparison <- all.equal(results[[1]], results[[i]])

    # If any comparison is not identical, set the flag to FALSE and break the loop
    if (comparison != TRUE) {
      all_identical <- FALSE
      print(paste("Difference found between result 1 and result", i))
      print(comparison)  # Print the differences
      break  # Exit the loop after the first non-identical comparison
    }
  }

  # After the loop, check if all elements were identical
  if (all_identical) {
    print("All elements are identical.")
  } else {
    print("There are differences between the elements.")
  }



  results_bind <- bind_rows(results)
  print("colnames results bind")
  print(colnames(results_bind))
  #print("dataframe maker 1")
  #print(results_bind)
  result_df <- as.data.frame(results_bind)

  #result_df <- as.data.frame(results)
  #print("dataframe maker 2")
  #print(result_df)

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
