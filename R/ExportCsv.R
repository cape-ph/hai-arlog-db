#' @title Process the 'tenn_arln' Data Frame and Save as CSV
#'
#' @description
#' This function processes the `tenn_arln_df` data frame by applying a series of
#' transformations:
#' 1. Replaces spaces in column names with underscores.
#' 2. Replaces any `#` characters in column names with the string "Num".
#' 3. Capitalizes all column names.
#'
#' After performing these transformations, the processed data is saved as a CSV
#' file in the specified output directory.
#'
#' @importFrom utils write.csv
#' @param tenn_arln_df A `tenn_arln` data frame to be processed
#' @param CSV_OUTPUT_DIR Directory to place the created csv file.
#'
#' @return NULL
#' This function does not return anything. It saves the processed data as a CSV file
#' in the `CSV_OUTPUT_DIR` directory.
#'
#' @details
#' The function expects the `CSV_OUTPUT_DIR` variable to specify the directory
#' where the resulting CSV file will be saved. The output file will be named
#' `tenn_arln.csv`.The function does not return any output to the user, but it
#' will write the processed data frame to the specified location.
#'
#'
#' @export
process_tenn_arln <- function(tenn_arln_df, CSV_OUTPUT_DIR) {
  s1 <- replace_spaces_with_underscores(tenn_arln_df)
  s2 <- replace_hash_with_num(s1)
  s3 <- capitalize_column_names(s2)

  filename <- "tenn_arln.csv"
  full_file_path <- file.path(CSV_OUTPUT_DIR, filename)
  write.csv(s3, full_file_path, row.names = FALSE)
}


#' @title Process the 'word_alert' Data Frame and Save as CSV
#'
#' @description
#' This function processes the `word_alert_df` data frame by applying a series of
#' transformations:
#' 1. Replaces spaces in column names with underscores.
#' 2. Capitalizes all column names.
#' 3. Replaces "DOB" with "DATE_OF_BIRTH" in column names.
#' 4. Replaces hyphens (more than 2 consecutive hyphens) in character columns
#' with `NA`.
#' 5. Converts date columns ("DATE_OF_BIRTH" and "DATE_OF_COLLECTION") from
#' character to Date type using the specified date format (`%m-%d-%Y`).
#'
#' After performing these transformations, the processed data is saved as a CSV
#' file in the specified output directory.
#'
#' @importFrom utils write.csv
#' @param word_alert_df A data frame to be processed, containing the
#' `word_alert` data.
#' @param CSV_OUTPUT_DIR Directory to place the created csv file.
#'
#' @return NULL
#' This function does not return anything. It saves the processed data as a
#' CSV file in the `Processed` directory.
#'
#' @details
#' The function expects the `CSV_OUTPUT_DIR` variable to specify the directory
#' where the resulting CSV file will be saved. The output file will be named
#' `word_alert.csv`. The function does not return any output to the user,
#' but it will write the processed data frame to the specified location.
#'
#'
#' @export
process_word_alert <- function(word_alert_df, CSV_OUTPUT_DIR) {
  date_columns <- c("DATE_OF_BIRTH", "DATE_OF_COLLECTION")
  s1 <- replace_spaces_with_underscores(word_alert_df)
  s2 <- capitalize_column_names(s1)
  s3 <- replace_dob_with_date_of_birth(s2)
  s4 <- replace_hyphens_with_na(s3)
  s5 <- convert_date_columns(s4,date_columns, date_format = "%m-%d-%Y")

  filename <- "word_alert.csv"
  full_file_path <- file.path(CSV_OUTPUT_DIR, filename)
  write.csv(s5, full_file_path, row.names = FALSE)
}


#' @title Process Excel CPO Data
#'
#' @description This function processes raw CPO data from an Excel file by
#' cleaning the column names, converting date columns to the correct format, and
#' saving the processed data to a CSV file in a specified directory.
#'
#' @importFrom utils write.csv
#' @param cpo_df A DataFrame containing the raw CPO data from an Excel sheet.
#' @param CSV_OUTPUT_DIR Directory to place the created csv file.
#'
#' @details The function performs the following operations on the input DataFrame:
#' - Cleans Excel column names (removes carriage returns, newlines, and
#' converts characters).
#' - Capitalizes all column names.
#' - Replaces "DOB" with "DATE_OF_BIRTH" and capitalizes.
#' - Replaces double underscores in column names with single underscores.
#' - Replaces spaces in column names with underscores.
#' - Converts date columns to the correct date format (MM/DD/YYYY).
#'
#' It then saves the processed data to a CSV file in a directory specified by
#' the global variable `CSV_OUTPUT_DIR` with the filename `"excel_cpo.csv"`.
#'
#' @return NULL This function does not return any value. It saves the processed
#'         DataFrame to a CSV file.
#'
#'
#' @export
process_excel_cpo <- function(cpo_df, CSV_OUTPUT_DIR) {
  date_columns <- c("DATE_SPECIMEN_RECEIVED", "DATE_REPORTED",
                    "DATE_OF_BIRTH", "DATE_OF_COLLECTION_(MM/DD/YYYY)")

  s1 <- clean_excel_column_names(cpo_df)
  s2 <- capitalize_column_names(s1)
  s3 <- replace_dob_with_date_of_birth(s2)
  s4 <- replace_double_underscore(s3)
  s5 <- replace_spaces_with_underscores(s4)
  s6 <- replace_excel_dots_with_underscores(s5)
  s7 <- convert_excel_dates(s6, date_columns)

  filename <- "excel_cpo.csv"
  full_file_path <- file.path(CSV_OUTPUT_DIR, filename)
  write.csv(s7, full_file_path, row.names = FALSE)
}


#' @title Process Excel Sentinel Data
#'
#' @description This function processes raw Sentinel data from an Excel file,
#' specifically converting certain date columns to the correct date format and
#' saving the processed data to a CSV file in a specified directory.
#'
#' @importFrom utils write.csv
#' @param df A DataFrame containing the raw Sentinel data from an Excel sheet.
#' @param CSV_OUTPUT_DIR Directory to place the created csv file.
#'
#' @details The function performs the following operations on the input
#' DataFrame:
#' - Replaces "DOB" with "DATE_OF_BIRTH" and ensures correct date format.
#' - Converts the columns `DATE_OF_BIRTH`, `CULTUREDT`, and `COLLECTIONDT`
#' to the correct date format (MM/DD/YYYY).
#'
#' It then saves the processed data to a CSV file in a directory specified by
#' the global variable `CSV_OUTPUT_DIR` with the filename `"excel_sent.csv"`.
#'
#' @return NULL This function does not return any value. It saves the processed
#' DataFrame to a CSV file.
#'
#'
#' @export
process_excel_sentinel <- function(df, CSV_OUTPUT_DIR){
  date_columns <- c("DATE_OF_BIRTH", "CULTUREDT","COLLECTIONDT")
  s1 <- replace_dob_with_date_of_birth(df)
  s2 <- convert_date_columns(s1, date_columns, date_format = "%m/%d/%Y")

  filename <- "excel_sent.csv"
  full_file_path <- file.path(CSV_OUTPUT_DIR, filename)
  write.csv(s2, full_file_path, row.names = FALSE)
}


#' @title Process PDF CPO Sequence Data
#'
#' @description This function processes raw PDF CPO sequence data, specifically
#' converting the `COLLECTION_DATE` column to the correct date format
#' (MM/DD/YYYY) and saving the processed data to a CSV file in a specified
#' directory.
#'
#' @importFrom utils write.csv
#' @param df A DataFrame containing the raw CPO sequence data extracted from a
#' PDF.
#' @param CSV_OUTPUT_DIR Directory to place the created csv file.
#'
#' @details The function performs the following operations on the input
#' DataFrame:
#' - Capitalizes the column names to ensure uniformity.
#' - Converts the `COLLECTION_DATE` column to the correct date format
#' (MM/DD/YYYY).
#'
#' It then saves the processed data to a CSV file in a directory specified by
#' the global variable `CsvOutputDir` with the filename `"pdf_cpo.csv"`.
#'
#' @return NULL This function does not return any value. It saves the processed
#' DataFrame to a CSV file.
#'
#'
#' @export
process_pdf_cpo_seq <- function(df, CSV_OUTPUT_DIR){
  date_columns <- c("COLLECTION_DATE")
  s1 <- capitalize_column_names(df)
  s2 <- convert_date_columns(s1, date_columns, date_format = "%m/%d/%Y")

  filename <- "pdf_cpo.csv"
  full_file_path <- file.path(CSV_OUTPUT_DIR, filename)
  write.csv(s2, full_file_path, row.names = FALSE)
}


#' @title Process Web Portal Data
#'
#' @description This function processes web portal data by capitalizing column
#' names, replacing spaces with underscores, replacing Excel-style dots with
#' underscores, and trimming datetime columns to remove the time component. The
#' processed data is then saved as a CSV file.
#'
#' @importFrom utils write.csv
#' @param df A DataFrame containing the raw web portal data.
#' @param CSV_OUTPUT_DIR Directory to place the created csv file.
#'
#' @details The function performs the following operations:
#' - Capitalizes all column names in the data frame.
#' - Replaces spaces with underscores in column names.
#' - Replaces dots (.) in column names with underscores, handling Excel-specific
#' formatting.
#' - Trims datetime columns to remove the time portion, leaving only the date.
#'
#' The processed data is saved to a CSV file with the name `"web_portal.csv"`
#' in the directory defined by the global variable `CSV_OUTPUT_DIR`.
#'
#' @return NULL This function does not return any value. It writes the processed
#' data to a CSV file.
#'
#'
#' @export
process_web_portal <- function(df, CSV_OUTPUT_DIR){
  datetime_columns <- c("DATE_COLLECTED","DATE_RECEIVED", "DATE_RELEASED")

  s1 <- capitalize_column_names(df)
  s2 <- replace_spaces_with_underscores(s1)
  s3 <- replace_excel_dots_with_underscores(s2)
  s4 <- cut_off_time(s3, datetime_columns)

  filename <- "web_portal.csv"
  full_file_path <- file.path(CSV_OUTPUT_DIR, filename)
  write.csv(s4, full_file_path, row.names = FALSE)
}
