source("~/Projects/haiarlogdb/R/ExportCsv/DataframeMaker.R")
library(lubridate)

replace_spaces_with_underscores <- function(df) {
  # Replace spaces with underscores in the column names
  colnames(df) <- gsub(" ", "_", colnames(df))
  return(df)
}

replace_hash_with_num <- function(df) {
  # Replace "#" with "Num" in the column names
  colnames(df) <- gsub("#", "Num", colnames(df))
  return(df)
}

capitalize_column_names <- function(df) {
  # Capitalize all letters in the column names
  colnames(df) <- toupper(colnames(df))
  return(df)
}

replace_dob_with_date_of_birth <- function(df) {
  # Replace "DOB" with "DATE_OF_BIRTH" and capitalize all letters
  colnames(df) <- toupper(gsub("DOB", "DATE_OF_BIRTH", colnames(df)))
  return(df)
}

replace_hyphens_with_na <- function(df) {
  # Apply the replacement to every column in the dataframe
  df[] <- lapply(df, function(x) {
    # Replace strings of hyphens of length greater than 2 with NA
    if (is.character(x)) {
      x[x == strrep("-", nchar(x)) & nchar(x) > 2] <- NA
    }
    return(x)
  })
  return(df)
}

convert_date_columns <- function(df, date_columns, date_format = "%m-%d-%Y") {
  # Loop through the list of specified date columns
  for (col in date_columns) {
    # Check if the column exists in the dataframe
    if (col %in% colnames(df)) {
      # Convert the character column to Date type (or POSIXct if datetime)
      df[[col]] <- as.Date(df[[col]], format = date_format)
    } else {
      warning(paste("Column", col, "not found in the dataframe"))
    }
  }
  return(df)
}

clean_excel_column_names <- function(df, replace_with = "_") {
  # Remove '\r' and '\n' characters from column names
  colnames(df) <- gsub("[\r\n]", replace_with, colnames(df))

  return(df)
}

convert_excel_dates <- function(df, date_columns) {
  # Loop through the list of columns
  for (col in date_columns) {
    # Check if the column exists in the dataframe
    if (col %in% colnames(df)) {
      # If the column is numeric (Excel serial date format)
      if (is.numeric(df[[col]])) {
        df[[col]] <- as.Date(df[[col]], origin = "1899-12-30")
      }
      # If the column is character (Excel serial date as string format)
      else if (is.character(df[[col]])) {
        df[[col]] <- as.Date(as.numeric(df[[col]]), origin = "1899-12-30")
      }
      # Format the date to MM/DD/YYYY
      df[[col]] <- format(df[[col]], "%m/%d/%Y")
    } else {
      warning(paste("Column", col, "not found in the dataframe"))
    }
  }
  return(df)
}

replace_double_underscore <- function(df) {
  # Replace double underscores with single underscore in column names
  colnames(df) <- gsub("__", "_", colnames(df))
  return(df)
}

replace_excel_dots_with_underscores <- function(df) {
  # Replace occurrences of . , .. , and ... with underscores in column names
  colnames(df) <- gsub("\\.{1,3}", "_", colnames(df))
  return(df)
}

cut_off_time <- function(df, datetime_columns) {
  for (col in datetime_columns) {
    if (col %in% colnames(df)) {
      # Cut the string before the space (keep the date part only)
      df[[col]] <- sub(" .*$", "", df[[col]])  # This removes everything after the space
    } else {
      warning(paste("Column", col, "not found in the dataframe"))
    }
  }

  return(df)
}
