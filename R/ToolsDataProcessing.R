#' @title Replace Spaces with Underscores in Column Names
#'
#' @description
#' This function takes a data frame and replaces all spaces in the column names
#' with underscores. It modifies the data frame in-place and returns the updated
#' data frame.
#'
#' @param df A data frame whose column names need to be modified.
#'
#' @return A data frame with column names where spaces are replaced by
#' underscores.
#'
#' @examples
#' df <- data.frame("Column One" = 1:3, "Column Two" = 4:6)
#' df_updated <- replace_spaces_with_underscores(df)
#' # df_updated will have columns "Column_One" and "Column_Two"
#'
#' @export
replace_spaces_with_underscores <- function(df) {
  # Replace spaces with underscores in the column names
  colnames(df) <- gsub(" ", "_", colnames(df))
  return(df)
}


#' @title Replace Hashes with 'Num' in Column Names
#'
#' @description
#' This function takes a data frame and replaces all occurrences of the hash
#' symbol (`#`) in the column names
#' with the string "Num". It modifies the data frame in-place and returns the
#' updated data frame.
#'
#' @param df A data frame whose column names need to be modified.
#'
#' @return A data frame with column names where all occurrences of `#` have been
#' replaced by "Num".
#'
#' @examples
#' df <- data.frame("#ID" = 1:3, "Age#" = c(25, 30, 35))
#' df_updated <- replace_hash_with_num(df)
#' # df_updated will have columns "NumID" and "AgeNum"
#'
#' @export
replace_hash_with_num <- function(df) {
  # Replace "#" with "Num" in the column names
  colnames(df) <- gsub("#", "Num", colnames(df))
  return(df)
}


#' @title Capitalize All Column Names
#'
#' @description
#' This function takes a data frame and capitalizes all the letters in the
#' column names.
#' It modifies the column names in-place and returns the updated data frame.
#'
#' @param df A data frame whose column names need to be capitalized.
#'
#' @return A data frame with column names where all letters have been
#' capitalized.
#'
#' @examples
#' df <- data.frame("name" = c("John", "Jane"), "age" = c(30, 25))
#' df_updated <- capitalize_column_names(df)
#' # df_updated will have column names "NAME" and "AGE"
#'
#' @export
capitalize_column_names <- function(df) {
  # Capitalize all letters in the column names
  colnames(df) <- toupper(colnames(df))
  return(df)
}


#' @title Replace "DOB" with "DATE_OF_BIRTH" in Column Names
#'
#' @description
#' This function replaces the string "DOB" with "DATE_OF_BIRTH" in the column
#' names of the input data frame.
#' It also capitalizes all column names to ensure consistency.
#'
#' @param df A data frame whose column names will be modified.
#'
#' @return A data frame with the column names modified such that any "DOB"
#' entries are replaced with "DATE_OF_BIRTH",
#' and all letters are capitalized.
#'
#' @examples
#' df <- data.frame("DOB" = c("1990-01-01", "1985-05-20"),
#'                  "name" = c("John", "Jane"))
#' df_updated <- replace_dob_with_date_of_birth(df)
#' # df_updated will have column names "DATE_OF_BIRTH" and "NAME"
#'
#' @export
replace_dob_with_date_of_birth <- function(df) {
  # Replace "DOB" with "DATE_OF_BIRTH" and capitalize all letters
  colnames(df) <- toupper(gsub("DOB", "DATE_OF_BIRTH", colnames(df)))
  return(df)
}


#' @title Replace Hyphens with NA in Data Frame
#'
#' @description
#' This function searches for cells in the data frame that contain a string of
#' hyphens ("-") of length greater than 2
#' and replaces those occurrences with `NA`. The function applies this
#' replacement to all columns in the data frame
#' where the columns contain character data.
#'
#' @param df A data frame where hyphens in character columns will be replaced
#' by `NA`.
#'
#' @return A data frame with the specified hyphen strings replaced by `NA`.
#'
#' @examples
#' df <- data.frame(col1 = c("abc", "---", "xyz"), col2 = c("123", "--", "---"))
#' df_updated <- replace_hyphens_with_na(df)
#' # df_updated will have `NA` in place of hyphen strings like "---"
#'
#' @export
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


#' @title Convert Date Columns in a Data Frame
#'
#' @description
#' This function converts specified columns in a data frame to Date format,
#' using a provided date format.
#' If the column exists in the data frame, it will be converted using the
#' as.Date` function.
#' If the column is not found, a warning message will be shown.
#'
#' @param df A data frame containing the columns to be converted.
#' @param date_columns A character vector specifying the names of the columns
#' to be converted to Date format.
#' @param date_format A string specifying the date format to be used for
#' conversion (default is "%m-%d-%Y").
#'
#' @return A data frame with the specified columns converted to Date format.
#'
#' @examples
#' df <- data.frame(name = c("Alice", "Bob"),
#'                  date_of_birth = c("01-15-1990", "12-22-1985"),
#'                  join_date = c("03-20-2015", "05-10-2017"),
#'                  stringsAsFactors = FALSE)
#' df_updated <- convert_date_columns(df, date_columns = c("date_of_birth",
#'                                    "join_date"))
#' # The 'date_of_birth' and 'join_date' columns will be converted to Date format.
#'
#' @export
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


#' @title Clean Column Names by Replacing Newline and Carriage Return Characters
#'
#' @description
#' This function cleans the column names of a data frame by removing any newline
#' (`\n`) or carriage return (`\r`) characters.
#' The unwanted characters are replaced with a specified string (default is an
#' underscore `_`).
#'
#' @param df A data frame whose column names will be cleaned.
#' @param replace_with A string to replace newline and carriage return
#' characters with (default is "_").
#'
#' @return A data frame with cleaned column names.
#'
#' @examples
#' df <- data.frame("col1\r" = c(1, 2, 3), "col2\n" = c(4, 5, 6),
#'                  stringsAsFactors = FALSE)
#' df_cleaned <- clean_excel_column_names(df)
#' # The column names of df_cleaned will have "\r" and "\n" replaced by "_".
#'
#' @export
clean_excel_column_names <- function(df, replace_with = "_") {
  # Remove '\r' and '\n' characters from column names
  colnames(df) <- gsub("[\r\n]", replace_with, colnames(df))

  return(df)
}


#' @title Convert Excel Serial Dates to Date Format
#'
#' @description
#' This function converts Excel serial date numbers or character representations
#' of those dates into standard `Date` objects in R. It also formats the dates
#' to the desired `MM/DD/YYYY` format.
#' Excel serial dates are the number of days since `1899-12-30`. If the column
#' contains Excel serial dates as numeric or character data, this function will
#' convert them to proper `Date` format.
#'
#' @param df A data frame containing columns with Excel serial date values.
#' @param date_columns A character vector of column names to be converted to
#' date format.
#'
#' @return A data frame with the specified columns converted to `Date` format
#' (in `MM/DD/YYYY` format).
#'
#' @examples
#' # Sample data with Excel serial date numbers
#' df <- data.frame(Date_Column = c(44204, 44205, 44206),
#'                  stringsAsFactors = FALSE)
#' df_converted <- convert_excel_dates(df, "Date_Column")
#' # The Date_Column will now contain proper Date objects in MM/DD/YYYY format.
#'
#'
#' @export
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


#' @title Replace Double Underscores with Single Underscore in Column Names
#'
#' @description
#' This function replaces any instances of double underscores (`__`) in the
#' column names of a data frame with a single underscore (`_`). This can be
#' useful for cleaning up column names that may have been generated with extra
#' underscores or when preparing data for processing.
#'
#' @param df A data frame whose column names may contain double underscores.
#'
#' @return A data frame with column names modified, where all double underscores
#' have been replaced with a single underscore.
#'
#' @examples
#' # Sample data frame with double underscores in column names
#' df <- data.frame(`col__one` = 1:3, `col__two` = 4:6)
#' df_cleaned <- replace_double_underscore(df)
#' # The column names of df_cleaned will be "col_one" and "col_two"
#'
#' @export
replace_double_underscore <- function(df) {
  # Replace double underscores with single underscore in column names
  colnames(df) <- gsub("__", "_", colnames(df))
  return(df)
}


#' @title Replace Excel Dots with Underscores in Column Names
#'
#' @description
#' This function replaces occurrences of one, two, or three consecutive dots
#' (`.`, `..`, `...`) with underscores (`_`) in the column names of a data
#' frame. This is useful when cleaning up column names extracted from Excel,
#' which often use dots to separate parts of a column name.
#'
#' @param df A data frame whose column names may contain dots (`.`, `..`, `...`).
#'
#' @return A data frame with column names modified, where any occurrence of one,
#' two, or three consecutive dots has been replaced by a single underscore.
#'
#' @examples
#' # Sample data frame with dots in column names
#' df <- data.frame(`col.one` = 1:3, `col..two` = 4:6, `col...three` = 7:9)
#' df_cleaned <- replace_excel_dots_with_underscores(df)
#' # The column names of df_cleaned will be "col_one", "col_two", and "col_three"
#'
#' @export
replace_excel_dots_with_underscores <- function(df) {
  # Replace occurrences of . , .. , and ... with underscores in column names
  colnames(df) <- gsub("\\.{1,3}", "_", colnames(df))
  return(df)
}


#' @title Cut Off Time from DateTime Columns
#'
#' @description
#' This function processes specified datetime columns in a data frame and truncates
#' the time portion, keeping only the date. It removes everything after the space
#' (which separates the date and time) for each specified column.
#'
#' @param df A data frame containing the datetime columns to be processed.
#' @param datetime_columns A character vector of column names in `df` that are datetime columns.
#' The function will truncate the time from these columns, keeping only the date part.
#'
#' @return A data frame with the specified datetime columns modified, where the time part is removed
#' and only the date is kept.
#'
#' @examples
#' # Sample data frame with datetime columns
#' df <- data.frame(
#'   ID = 1:3,
#'   created_at = c("2024-12-01 12:34:56", "2024-12-02 15:22:33", "2024-12-03 08:45:01"),
#'   updated_at = c("2024-12-01 12:34:56", "2024-12-02 15:22:33", "2024-12-03 08:45:01"),
#'   stringsAsFactors = FALSE
#' )
#'
#' # Apply the cut_off_time function to truncate time part from 'created_at' and 'updated_at'
#' df_cleaned <- cut_off_time(df, c("created_at", "updated_at"))
#'
#' # The resulting df_cleaned will have only the date part in 'created_at' and 'updated_at'
#'
#' @export
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
