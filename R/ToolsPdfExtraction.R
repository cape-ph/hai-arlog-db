utils::globalVariables(c("VALUES_TO_KEEP",
                         "CULTURE_ORGANISM_LIST",
                         "COLUMN_NAMES"))

# Global variable for the list of column names to be used across the package
#' @name VALUES_TO_KEEP
#' @title A list of columns to keep from the first page of the Tenn ARLN pdfs.
#' @examples
#' VALUES_TO_KEEP <- c("Patient Name",
#'                     "Date of Birth",
#'                     "Specimen Type",
#'                     "Date Collected",
#'                     "Date Received",
#'                     "Date Reported")
#' @export
VALUES_TO_KEEP <- c("Patient Name",
                    "Date of Birth",
                    "Specimen Type",
                    "Date Collected",
                    "Date Received",
                    "Date Reported")

#' @name CULTURE_ORGANISM_LIST
#' @title A list of Results used for data selection from Tenn ARLN pdfs.
#' @examples
#' CULTURE_ORGANISM_LIST <- c("RESULT:Carbapenem Resistant Organism Culture",
#'                            "RESULT:Mycology Culture")
#' @export
CULTURE_ORGANISM_LIST <- c("RESULT:Carbapenem Resistant Organism Culture",
                           "RESULT:Mycology Culture")

#' @name COLUMN_NAMES
#' @title A list of column names used for data selection from Tenn ARLN pdfs.
#' @examples
#' COLUMN_NAMES <- c("Results",
#'                   "Reference Range",
#'                   "Performing Location")
#' @export
COLUMN_NAMES <- c("Results", "Reference Range", "Performing Location")


#' @title Extract lines between two words from a list of lines.
#' @description
#' In the TN Department of Health PDFs, exporting the data requires reading the
#' pdf and extracting different segments at a time. This function gives a
#' selection of lines from a list of lines based on a start and stop word.
#'
#' @param lines The lines extracted from reading the pdf or other document.
#' @param start_string The word that starts the line selection, inclusive.
#' @param end_string The word that ends the line selection, not inclusive.
#'
#' @return The lines between start and stop words, including start line and
#' exlcuding the stop line.
#' @importFrom pdftools pdf_text
#' @importFrom utils tail
#' @export
#'
#' @examples
#' # Use system.file to get the path to the example document
#'   pdf_file_path <- system.file("extdata",
#'                                "pdf_tnarln_example_1.pdf",
#'                                 package = "arlog")
#'
#' # Extract the lines from the pdf
#'   pdf <- pdftools::pdf_text(pdf_file_path)
#' # Convert the list of list of lists into a single list where each item is a
#' # line from the document.
#'   lines <- unlist(strsplit(pdf, "\n"))
#'   result <- pdf_extract_lines_between(lines, "Accession", "Key")
#'   print(result)
pdf_extract_lines_between <- function(lines, start_string, end_string) {
  # Find indices of the lines containing the start and end strings
  start_index <- grep(start_string, lines)
  end_index <- grep(end_string, lines)

  # Check if both indices were found
  if (length(start_index) == 0 || length(end_index) == 0) {
    stop("Start or end string not found in the lines.")
  }

  # Get the first occurrence of the start string and the last occurrence of the
  # end string
  start <- start_index[1]
  end <- tail(end_index, 1)

  # Extract lines including the start line and excluding the end line
  if (start < end) {
    return(lines[start:(end - 1)])  # Return lines from start to just before end
  } else {
    return(character(0))  # Return an empty character vector if start is after end
  }
}


#' @title Split lines into columns when spacing between words is larger than 9
#' @description
#' The Tennessee Dept of Health PDFs feature a Preliminary report section. The
#' section is divided visually into three columns. This function takes the lines
#' extracted from the document and breaks the lines into three columns based on
#' the distance between the words, when there are more than 9 spaces between
#' words, that marks a column break.
#'
#' @param lines The lines extracted from a Tenn ARLN pdf between the words
#' "Accession" and "Key".
#'
#' @return A data frame with three columns, Column1, Column2, Column3.
#' @export
#'
#' @examples
#' # Use system.file to get the path to the example document
#'   pdf_file_path <- system.file("extdata",
#'                                "pdf_tnarln_example_1.pdf",
#'                                 package = "arlog")
#'
#' # Extract the lines from the pdf
#'   pdf <- pdftools::pdf_text(pdf_file_path)
#' # Convert the list of list of lists into a single list where each item is a
#' # line from the document.
#'   lines <- unlist(strsplit(pdf, "\n"))
#'   result <- pdf_extract_lines_between(lines, "Accession", "Key")
#' # Convert the selected lines into a dataframe of three columns.
#'   result_df <- pdf_split_lines_into_columns(result)
#'   print(result_df)
pdf_split_lines_into_columns <- function(lines) {
  # Define a regex pattern for matching more than 9 spaces
  pattern <- "\\s{9,}"

  # Split each line using the defined pattern
  split_columns <- lapply(lines, function(line) {
    # Split the line based on the pattern
    parts <- unlist(strsplit(line, pattern))

    # Return only the first three parts or NA if there are less than three
    length(parts) <- max(3, length(parts))  # Ensure it has at least 3 elements
    return(parts)
  })

  # Convert the list of columns to a data frame
  df <- do.call(rbind, split_columns)

  # Convert to a data frame and set appropriate column names
  colnames(df) <- c("Column1", "Column2", "Column3")
  return(as.data.frame(df, stringsAsFactors = FALSE))
}


#' @title Extract Key-Value Pair from String
#' @description
#' Function to extract keys and values from a colon-separated string. This
#' function is intended to be used by other functions and is not called
#' directly on dataframes. It splits a string at the first occurrence of
#' `": "` and returns a list containing the key and value. If the input
#' string is not in the expected format or is missing, it returns `NULL` or
#' a list with a key and `NA` for the value.
#'
#' @param cell A string representing a key-value pair in the format
#'             "key: value".
#'             If the string is empty or contains "<NA>", the function
#'             returns `NULL`.
#'
#' @return A list with two elements:
#'   \item{key}{The part before the colon (":") as the key.}
#'   \item{value}{The part after the colon as the value. If no value is
#'   found, `NA` is returned.}
#'   If the input is invalid (i.e., `NA` or "<NA>"), the function
#'   returns `NULL`.
#' @importFrom stringr str_split
#' @export
#'
#' @examples
#' extract_key_value("name: John Doe")
#' # Returns: list(key = "name", value = "John Doe")
#'
#' extract_key_value("<NA>")
#' # Returns: NULL
#'
#' extract_key_value("age: ")
#' # Returns: list(key = "age", value = NA)
#'
extract_key_value <- function(cell) {
  if (is.na(cell) || cell == "<NA>") {
    return(NULL)
  }
  parts <- stringr::str_split(cell, ": ", simplify = TRUE)
  return(list(key = parts[1],
              value = ifelse(length(parts) > 1,
                             parts[2], NA)))
}


#' @title Extract Key-Value Pairs from Dataframe
#' @description
#' This function takes a dataframe as input and extracts key-value pairs
#' from each cell in the dataframe. The function loops through each column
#' and each row, calling `extract_key_value` to extract the key-value pair
#' from the cell. The result is returned as a new dataframe with two columns:
#' `Keys` and `Values`, containing the extracted key-value pairs.
#'
#' This function is useful for dataframes where each cell contains key-value
#' pairs in a specific format (e.g., "key: value") that needs to be parsed
#' and structured in a more accessible form.
#'
#' @param data A dataframe where each cell may contain a key-value pair in the
#'             format "key: value". The function will extract and return these
#'             pairs for each cell in the dataframe.
#'
#' @return A dataframe with two columns:
#'   \item{Keys}{The extracted keys from the input dataframe's cells.}
#'   \item{Values}{The extracted values corresponding to the keys. If no
#'                 valid key-value pair is found, those rows will not be included.}
#'
#' @importFrom stringr str_split
#' @export
#'
#' @examples
#' # Example 1: Tenn ARLN pdf
#' # Use system.file to get the path to the example document
#'   pdf_file_path <- system.file("extdata",
#'                                "pdf_tnarln_example_1.pdf",
#'                                 package = "arlog")
#'
#' # Extract the lines from the pdf
#'   pdf <- pdftools::pdf_text(pdf_file_path)
#' # Convert the list of list of lists into a single list where each item is a
#' # line from the document.
#'   lines <- unlist(strsplit(pdf, "\n"))
#'   result <- pdf_extract_lines_between(lines, "Accession", "Key")
#' # Convert the selected lines into a dataframe of three columns.
#'   result_df <- pdf_split_lines_into_columns(result)
#' # Extract the three columns from the dataframe.
#'   extract_table <- extract_from_dataframe(result_df)
#'   print(extract_table)
#'
#' # Example 2: A dataframe with empty or missing cells
#' data2 <- data.frame(
#'   col1 = c("name: Alice", "", "city: Paris"),
#'   col2 = c("age: 30", NA, "country: France"),
#'   stringsAsFactors = FALSE
#' )
#' extract_from_dataframe(data2)
#' # Returns a dataframe with extracted keys and values:
#' # Keys      Values
#' # name      Alice
#' # age       30
#' # city      Paris
#' # country   France
#'
extract_from_dataframe <- function(data) {
  # Create an empty data frame for results
  results <- data.frame(Keys = character(),
                        Values = character(),
                        stringsAsFactors = FALSE)

  # Loop through each column and extract keys and values
  for (col in names(data)) {
    for (item in data[[col]]) {
      extracted <- extract_key_value(item)
      if (!is.null(extracted)) {
        results <- rbind(results,
                         data.frame(Keys = extracted$key,
                                    Values = extracted$value,
                                    stringsAsFactors = FALSE))
      }
    }
  }

  return(results)
}


#' @title Select Rows by Column Values from a List
#' @description
#' This function filters a dataframe to select rows where the values in a
#' specified column match any of the values in a provided list. The function
#' uses the `dplyr::filter` function and the `%in%` operator to filter rows
#' based on the given list of values. It is useful for subsetting a dataframe
#' based on a  specific set of values from a column.
#'
#' @param data A dataframe to filter.
#' @param col_name A string representing the column name to filter by.
#' @param value_list A vector of values. The function will return all rows where
#'                  the specified column contains a value from this list.
#'
#' @return A filtered dataframe that includes only the rows where the value in the
#'         specified column matches one of the values from the provided list.
#'
#' @importFrom dplyr filter
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' # Example 1: Tenn ARLN pdf
#' # Use system.file to get the path to the example document
#'   pdf_file_path <- system.file("extdata",
#'                                "pdf_tnarln_example_1.pdf",
#'                                 package = "arlog")
#'
#' # Extract the lines from the pdf
#'   pdf <- pdftools::pdf_text(pdf_file_path)
#' # Convert the list of list of lists into a single list where each item is a
#' # line from the document.
#'   lines <- unlist(strsplit(pdf, "\n"))
#'   result <- pdf_extract_lines_between(lines, "Accession", "Key")
#' # Convert the selected lines into a dataframe of three columns.
#'   result_df <- pdf_split_lines_into_columns(result)
#' # Extract the three columns from the dataframe.
#'   extract_table <- extract_from_dataframe(result_df)
#'   selection <- select_rows_by_list(extract_table, "Keys", VALUES_TO_KEEP)
#'   print(extract_table)
#'
#' # Example 2: Filter rows where 'col2' contains values 3 or 5
#' data2 <- data.frame(
#'   col1 = c("E", "F", "G", "H", "I"),
#'   col2 = c(3, 4, 5, 6, 7),
#'   stringsAsFactors = FALSE
#' )
#' select_rows_by_list(data2, "col2", c(3, 5))
#' # Returns a dataframe with rows where col2 is 3 or 5:
#' # col1 col2
#' # 1   E    3
#' # 3   G    5
#'
select_rows_by_list <- function(data, col_name, value_list) {
  data %>%
    dplyr::filter(.data[[col_name]] %in% value_list)
}


#' @title Extract Accession Information from Text
#' @description
#' Some of the values in the columns cannot be extracted by using the key-value
#' matching form the previous function. Accession # is one of those values.
#' This function extracts the accession number information from a list of lines
#' of text.It looks for a line containing the phrase "Accession #" and splits
#' the line into a key-value pair, where the key is "Accession #" and the value
#' is the corresponding accession number.
#'
#' @param lines A character vector (list of lines) containing the text from which
#'              to extract the accession information.
#'
#' @return A data frame with two columns:
#'   - `Keys`: A character string representing the key "Accession #".
#'   - `Values`: A character string representing the accession number (value).
#'
#' @details
#' The function searches through the input text and looks for a line containing
#' the phrase "Accession #". If the line is found, it splits the line at the
#' colon (`:`) to separate the key from the value. The function returns a data
#' frame with these key-value pairs. If no line containing "Accession #" is
#' found, the function throws an error.
#'
#' @examples
#' # Example 1: Tenn ARLN pdf
#' # Use system.file to get the path to the example document
#'   pdf_file_path <- system.file("extdata",
#'                                "pdf_tnarln_example_1.pdf",
#'                                 package = "arlog")
#'
#' # Extract the lines from the pdf
#'   pdf <- pdftools::pdf_text(pdf_file_path)
#' # Convert the list of list of lists into a single list where each item is a
#' # line from the document.
#'   lines <- unlist(strsplit(pdf, "\n"))
#'   result <- pdf_extract_lines_between(lines, "Accession", "Key")
#'   accession_num <- extract_accession_info(result)
#'   print(accession_num)
#'
#' # Example 2: Extract accession information from a sample text
#' lines <- c("Header", "Accession #: ABC12345", "Footer")
#' result <- extract_accession_info(lines)
#' print(result)
#'
#' # Example: Error case when "Accession #" is not found
#' lines <- c("Header", "Footer")
#' # This will throw an error:
#' # extract_accession_info(lines)
#'
#' @export
extract_accession_info <- function(lines) {
  # Find the line containing "Accession #"
  indices <- grep("Accession #", lines)

  # Check if any such line is found
  if (length(indices) == 0) {
    stop("No line containing 'Accession #' found.")
  }

  # Extract the relevant line
  accession_line <- lines[indices]

  # Split the line on ":"
  split_line <- strsplit(accession_line, ":")[[1]]

  # Create a data frame with Keys and Values
  accession_data <- data.frame(
    Keys = trimws(split_line[1]),       # The key (Accession #)
    Values = trimws(split_line[2]),     # The value (remaining part)
    stringsAsFactors = FALSE
  )

  return(accession_data)
}


#' @title Extract Facility Data from a Data Frame
#' @description
#' This function extracts information related to the "Facility" from a specific
#' column of a data frame. It looks for the cell that contains the text
#' "Facility:", and if it finds it, retrieves the two subsequent cells in the
#' column. These values are concatenated into a single string, and the function
#' returns the result in a data frame. If the "Facility:" entry is not found,
#' or if there are not enough rows below it, the function returns a data frame
#' with `NA` values.
#'
#' @param data A data frame that contains the data to search.
#' @param col_name The name of the column (as a string) to search for the
#' "Facility:" entry.
#'
#' @return A data frame with two columns:
#'   - `Keys`: The string `"Facility"`.
#'   - `Values`: A concatenated string of the two subsequent cells below the
#'   "Facility:" entry in the specified column. If no "Facility:" entry is found
#'   or there are not enough rows, both columns will contain `NA`.
#'
#' @details
#' The function searches for the row in the specified column that contains the
#' string "Facility:".
#' It then checks if there are at least two rows below this entry and
#' concatenates the values of the two cells directly below "Facility:". The
#' concatenated result is returned in a data frame with "Facility" as the key.
#' If "Facility:" is not found or if there are fewer than two rows below it, the
#' function returns a data frame with `NA` values.
#'
#' @examples
#' # Example 1: Tenn ARLN pdf
#' # Use system.file to get the path to the example document
#'   pdf_file_path <- system.file("extdata",
#'                                "pdf_tnarln_example_1.pdf",
#'                                 package = "arlog")
#'
#' # Extract the lines from the pdf
#'   pdf <- pdftools::pdf_text(pdf_file_path)
#' # Convert the list of list of lists into a single list where each item is a
#' # line from the document.
#'   lines <- unlist(strsplit(pdf, "\n"))
#'   result <- pdf_extract_lines_between(lines, "Accession", "Key")
#'   result_df <- pdf_split_lines_into_columns(result)
#'   extract_table <- extract_from_dataframe(result_df)
#'   pdf_facility <- get_facility_data(extract_table, "Keys")
#'   print(pdf_facility)
#' # Example 2: Extract Facility data from a data frame
#' data <- data.frame(Column1 = c("Header", "Facility:", "Building A",
#'                                "Room 101", "Footer"))
#' result <- get_facility_data(data, "Column1")
#' print(result)
#' # Should return a data frame with "Facility" as the key and
#' # "Building A Room 101" as the value
#' @export
get_facility_data <- function(data, col_name) {
  # Find the index of the "Facility" cell
  facility_index <- which(data[[col_name]] == "Facility:")

  # Check if "Facility" was found and there are at least two cells below
  if (length(facility_index) > 0 && facility_index + 2 <= nrow(data)) {
    # Get the two cells below "Facility"
    data_below <- data[(facility_index + 1):(facility_index + 2), col_name]

    # Concatenate the cells with a space
    concatenated_string <- paste(data_below, collapse = " ")

    # Return the result as a data frame
    return(data.frame(Keys = "Facility", Values = concatenated_string,
                      stringsAsFactors = FALSE))
  } else {
    return(data.frame(Keys = NA, Values = NA, stringsAsFactors = FALSE))
  }
}



#' @title Search for Culture Organism in Lines
#' @description
#' This function searches through a list of lines (e.g., text lines) and returns
#' the lines that contain any of the organisms specified in the provided list
#' `CULTURE_ORGANISM_LIST`. It performs a case-insensitive search for the
#' organisms within each line.
#'
#' @param lines A character vector containing lines of text to search through.
#' @param CULTURE_ORGANISM_LIST A character vector containing the names of
#'        organisms to search for within the lines.
#'
#' @return A list of lines from the input `lines` that contain any of the
#'         organisms in `CULTURE_ORGANISM_LIST`.
#'
#' @examples
#' # Example 1: Tenn ARLN pdf
#' # Use system.file to get the path to the example document
#'   pdf_file_path <- system.file("extdata",
#'                                "pdf_tnarln_example_1.pdf",
#'                                 package = "arlog")
#' # Extract the lines from the pdf
#'   pdf <- pdftools::pdf_text(pdf_file_path)
#' # Convert the list of list of lists into a single list where each item is a
#' # line from the document.
#'   lines <- unlist(strsplit(pdf, "\n"))
#'   pdf_organism_culture <- search_for_culture_organism(lines,
#'                                                       CULTURE_ORGANISM_LIST)
#'   print(pdf_organism_culture)
#' # Example 2:
#' lines <- c("The culture of E. coli was successful.",
#'            "Yeast culture did not grow.",
#'            "Salmonella growth observed.")
#' CULTURE_ORGANISM_LIST <- c("E. coli", "Yeast", "Salmonella")
#' search_for_culture_organism(lines, CULTURE_ORGANISM_LIST)
#'
#' @export
search_for_culture_organism <- function(lines, CULTURE_ORGANISM_LIST) {
  # Initialize a list to store matching lines
  matching_lines <- list()

  # Loop through each line
  for (line in lines) {
    # Check if any organism from the list is present in the line
    if (any(grepl(paste(CULTURE_ORGANISM_LIST, collapse = "|"), line,
                  ignore.case = TRUE))) {
      matching_lines <- append(matching_lines, line)
    }
  }

  return(matching_lines)
}


#' @title Create Event Dataframe from Lines
#'
#' @description
#' This function takes a vector of strings where each string is expected to
#' contain a key-value pair separated by a colon (`:`).
#' It processes each line, trims whitespace from the key and value, and
#' returns a data frame with two columns: "Keys" and "Values".
#'
#' @param lines A character vector where each element is a string containing a
#' key-value pair separated by a colon (`:`).
#'
#' @return A data frame with two columns:
#'   \describe{
#'     \item{Keys}{A character vector containing the keys extracted
#'     from the input lines.}
#'     \item{Values}{A character vector containing the values extracted
#'     from the input lines.}
#'   }
#'
#' @examples
#' # Example 1: Tenn ARLN pdf
#' # Extract the lines from the pdf
#' # Use system.file to get the path to the example document
#'   pdf_file_path <- system.file("extdata",
#'                                "pdf_tnarln_example_1.pdf",
#'                                 package = "arlog")
#'   pdf <- pdftools::pdf_text(pdf_file_path)
#' # Convert the list of list of lists into a single list where each item is a
#' # line from the document.
#'   lines <- unlist(strsplit(pdf, "\n"))
#'   pdf_organism_culture <- search_for_culture_organism(lines,
#'                                                       CULTURE_ORGANISM_LIST)
#'   pdf_orgnsm_rslt <- create_event_dataframe_from_lines(pdf_organism_culture)
#'   print(pdf_orgnsm_rslt)
#' # Example 2:
#' lines <- c("Name: John Doe", "Age: 30", "Location: New York")
#' create_event_dataframe_from_lines(lines)
#'
#' @export
create_event_dataframe_from_lines <- function(lines) {
  # Initialize lists to store keys and values
  keys <- c()
  values <- c()

  # Loop through each line
  for (line in lines) {
    # Split the line on the colon
    parts <- unlist(strsplit(line, ":"))

    # Check if there are exactly 2 parts
    if (length(parts) == 2) {
      keys <- c(keys, trimws(parts[1]))   # Add the key (trimmed)
      values <- c(values, trimws(parts[2])) # Add the value (trimmed)
    }
  }

  # Create a data frame
  result_df <- data.frame(Keys = keys, Values = values,
                          stringsAsFactors = FALSE)

  return(result_df)
}


#' @title Extract Tables from Lines
#'
#' @description
#' This function processes a character vector `lines`, extracting tables based
#' on certain markers. It identifies sections of text that start with a line
#' containing `"-PCR"` and ends with a line containing either `"Performing"` or
#' `"Comments"`. The function collects these lines into separate tables, which
#' are stored as a list of character vectors.
#'
#' @param lines A character vector where each element is a line of text. The
#' function will extract tables based on the presence of specific keywords such
#' as `"-PCR"`, `"Performing"`, and `"Comments"`.
#'
#' @return A list of character vectors. Each element of the list represents a
#' table, which is a sequence of lines collected between a `"-PCR"` and a line
#' containing `"Performing"` or `"Comments"`.
#'
#' @importFrom stringr str_detect
#'
#' @examples
#' # Example 1: Tenn ARLN pdf
#' # Extract the lines from the pdf
#' # Use system.file to get the path to the example document
#'   pdf_file_path <- system.file("extdata",
#'                                "pdf_tnarln_example_1.pdf",
#'                                 package = "arlog")
#'   pdf <- pdftools::pdf_text(pdf_file_path)
#' # Convert the list of list of lists into a single list where each item is a
#' # line from the document.
#'   lines <- unlist(strsplit(pdf, "\n"))
#'   tables <- extract_tables_from_lines(lines)
#'   print(tables)
#' # Example 2
#' lines <- c("Start of table 1", "-PCR", "Line 1 of table 1",
#'            "Line 2 of table 1", "Performing", "Start of table 2",
#'            "-PCR", "Line 1 of table 2", "Comments")
#' extract_tables_from_lines(lines)
#' @export
extract_tables_from_lines <- function(lines) {
  # Initialize variables
  tables <- list()
  current_table <- NULL
  collecting <- FALSE

  for (line in lines) {
    # Check if the line contains "-PCR"
    if (str_detect(line, "-PCR")) {
      # If currently collecting, save the current table
      if (collecting) {
        tables <- append(tables, list(current_table))
      }
      # Start a new table
      current_table <- c()
      collecting <- TRUE
    }

    # Check if we're currently collecting a table
    if (collecting) {
      # Check if the line contains "Performing" or "Comments"
      if (str_detect(line, "Performing") || str_detect(line, "Comments")) {
        collecting <- FALSE
        # Save the current table (if not empty)
        if (length(current_table) > 0) {
          tables <- append(tables, list(current_table))
        }
        current_table <- NULL  # Reset for future tables
      } else {
        # If it's between "PCR" and "Performing" or "Comments", add the line
        # to the current table
        current_table <- c(current_table, line)
      }
    }
  }

  return(tables)
}


#' @title Drop Empty Rows from Tables
#'
#' @description
#' This function processes a list of tables (each table being a character
#' vector) and removes any rows that are empty or contain only whitespace.
#' It returns a cleaned list of tables, where each table has no empty rows.
#'
#' @param tables A list of character vectors. Each element in the list
#' represents a table, and each table is a sequence of lines
#' (character strings).
#'
#' @return A list of character vectors. Each element of the list is a cleaned
#' table with empty or whitespace-only rows removed.
#'
#' @examples
#' tables <- list(c("Row 1", "   ", "Row 3"), c("Row A", " ", "Row C"))
#' drop_empty_rows(tables)
#'
#' @export
drop_empty_rows <- function(tables) {
  cleaned_tables <- lapply(tables, function(table) {
    # Remove rows that are empty (only whitespace)
    table[trimws(table) != ""]
  })
  return(cleaned_tables)
}


#' @title Add First Line as Column Name
#'
#' @description
#' This function extracts the first line from a specified table in a list of
#' tables, processes it by extracting the substring after `"-PCR"` and
#' replacing any occurrence of `ß` with `"beta"`. The processed first line is
#' then added to the existing `COLUMN_NAMES`, modifying the subsequent column
#' names to incorporate the first line.
#'
#' @param tables A list of character vectors, where each element represents a
#' table. Each table is a sequence of lines, and the first line of the specified
#' table is used to generate new column names.
#'
#' @param table_index An integer specifying the index of the table in the
#' `tables` list. The function extracts and processes the first line of this
#' table.
#'
#' @return A character vector of updated column names:
#' - The first element is the processed first line of the table (extracted
#' after the `"-PCR"` substring and with `ß` replaced by `"beta"`).
#' - The remaining elements are the modified `COLUMN_NAMES`, where each column
#' name is updated by prepending the processed first line.
#'
#' @examples
#' tables <- list(c("Sample -PCR data", "Line 2", "Line 3"),
#'                c("Another -PCR example", "Line A", "Line B"))
#' COLUMN_NAMES <- c("Column1", "Column2", "Column3")
#' updated_columns <- add_first_line_as_column_name(tables, 1)
#' print(updated_columns)
#' @export
add_first_line_as_column_name <- function(tables, table_index) {
  # Check if the specified table index is valid
  if (table_index <= length(tables) && table_index > 0) {
    # Get the first line of the specified table
    first_line <- tables[[table_index]][1]


    # Extract the substring after "-PCR"
    # Get everything after "-PCR"
    if (grepl("-PCR", first_line)) {
      first_line <- sub(".*-PCR\\s*", "", first_line)
    } else {
      stop("The first line does not contain '-PCR'.")
    }

    # Replace "beta" in the first line if it exists
    if (grepl("\u00DF", first_line)) {
      first_line <- gsub("\u00DF", "beta", first_line)
    }

    # Add the first line to the column names
    new_column_names <- c(first_line, COLUMN_NAMES)

    # Modify the rest of the column names
    new_column_names[-1] <- paste(first_line, new_column_names[-1])


    return(new_column_names)  # Return the updated column names
  } else {
    stop("Invalid table index.")
  }
}


#' @title Parse Lines into a Data Frame with Fixed Columns
#'
#' @description
#' This function takes a vector of lines, processes each line by trimming
#' leading and trailing whitespace, and splits each line into exactly four
#' parts based on the occurrence of five or more spaces. If a line contains
#' fewer than four parts, the function will pad the missing parts with `NA`.
#' The processed lines are then stored as rows in a data frame with four
#' columns (`Column1`, `Column2`, `Column3`, `Column4`).
#'
#' @param lines A character vector where each element is a line of text. Each
#' line will be split into four parts, and missing parts will be filled with
#' `NA`.
#'
#' @return A data frame with four columns
#' (`Column1`, `Column2`, `Column3`, `Column4`). Each row corresponds to a
#' processed line from the input.
#'
#' @examples
#' lines <- c("part1     part2     part3     part4",
#'            "part1     part2",
#'            "part1     part2     part3     part4")
#' result_df <- parse_lines_to_dataframe(lines)
#' print(result_df)
#' @export
parse_lines_to_dataframe <- function(lines) {
  # Initialize a list to store rows
  rows <- list()

  for (line in lines) {
    # Trim leading/trailing white space and split by 5 or more spaces
    parts <- unlist(strsplit(trimws(line), "\\s{5,}"))

    # Ensure exactly 4 parts; fill with NA if necessary
    if (length(parts) < 4) {
      parts <- c(parts, rep(NA, 4 - length(parts)))
    } else {
      parts <- parts[1:4]  # Keep only the first four parts
    }

    # Add the parts as a new row in the list
    rows[[length(rows) + 1]] <- parts
  }

  # Convert the list of rows to a data frame
  result_df <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)

  # Set column names explicitly
  colnames(result_df) <- c("Column1", "Column2", "Column3", "Column4")

  return(result_df)
}


#' @title Rename Data Frame Columns
#'
#' @description
#' This function renames the columns of a data frame by setting the column
#' names to the values provided in the `column_names` vector. It checks if the
#' length of `column_names` matches the number of columns in the data frame.
#' If the lengths do not match, an error is raised.
#'
#' @param df A data frame whose columns will be renamed.
#'
#' @param column_names A character vector of new column names. The length of
#' this vector must match the number of columns in the `df`.
#'
#' @return The data frame with updated column names.
#'
#' @examples
#' df <- data.frame(A = 1:3, B = 4:6)
#' new_column_names <- c("Column1", "Column2")
#' df_renamed <- rename_dataframe_columns(df, new_column_names)
#' print(df_renamed)
#'
#' @export
rename_dataframe_columns <- function(df, column_names) {
  # Check if the length of column_names matches the
  # number of columns in the data frame
  if (length(column_names) != ncol(df)) {
    stop("Length of column_names must match the number of dataframe columns.")
  }

  # Set the new column names
  colnames(df) <- column_names
  return(df)
}


#' @title Remove Lines Before a Target String
#'
#' @description
#' This function removes all lines from the beginning of a text vector up to
#' and including the first occurrence of a specified target string. It returns
#' the lines starting from the target string onward. If the target string is
#' not found, the function returns the original list of lines unchanged.
#'
#' @param lines A character vector containing the lines of text.
#'
#' @param target_string A character string that serves as the target. The
#' function removes all lines before (and including) the first occurrence of
#' this string.
#'
#' @return A character vector containing the lines starting from the first
#' occurrence of the target string onward.
#'
#' @examples
#' lines <- c("Line 1", "Line 2", "Target string", "Line 3", "Line 4")
#' result <- remove_lines_before_string(lines, "Target string")
#' # result will be c("Target string", "Line 3", "Line 4")
#' print(result)
#'
#' @export
remove_lines_before_string <- function(lines, target_string) {
  # Find the index of the first occurrence of the target string
  index <- which(grepl(target_string, lines))

  # If the target string is found, return the lines from that index onward
  if (length(index) > 0) {
    return(lines[index:length(lines)])
  } else {
    return(lines)  # If not found, return the original list
  }
}


#' @title Remove Empty Lines
#'
#' @description
#' This function removes empty or whitespace-only lines from a character vector.
#' It trims the whitespace from each line and then filters out any lines that
#' are empty. The function returns a vector of lines with all non-empty content.
#'
#' @param lines A character vector containing the lines of text.
#'
#' @return A character vector containing only the non-empty lines from the input.
#'
#' @examples
#' lines <- c("Line 1", "  ", "Line 2", "", "Line 3")
#' result <- remove_empty_lines(lines)
#' # result will be c("Line 1", "Line 2", "Line 3")
#' print(result)
#'
#' @export
remove_empty_lines <- function(lines) {
  # Filter out empty lines
  return(lines[nzchar(trimws(lines))])
}


#' @title Add Spaces to the Beginning of Lines
#'
#' @description
#' This function adds three spaces to the beginning of each line in a character
#' vector. It processes each line by prefixing it with three spaces and returns
#' a modified character vector with the updated lines.
#'
#' @param lines A character vector containing the lines of text to be modified.
#'
#' @return A character vector with three spaces added to the beginning of each
#' line in the input vector.
#'
#' @examples
#' lines <- c("Line 1", "Line 2", "Line 3")
#' result <- add_spaces_to_lines(lines)
#' # result will be c("   Line 1", "   Line 2", "   Line 3")
#' print(result)
#'
#' @export
add_spaces_to_lines <- function(lines) {
  # Add three spaces to the beginning of each line
  modified_lines <- paste0("   ", lines)

  return(modified_lines)  # Return the modified list of lines
}


#' @title Create Data Frame from Lines of Text
#'
#' @description
#' This function takes a character vector of lines, removes any empty lines,
#' splits each line into columns based on 2 or more spaces, and converts the
#' result into a data frame. The first row of the parsed lines is used as the
#' column names. The function ensures that all rows have the same number of
#' columns by padding with `NA` if necessary.
#'
#' @param lines A character vector where each element is a line of text to be
#' processed. Each line is expected to have columns separated by two or more
#' spaces.
#'
#' @return A data frame where the first row is used as the column names, and
#' the remaining rows are the data.
#'
#' @examples
#' lines <- c("Column1   Column2   Column3", "A   B   C", "D   E   F")
#' df <- create_dataframe_from_lines(lines)
#' # df will be a data frame with the first row as column names and the rest
#' # as data
#' print(df)
#'
#' @export
create_dataframe_from_lines <- function(lines) {
  # Remove empty lines
  lines <- lines[nzchar(trimws(lines))]

  # Split each line by 4 or more spaces
  parsed_lines <- strsplit(lines, "\\s{2,}")

  # Convert the list of parsed lines into a data frame
  df <- do.call(rbind, lapply(parsed_lines, function(x) {
    # Ensure each parsed line has the same number of columns
    length(x) <- max(lengths(parsed_lines))
    return(x)
  }))

  # Convert to data frame
  df <- as.data.frame(df, stringsAsFactors = FALSE)

  # Set the first row as column names
  colnames(df) <- df[1, ]

  # Remove the first row used as column names
  df <- df[-1, , drop = FALSE]

  return(df)
}


#' @title Add String to the First Line
#'
#' @description
#' This function adds a specified string to the beginning of the first line of
#' a character vector. If the input vector is not empty, the function prepends
#' the provided string to the first element and returns the modified vector.
#' If the vector is empty, it returns the vector unchanged.
#'
#' @param lines A character vector where the string will be added to the first
#' line.
#'
#' @param string_to_add A character string that will be prepended to the first
#' line of the input vector.
#'
#' @return A character vector where the first line has the specified string
#' added to the beginning.
#'
#' @examples
#' lines <- c("First line", "Second line", "Third line")
#' result <- add_string_to_first_line(lines, "New Start: ")
#' # result will be c("New Start: First line", "Second line", "Third line")
#' print(result)
#'
#' @export
add_string_to_first_line <- function(lines, string_to_add) {
  # Check if the list is not empty
  if (length(lines) > 0) {
    # Add the string to the beginning of the first line
    lines[1] <- paste0(string_to_add, lines[1])
  }

  return(lines)  # Return the modified list of lines
}


#' @title Trim Leading and Trailing Whitespace in Data Frame
#'
#' @description
#' This function trims leading and trailing whitespace from all character
#' columns in a data frame. It applies `trimws()` to each column of the data
#' frame, but only to those that are of character type. Non-character columns
#' (e.g., numeric, logical) are left unchanged.
#'
#' @param df A data frame where the whitespace will be trimmed from character
#' columns.
#'
#' @return A data frame with leading and trailing whitespace removed from all
#' character columns.
#'
#' @examples
#' df <- data.frame(Name = c("  Alice  ", " Bob "),
#'                           Age = c(25, 30),
#'                           stringsAsFactors = FALSE)
#' trimmed_df <- trim_whitespace(df)
#' # trimmed_df will have "Alice" and "Bob" without leading/trailing
#' # spaces in the 'Name' column
#' print(trimmed_df)
#'
#' @export
trim_whitespace <- function(df) {
  df[] <- lapply(df, function(x) {
    if (is.character(x)) {
      trimws(x)  # Trim whitespace for character columns
    } else {
      x  # Leave other types unchanged
    }
  })
  return(df)
}
