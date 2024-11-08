library(docxtractr)
library(readxl)
library(dplyr)
library(pdftools)
library(stringr)
library(tidyr)


pdf_extract_lines_between <- function(lines, start_string, end_string) {
  # Find indices of the lines containing the start and end strings
  start_index <- grep(start_string, lines)
  end_index <- grep(end_string, lines)
  
  # Check if both indices were found
  if (length(start_index) == 0 || length(end_index) == 0) {
    stop("Start or end string not found in the lines.")
  }
  
  # Get the first occurrence of the start string and the last occurrence of the end string
  start <- start_index[1]
  end <- tail(end_index, 1)
  
  # Extract lines including the start line and excluding the end line
  if (start < end) {
    return(lines[start:(end - 1)])  # Return lines from start to just before end
  } else {
    return(character(0))  # Return an empty character vector if start is after end
  }
}

pdf_split_lines_into_columns <- function(lines) {
  # Define a regex pattern for matching more than 15 spaces
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

# Function to extract keys and values, this does not get called directly on dataframes
# it gets called by the next function 
extract_key_value <- function(cell) {
  if (is.na(cell) || cell == "<NA>") {
    return(NULL)
  }
  parts <- str_split(cell, ": ", simplify = TRUE)
  return(list(key = parts[1], value = ifelse(length(parts) > 1, parts[2], NA)))
}

# this gets called on the dataframe
extract_from_dataframe <- function(data) {
  # Create an empty data frame for results
  results <- data.frame(Keys = character(), Values = character(), stringsAsFactors = FALSE)
  
  # Loop through each column and extract keys and values
  for (col in names(data)) {
    for (item in data[[col]]) {
      extracted <- extract_key_value(item)
      if (!is.null(extracted)) {
        results <- rbind(results, data.frame(Keys = extracted$key, Values = extracted$value, stringsAsFactors = FALSE))
      }
    }
  }
  
  return(results)
}

select_rows_by_list <- function(data, col_name, value_list) {
  data %>%
    filter(.data[[col_name]] %in% value_list)
}

values_to_keep <- c("Patient Name", "Date of Birth", "Specimen Type", "Date Collected", "Date Received", "Date Reported")

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
    return(data.frame(Keys = "Facility", Values = concatenated_string, stringsAsFactors = FALSE))
  } else {
    return(data.frame(Keys = NA, Values = NA, stringsAsFactors = FALSE))
  }
}

CULTURE_ORGANISM_LIST <- c("RESULT:Carbapenem Resistant Organism Culture","RESULT:Mycology Culture")

search_for_culture_organism <- function(lines, CULTURE_ORGANISM_LIST) {
  # Initialize a list to store matching lines
  matching_lines <- list()
  
  # Loop through each line
  for (line in lines) {
    # Check if any fruit from the list is present in the line
    if (any(grepl(paste(CULTURE_ORGANISM_LIST, collapse = "|"), line, ignore.case = TRUE))) {
      matching_lines <- append(matching_lines, line)
    }
  }
  
  return(matching_lines)
}

# this function appears to exist in 2 places
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
  result_df <- data.frame(Keys = keys, Values = values, stringsAsFactors = FALSE)
  
  return(result_df)
}

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
        # If it's between "PCR" and "Performing" or "Comments", add the line to the current table
        current_table <- c(current_table, line)
      }
    }
  }
  
  return(tables)
}

drop_empty_rows <- function(tables) {
  cleaned_tables <- lapply(tables, function(table) {
    # Remove rows that are empty (only whitespace)
    table[trimws(table) != ""]
  })
  return(cleaned_tables)
}

COLUMN_NAMES <- c("Results", "Reference Range", "Performing Location")

add_first_line_as_column_name <- function(tables, table_index) {
  # Check if the specified table index is valid
  if (table_index <= length(tables) && table_index > 0) {
    first_line <- tables[[table_index]][1]  # Get the first line of the specified table
    
    # Extract the substring after "-PCR"
    if (grepl("-PCR", first_line)) {
      first_line <- sub(".*-PCR\\s*", "", first_line)  # Get everything after "-PCR"
    } else {
      stop("The first line does not contain '-PCR'.")
    }
    
    # Replace ß with "beta" in the first line if it exists
    if (grepl("ß", first_line)) {
      first_line <- gsub("ß", "beta", first_line)
    }
    
    new_column_names <- c(first_line, COLUMN_NAMES)  # Add the first line to the column names
    
    new_column_names[-1] <- paste(first_line, new_column_names[-1])  # Modify the rest of the column names
    
    return(new_column_names)  # Return the updated column names
  } else {
    stop("Invalid table index.")
  }
}

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

rename_dataframe_columns <- function(df, column_names) {
  # Check if the length of column_names matches the number of columns in the data frame
  if (length(column_names) != ncol(df)) {
    stop("Length of column_names must match the number of columns in the data frame.")
  }
  
  # Set the new column names
  colnames(df) <- column_names
  return(df)
}

# the 2 table pdf thats different from the other one
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

remove_empty_lines <- function(lines) {
  # Filter out empty lines
  return(lines[nzchar(trimws(lines))])
}

add_spaces_to_lines <- function(lines) {
  # Add three spaces to the beginning of each line
  modified_lines <- paste0("   ", lines)
  
  return(modified_lines)  # Return the modified list of lines
}

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

add_string_to_first_line <- function(lines, string_to_add) {
  # Check if the list is not empty
  if (length(lines) > 0) {
    # Add the string to the beginning of the first line
    lines[1] <- paste0(string_to_add, lines[1])
  }
  
  return(lines)  # Return the modified list of lines
}

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