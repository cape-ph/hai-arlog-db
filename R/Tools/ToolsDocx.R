library(docxtractr)
library(readxl)
library(dplyr)
library(pdftools)
library(stringr)
library(tidyr)


# Reads a word document and finds a table. 
# extract_table_from_word(word_document) -> table
docx_extract_table <- function(file_path) {
  # Read the Word document
  doc <- read_docx(file_path)
  
  # Extract tables from the document
  tables <- doc %>% docx_extract_all_tbls()
  
  # Check if there are tables and return the first one
  if (length(tables) == 0) {
    stop("No tables found in the document.")
  }
  
  # Return the first table as a data frame
  return(tables[[1]])
}


# Function to rename columns using the first row
docx_rename_columns_with_first_row <- function(table) {
  # Check if the table has at least one row
  if (nrow(table) < 1) {
    stop("The table does not have enough rows to rename columns.")
  }
  
  # Set the first row as column names
  colnames(table) <- table[1, ]
  
  # Remove that first row after setting column names
  table <- table[-1, ]
  
  # Reset row names
  rownames(table) <- NULL
  
  return(table)
}