library(docxtractr)
library(readxl)
library(dplyr)
library(pdftools)
library(stringr)
library(tidyr)


excel_set_second_row_as_colnames <- function(data) {
  # Check if the data has at least two rows
  if (nrow(data) < 2) {
    stop("Data must have at least two rows to set column names.")
  }
  
  # Set the second row as column names
  colnames(data) <- data[1, ]
  
  # Remove the first row
  data <- data[-c(1,1), , drop = FALSE]
  
  # Return the cleaned data
  return(data)
}