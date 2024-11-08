source("~/Projects/hai-arlog-db/R/Extract/ExtractTennPdfData.R")
library(tidyverse)


test_list <- c("OXA","beta-lactamase")
prefix_list <- c("oxa","bla")

prefix_column_single_by_test <- function(data) {
  if ("beta-lactamase" %in% names(data)) {
    data$`beta-lactamase` <- paste0("bla_", data$`beta-lactamase`)
  } else if ("OXA" %in% names(data)) {
    data$OXA <- paste0("oxa_", data$OXA)
  }
  return(data)
}

rename_columns_for_join <- function(df) {
  df_select <- df[c(1,3)]
  colnames(df_select) <- c("Keys", "Values")
  return(df_select)
}

results_processing <- function(df) {
  res <- prefix_column_single_by_test(df)
  res_final <- rename_columns_for_join(res)
  
  return(res_final)
}

# Function to process a list of data frames, Tennessee ARLN
arln_process_dfs_to_row <- function(df_list) {
  if (length(df_list) == 0) {
    stop("The list is empty.")
  }
  
  # Process the results data frames uniformly
  results <- lapply(df_list[-1], results_processing)
  fulljoin_result <- bind_rows(df_list[[1]],results)
  wide_df <- fulljoin_result %>% pivot_wider(names_from = Keys, values_from = Values)
  
  # Return the results as a list
  return(wide_df)
}

# create processed data tables
data_rows <- function(file_list){
  rows <- lapply(file_list, function(file) {
    pdf <- pdf_text(file)
    df <- tenn_arln_pdf_extractor(pdf)
    proc_df <- arln_process_dfs_to_row(df)
  })
  return(rows)
}

fill_missing_columns <- function(df, all_columns) {
  # Create a new data frame with all columns
  new_df <- data.frame(matrix(ncol = length(all_columns), nrow = nrow(df)))
  colnames(new_df) <- all_columns
  
  # Fill the new data frame with the existing data
  new_df[ , colnames(df)] <- df
  
  # Fill missing columns with "Not in Report"
  new_df[is.na(new_df)] <- "Not in Report"
  
  return(new_df)
}

create_table_arln <-function(rows){
  # Get the data frame with the most columns
  base_df <- rows[[which.max(sapply(rows, ncol))]]
  base_columns <- colnames(base_df)
  
  filled_dfs <- lapply(rows, fill_missing_columns, all_columns = base_columns)
  combined_df <- bind_rows(filled_dfs)
  
  df_trimmed <- combined_df %>%
    mutate(across(where(is.character), trimws))
  
  return(df_trimmed)
}