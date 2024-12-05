utils::globalVariables(c("TEST_LIST",
                         "PREFIX_LIST"))

# Global variable for the list of test names
#' @name TEST_LIST
#' @title A list of test types that appear in ARLN pdfs
#' @examples
#' TEST_LIST <- c("OXA","beta-lactamase")
#' @export
TEST_LIST <- c("OXA","beta-lactamase")

# Global variable for the list of test prefix names
#' @name PREFIX_LIST
#' @title A list of prefixes to associate data with the respective test
#' @examples
#' PREFIX_LIST <- c("oxa","bla")
#' @export
PREFIX_LIST <- c("oxa","bla")


#' @title Add Prefix to Specific Columns Based on Name
#'
#' @description
#' This function checks if certain columns (`"beta-lactamase"` or `"OXA"`)
#' exist in a given data frame. If the `"beta-lactamase"` column is present,
#' the function adds the prefix `"bla_"` to each value in that column. If the
#' `"OXA"` column exists instead, it adds the prefix `"oxa_"`. The modified
#' data frame is then returned.
#'
#' @param data A data frame that may contain the columns `"beta-lactamase"` or
#' `"OXA"`. The function will add the respective prefix to the values in these
#' columns if they are present.
#'
#' @return The modified data frame with prefixed values in the
#' `"beta-lactamase"` or `"OXA"` columns.
#'
#' @examples
#' df <- data.frame(`beta-lactamase` = c("gene1", "gene2"),
#'                   OXA = c("geneA", "geneB"),
#'                   stringsAsFactors = FALSE)
#' result_df <- prefix_column_single_by_test(df)
#' # result_df will have "bla_gene1", "bla_gene2" in the 'beta-lactamase'
#' # column and "oxa_geneA", "oxa_geneB" in the 'OXA' column
#'
#' @export
prefix_column_single_by_test <- function(data) {
  if ("beta-lactamase" %in% names(data)) {
    data$`beta-lactamase` <- paste0("bla_", data$`beta-lactamase`)
  } else if ("OXA" %in% names(data)) {
    data$OXA <- paste0("oxa_", data$OXA)
  }
  return(data)
}


#' @title Rename Columns for Data Frame Join
#'
#' @description
#' This function selects the first and third columns from a data frame and
#' renames them to `"Keys"` and `"Values"`, respectively. This is typically
#' used to prepare the data frame for a join operation where these columns
#' serve as the key-value pairs.
#'
#' @param df A data frame with at least three columns. The first and third
#' columns are selected and renamed.
#'
#' @return A data frame with only the first and third columns, renamed to
#' `"Keys"` and `"Values"`.
#'
#' @examples
#' df <- data.frame(A = 1:3,
#'                  B = letters[1:3],
#'                  C = c("X", "Y", "Z"),
#'                  stringsAsFactors = FALSE)
#' renamed_df <- rename_columns_for_join(df)
#' # renamed_df will have two columns: 'Keys' (from column A) and
#' # 'Values' (from column C)
#'
#' @export
rename_columns_for_join <- function(df) {
  df_select <- df[c(1,3)]
  colnames(df_select) <- c("Keys", "Values")
  return(df_select)
}


#' @title Process Results for Join Operation
#'
#' @description
#' This function processes the input data frame by applying two operations:
#' 1. It adds a prefix to specific columns (using the
#' `prefix_column_single_by_test` function).
#' 2. It selects and renames the first and third columns (using the
#' `rename_columns_for_join` function) to prepare them for a join operation.
#' The function returns the modified data frame with the appropriate
#' transformations applied.
#'
#' @param df A data frame to be processed. The function expects the input data
#' frame to have columns that may require prefixing and renaming.
#'
#' @return A data frame that has been processed with the appropriate prefix
#' added to columns and the first and third columns renamed to `"Keys"` and
#' `"Values"`, respectively.
#'
#' @examples
#' df <- data.frame(`beta-lactamase` = c("IMP", "KPC", "OXA-48","VIM","NDM"),
#'                   Results = c("not det",
#'                               "not det",
#'                               "not det",
#'                               "not det",
#'                               "det"),
#'                   `Reference Range` = c("not det",
#'                                         "not det",
#'                                         "not det",
#'                                         "not det",
#'                                         "not det" ),
#'                   Location = c("NYC", "NYC", "NYC", "NYC", "NYC"),
#'                   stringsAsFactors = FALSE
#'                   )
#' processed_df <- results_processing(df)
#' # processed_df will have the `"beta-lactamase"` or `"OXA"` columns prefixed,
#' # and the first and third columns renamed to 'Keys' and 'Values'.
#'
#' @export
results_processing <- function(df) {
  res <- prefix_column_single_by_test(df)
  res_final <- rename_columns_for_join(res)

  return(res_final)
}



#' @title Process a List of Data Frames and Pivot to Wide Format
#'
#' @description
#' This function processes a list of data frames, applies the
#' `results_processing` function to each data frame (except the first one),
#' and then combines them into a single wide-format data frame. The function
#' performs the following operations:
#' 1. Applies the `results_processing` function to each data frame (excluding
#' the first one).
#' 2. Combines the processed data frames into one using `bind_rows()`.
#' 3. Pivots the resulting combined data frame to a wide format, with columns
#' derived from the `"Keys"` column and their corresponding values from the
#' `"Values"` column.
#'
#' @param df_list A list of data frames. The function expects that the first
#' data frame in the list will serve as the base, and all subsequent data
#' frames will be processed and joined.
#'
#' @return A data frame in wide format, with columns based on the unique
#' values in the `"Keys"` column and corresponding values from the `"Values"`
#' column.
#' @importFrom tidyr pivot_wider
#' @importFrom magrittr %>%
#' @importFrom rlang sym !!
#'
#' @examples
#' df1 <- data.frame(Keys = c("gene1", "gene2"),
#'                   Data_A =c("a1", "a2"),
#'                   Values = c("bla_gene1", "bla_gene2"))
#' df2 <- data.frame(Keys = c("gene3", "gene4"),
#'                   Data_B =c("b1", "b2"),
#'                   Values = c("oxa_gene3", "oxa_gene4"))
#' df_list <- list(df1, df2)
#' result_df <- arln_process_dfs_to_row(df_list)
#' # result_df will be a wide-format data frame with columns
#' # for 'gene1', 'gene2', 'gene3', 'gene4'
#' # and the corresponding values from the 'Values' column
#'
#' @export
arln_process_dfs_to_row <- function(df_list) {
  if (length(df_list) == 0) {
    stop("The list is empty.")
  }

  # Process the results data frames uniformly
  results <- lapply(df_list[-1], results_processing)
  fulljoin_result <- dplyr::bind_rows(df_list[[1]],results)
  wide_df <- fulljoin_result %>% tidyr::pivot_wider(names_from = !!sym("Keys"),
                                                    values_from = !!sym("Values"))

  # Return the results as a list
  return(wide_df)
}


#' @title Process a List of PDF Files and Extract Data
#'
#' @description
#' This function processes a list of PDF files, extracting text from each PDF
#' using the `pdftools::pdf_text` function, then applies the `tenn_arln_pdf_extractor`
#' function to extract relevant data from the PDF content, and finally processes
#' the data frame using the `arln_process_dfs_to_row` function.
#'
#' @param file_list A character vector of file paths to the PDF files to be processed.
#'
#' @return A list of data frames, where each data frame corresponds to the data extracted
#'         from a single PDF file and processed into rows.
#'
#' @importFrom pdftools pdf_text
#'
#' @examples
#' example1 = system.file("extdata",
#'                        "pdf_tnarln_example_1.pdf",
#'                         package = "arlog")
#' example2 = system.file("extdata",
#'                        "pdf_tnarln_example_2.pdf",
#'                         package = "arlog")
#' file_list <- c(example1, example2)
#'
#' data_rows(file_list)
#'
#' @export
# create processed data tables
data_rows <- function(file_list){
  rows <- lapply(file_list, function(file) {
    pdf <- pdftools::pdf_text(file)
    df <- tenn_arln_pdf_extractor(pdf)
    proc_df <- arln_process_dfs_to_row(df)
  })
  return(rows)
}


#' @title Fill Missing Columns with Default Values
#'
#' @description
#' This function ensures that a data frame contains all specified columns. If
#' any columns are missing, they are added to the data frame and filled with the
#' default value `"Not in Report"`. Existing columns from the input data frame
#' are retained. This function is useful for standardizing data frames to a
#' common set of columns.
#'
#' @param df A data frame that may have a subset of the columns defined in
#' `all_columns`.
#' @param all_columns A character vector of column names that should be present
#' in the output data frame.
#'
#' @return A data frame that contains all columns specified in `all_columns`.
#' Missing columns are added and filled with the value `"Not in Report"`.
#' Existing columns from the input data frame are retained.
#'
#' @examples
#' # Example data frames (df1 and df2) to demonstrate the function
#' df1 <- data.frame(Keys = c("gene1", "gene2"),
#'                   Data_A =c("a1", "a2"),
#'                   Values = c("bla_gene1", "bla_gene2"))
#' df2 <- data.frame(Keys = c("gene3", "gene4"),
#'                   Data_B =c("b1", "b2"),
#'                   Values = c("oxa_gene3", "oxa_gene4"))
#'
#' # Combine the data frames into a list
#' df_list <- list(df1, df2)
#'
#' # Process the data frames and convert to wide format
#' result_df <- arln_process_dfs_to_row(df_list)
#'
#' # View the resulting wide-format data frame
#' print(result_df)
#' # The result will be a wide-format data frame with columns:
#' # 'gene1', 'gene2', 'gene3', 'gene4' and their corresponding values.
#'
#' @export
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


#' @title Create a Unified Table with Standardized Columns
#'
#' @description
#' This function processes a list of data frames, ensuring they all have the
#' same columns by filling missing columns with a default value ("Not in
#' Report"). It then combines the data frames into one and trims any leading or
#' trailing white space from character columns. The resulting data frame is
#' returned.
#'
#' @param rows A list of data frames. Each data frame may have different
#' columns, and this function will standardize them by filling in missing
#' columns and combining them into one data frame.
#'
#' @return A data frame with standardized columns (based on the data frame with
#' the most columns) and trimmed character columns. Missing values are replaced
#' with `"Not in Report"`.
#'
#' @importFrom dplyr mutate across where
#'
#' @examples
#' df1 <- data.frame(ColumnA = c(1, 2),
#'                   ColumnB = c(" A ", " B "),
#'                   stringsAsFactors = FALSE)
#' df2 <- data.frame(ColumnA = c(3, 4),
#'                   ColumnC = c(" C1 ", " C2 "),
#'                   stringsAsFactors = FALSE)
#' rows <- list(df1, df2)
#' final_df <- create_table_arln(rows)
#' # final_df will have columns "ColumnA", "ColumnB", and "ColumnC", with
#' # missing values filled
#' # and leading/trailing spaces trimmed from character columns.
#'
#' @export
create_table_arln <-function(rows){
  # Get the data frame with the most columns
  base_df <- rows[[which.max(sapply(rows, ncol))]]
  base_columns <- colnames(base_df)

  filled_dfs <- lapply(rows, fill_missing_columns, all_columns = base_columns)
  combined_df <- dplyr::bind_rows(filled_dfs)

  df_trimmed <- combined_df %>%
    mutate(across(where(is.character), trimws))

  return(df_trimmed)
}
