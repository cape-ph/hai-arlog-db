#' @title Extracts a dataframe from CPO Sequencing Report
#' @description
#' Extracts the tables found on the second page of the CPO sequencing
#' report pdf.
#'
#' @param pdf pdf lines read in from pdftools.
#'
#' @return Dataframe with data extracted from CPO pdf.
#' @importFrom tibble rownames_to_column
#' @export
#'
#' @examples
#' # Read in pdf and extract lines
#' pdf_file_path <- system.file("extdata",
#'                              "pdf_cposeq_example.pdf",
#'                              package = "arlog")
#' pdf <- pdftools::pdf_text(pdf_file_path)
#' df <- tenn_cpo_seq_extractor(pdf)
#' print(df)
tenn_cpo_seq_extractor <- function(pdf) {
lines <- unlist(strsplit(pdf, "\n"))

table1 <- pdf_extract_lines_between(lines, "Table 1:", "Table 2:")
strip_table1 <- remove_lines_before_string(table1,'WGS_ID')
drop_empty_lines_t1 <- remove_empty_lines(strip_table1)
df1 <- create_dataframe_from_lines(drop_empty_lines_t1)
# Remove leading and trailing spaces from column names
colnames(df1) <- trimws(colnames(df1))
df1_cleaned <- trim_whitespace(df1)


table2 <- pdf_extract_lines_between(lines, "Table 2:", "Results:")
strip_table2 <- remove_lines_before_string(table2,'####')
edit_strip_t2 <- add_string_to_first_line(strip_table2, '   Gene  ')
add_buffer_t2 <- add_spaces_to_lines(edit_strip_t2)
df2 <- create_dataframe_from_lines(add_buffer_t2)
transposed_matrix <- t(df2[-1])
transposed_df2 <- as.data.frame(transposed_matrix, stringsAsFactors = FALSE)
colnames(transposed_df2) <- transposed_df2[1, ]
transposed_df2 <- transposed_df2[-1, , drop = FALSE]
new_index_tdf2 <- tibble::rownames_to_column(transposed_df2, "WGS_ID")


merged_df <- merge(df1, new_index_tdf2, by.x = "WGS_ID", by.y = "WGS_ID", all = TRUE)
merged_df <- merge(df1_cleaned, new_index_tdf2, by = "WGS_ID", all = TRUE)

return(merged_df)
}

