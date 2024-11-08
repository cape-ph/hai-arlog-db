source("~/Projects/hai-arlog-db/R/Tools/ToolsPdfExtraction.R")

tenn_arln_pdf_extractor <- function(pdf) {
  
lines <- unlist(strsplit(pdf, "\n"))
result <- pdf_extract_lines_between(lines, "Accession", "Key")
result_df <- pdf_split_lines_into_columns(result)
extract_table <- extract_from_dataframe(result_df)
selection <- select_rows_by_list(extract_table,"Keys", values_to_keep)

accession_num <- extract_accession_info(result)
selection_add_accession_num <- bind_rows(selection,accession_num)

pdf_facility <- get_facility_data(extract_table, "Keys")
table_add_facility <- bind_rows(selection_add_accession_num,pdf_facility)

pdf_organism_culture <- search_for_culture_organism(lines, culture_organism_list)
pdf_organism_culture_result <- create_event_dataframe_from_lines(pdf_organism_culture)
table_add_culture_organism <- bind_rows(table_add_facility, pdf_organism_culture_result)

tables <- extract_tables_from_lines(lines)
cleaned_tables <- drop_empty_rows(tables)

results_dfs <- list()
results_dfs[[1]] <- table_add_culture_organism

n <- length(cleaned_tables)

if (n > 0){
  for (i in 1:n) {
    table <- add_first_line_as_column_name(cleaned_tables,i)
    df <- parse_lines_to_dataframe(cleaned_tables[[i]][-1])
    updated_column_names <- add_first_line_as_column_name(cleaned_tables,i)
    renamed_table_df <- rename_dataframe_columns(df, updated_column_names)
    results_dfs[[i+1]] <- renamed_table_df
  }
}

return(results_dfs)
}

