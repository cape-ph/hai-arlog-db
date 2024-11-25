source('~/Projects/haiarlogdb/R/ExportCsv/CsvExporter.R')

export_data <- function() {
  tenn_arln <- process_tenn_arln(create_tenn_arln())
  wrd_alrt <- process_word_alert(create_word_alert())
  xl_cpo <- process_excel_cpo(create_excel_cpo())
  xl_sent <- process_excel_sentinel(create_sentinel())
  pdf_cpo <- process_pdf_cpo_seq(create_cpo_seq())
  wb_prt <- process_web_portal(create_web_portal())
}
