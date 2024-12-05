EXCEL_ARLN_WEB_PORTAL_DIR = "/Users/edonate3/Documents/CAPE/HAI_Data/ExcelArlnWebPortal"
EXCEL_CPO_DIR = "/Users/edonate3/Documents/CAPE/HAI_Data/ExcelCpo"
EXCEL_SENTINEL_DIR = "/Users/edonate3/Documents/CAPE/HAI_Data/ExcelSentinel"
PDF_ARLN_DIR = "/Users/edonate3/Documents/CAPE/HAI_Data/PdfArln"
PDF_CPO_SEQ_DIR = "/Users/edonate3/Documents/CAPE/HAI_Data/PdfCpoSeq"
WORD_ALERT_DIR = "/Users/edonate3/Documents/CAPE/HAI_Data/WordAlert"

CSV_OUTPUT_DIR = "/Users/edonate3/Documents/CAPE/HAI_Processed/"

export_data <- function(CSV_OUTPUT_DIR) {
  process_tenn_arln(create_tenn_arln(PDF_ARLN_DIR), CSV_OUTPUT_DIR)
  process_word_alert(create_word_alert(WORD_ALERT_DIR), CSV_OUTPUT_DIR)
  process_excel_cpo(create_excel_cpo(EXCEL_CPO_DIR), CSV_OUTPUT_DIR)
  process_excel_sentinel(create_sentinel(EXCEL_SENTINEL_DIR), CSV_OUTPUT_DIR)
  process_pdf_cpo_seq(create_cpo_seq(PDF_CPO_SEQ_DIR), CSV_OUTPUT_DIR)
  process_web_portal(create_web_portal(EXCEL_ARLN_WEB_PORTAL_DIR), CSV_OUTPUT_DIR)
}
