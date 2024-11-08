source("~/Projects/hai_arlog/CreatePostProcessTablesAll.R")

tenn_arln <- process_tenn_arln(create_tenn_arln_csv())
wrd_alrt <- process_word_alert(create_word_alert_csv())
xl_cpo <- process_excel_cpo(create_excel_cpo_csv())
xl_sent <- process_excel_sentinel(create_sentinel_csv())
pdf_cpo <- process_pdf_cpo_seq(create_cpo_seq_csv())
wb_prt <- process_web_portal(create_web_portal_csv())
data <- list(tenn_arln, wrd_alrt, xl_cpo, xl_sent, pdf_cpo, wb_prt)

#wrd_alrt[, c()]

# do stuff eith tenn_arln and wrd_alert -> newtable1
# do stuff with new table 1 and excel cpo -> newtable 2
