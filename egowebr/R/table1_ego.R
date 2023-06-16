#' Ego Summary Table
#' 
#' Produces a formatted table of ego-level summary variables created by
#' make_summary_vars or construct_measures
#' 
#' @param egor.data An egor object.
#' @param caption An optional string to put at the top of the table
#' @export
table1_ego <- function(egor.data, caption = NULL) {
  
  table_text <- paste0("table1::table1(~`", paste0(names(egor.data$ego)[grepl("cat\\.", names(egor.data$ego))], "`", collapse = "+`"),
                       "+`", paste0(names(egor.data$ego)[grepl("mean\\.", names(egor.data$ego))], "`", collapse = "+`"),
                       "+`", paste0(names(egor.data$ego)[grepl("struct\\.", names(egor.data$ego))], "`", collapse = "+`"),
                       "+`", paste0(names(egor.data$ego)[grepl("tot\\.", names(egor.data$ego))],"`", collapse = "+`"),
                       "+`", paste0(names(egor.data$ego)[grepl("prop\\.", names(egor.data$ego))], "`", collapse = "+`"),
                       "+`", paste0(names(egor.data$ego)[grepl("di\\.", names(egor.data$ego))], "`", collapse = "+`"),
                       ", data = egor.data$ego, overall = 'Egos', caption = caption)")
  
  # If one type of those variables is missing, this will break, so fix that here:
  table_text <- gsub("\\+\\`\\`", "", table_text)
  table_text <- gsub("\\`\\`\\+", "", table_text)
  eval(parse(text = table_text))
}