#' Alter Summary Table
#' 
#' Produces a formatted table of alter-level summary variables created by
#' make_summary_vars or construct_measures 
#' 
#' @param egor.data An egor object.
#' @param caption An optional string to put at the top of the table
#' @export
table1_alter <- function(egor.data, caption = NULL) {
  
  table_text <- paste0("table1::table1(~`", paste0(names(egor.data$alter)[grepl("cat\\.", names(egor.data$alter))], "`", collapse = "+`"),
                       "+`", paste0(names(egor.data$alter)[grepl("struct\\.", names(egor.data$alter))], "`", collapse = "+`"), 
                       "+`", paste0(names(egor.data$alter)[grepl("di\\.", names(egor.data$alter))], "`", collapse = "+`"), 
                       ", data = egor.data$alter, overall = 'Alters', caption = caption)")
  
  # If one type of those variables is missing, this will break, so fix that here:
  table_text <- gsub("\\+\\`\\`", "", table_text)
  table_text <- gsub("\\`\\`\\+", "", table_text)
  eval(parse(text = table_text))
}

