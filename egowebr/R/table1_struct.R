#' Structural Measure Summary Table
#' 
#' Produces a formatted table of structural measures created by construct_measures
#' 
#' @param egor.data An egor object.
#' @param caption An optional string to put at the top of the table
#' @export
table1_struct <- function(egor.data, caption = NULL) {
  
  # Construct a `from` variable to indicate the combination of subnetwork and ego vs alter
  # Then bind all those datasets together
  for(i in 1:length(egor.data)) {
    subnet_name <- names(egor.data)
    
    egos   <- egor.data[[i]]$ego |>
      select(.egoID, starts_with("struct.")) |>
      mutate(from = paste0(subnet_name[i], ", ego"))
    
    alters <- egor.data[[i]]$alter |>
      select(.egoID, .altID, starts_with("struct.")) |>
      mutate(from = paste0(subnet_name[i], ", alter"))
    
    if(i == 1){
      bound_data <- bind_rows(egos, alters)
    } else {
      bound_data <- bind_rows(bound_data, egos, alters)
    }
  }
  
  # We want all ego columns to appear before alter columns --
  # Change `from` to be an ordered factor
  from <- unique(bound_data$from)
  from_ego   <- from[substr(from, nchar(from) - 2, nchar(from)) == "ego"]
  from_alter <- from[substr(from, nchar(from) - 4, nchar(from)) == "alter"]
  
  bound_data <- bound_data %>%
    mutate(from = factor(from, 
                         levels = c(from_ego, from_alter))) %>%
    arrange(from)

  # Run all the structural variables through table1, splitting on the `from` variable
  struct.vars <- names(bound_data)[substr(names(bound_data), 1, 7) == "struct."]
  
  table1_text <- paste0("table1::table1(~", paste0(struct.vars, collapse = "+"), " | from, data = bound_data, overall = NULL)")
  eval(parse(text = table1_text))
}


