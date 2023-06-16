#' Split Egor Data Into Subnetworks 
#' 
#' Uses provided expressions to define indicator variables for different
#' subnetwork levels and add another level to the egor input object
#' that specifies each subnetwork
#' 
#' @param egor.data An egor object.
#' @param alter.formula.binary An expression to be executed within a dplyr::mutate to define a new binary variable to distinguish separate alter-level networks.
#' @param aatie.formula.binary An expression to be executed within a dplyr::mutate to define a new binary variable to distinguish separate aatie-level networks.
#' @export 
split_subnetworks <- function(egor.data, alter.formula.binary = list(""), aatie.formula.binary = list("")) {
  
  if(! "egor" %in% class(egor.data)){
    stop("egor.data must be an egor-type object.")
  }
  
  egor.alter.splits <- NULL
  
  # 4/13 add: Drop records from ego and alters where only one alter exists (moved to here from process_egoweb)
  egor.data$alter <- egor.data$alter |>
    group_by(.egoID) |>
    filter(length(unique(.altID)) >= 2) |>
    ungroup()
  
  egor.data$ego <- egor.data$ego |>
    filter(.egoID %in% egor.data$alter$.egoID)
  
  # 5/4 add to fix "Inclue Ego" not working: add a weight constant to aatie level
  egor.data$aatie <- egor.data$aatie |>
    mutate(weight = 1)
  
  ###############################
  ### Apply Alter Split First ###
  ###############################
  
  if(TRUE %in% (alter.formula.binary != "")){
    
    # Activate the alter level
    egor.data <- egor.data |> egor::activate(alter)
  
    # Mutate the indicator variable(s)
    alter_form_text <- paste0("egor.data <- egor.data |> mutate(subnetwork.alter.", alter.formula.binary, ")")
    eval(parse(text = alter_form_text))
  
    # Pull names of the alter subnetwork variables and levels
    subnetwork.alter.vars <- unique(names(egor.data$alter)[grepl("subnetwork.alter", names(egor.data$alter))])
    subnetwork.alter.levels <- gsub("subnetwork.alter.", "", subnetwork.alter.vars)
  
    # Create new version of egor object filtering to each network binary level
    alter_split_text <- paste0("egor.data.", subnetwork.alter.levels, " <- ", 
                               "egor.data |> filter(", subnetwork.alter.vars, " %in% 1)")
    eval(parse(text = alter_split_text))
    
    # Drop the alter-level subnetwork variables
    drop_var_alter_text <- NULL
    for(i in 1:length(subnetwork.alter.vars)){
      drop_var_alter_text <- append(drop_var_alter_text, 
                                    paste0("egor.data.", subnetwork.alter.levels, " <- ",
                                           "egor.data.", subnetwork.alter.levels, " |> select(-", subnetwork.alter.vars[i], ")"))
    }
    eval(parse(text = drop_var_alter_text))
  
    egor.alter.splits <- paste0("egor.data.", subnetwork.alter.levels) |> paste0(collapse = ", ")
  }
  
  
  ############################
  ### Apply Aatie Formulas ###
  ############################

  if(TRUE %in% (aatie.formula.binary != "")){
  
    if(is.null(egor.alter.splits)) {
      egor.alter.splits <- "egor.data"} else {
        egor.alter.splits <- egor.alter.splits |> strsplit(split = ", ") |> unlist()
      }
  
    # Activate the aatie level
    activate_text <- paste0(egor.alter.splits, " <- ", egor.alter.splits, " |> egor::activate(aatie)")
    eval(parse(text = activate_text))
  
    # Mutate the indicator variable(s)
    aatie_form_text <- NULL
    for(i in 1:length(egor.alter.splits)){
      aatie_form_text <- append(aatie_form_text,
                                paste0(egor.alter.splits[i], " <- ", egor.alter.splits[i], 
                                  " |> mutate(subnetwork.aatie.", aatie.formula.binary, ")"))
    }
    eval(parse(text = aatie_form_text))
    
    # Pull names of the aatie subnetwork variables and levels
    subnetwork.aatie.levels <- substr(aatie.formula.binary, 1, regexpr("=", aatie.formula.binary) - 1) |> trimws()
    subnetwork.aatie.vars   <- paste0("subnetwork.aatie.", subnetwork.aatie.levels)
    
    # Create new version of networks filtering to each aatie binary level
    aatie_split_text <- NULL
    for(i in 1:length(egor.alter.splits)){
      for(j in 1:length(subnetwork.aatie.levels)){
        aatie_split_text <- append(aatie_split_text,
                                   paste0(egor.alter.splits[i], ".", subnetwork.aatie.levels[j], " <- ", 
                                          egor.alter.splits[i], " |> filter(", subnetwork.aatie.vars[j], " == 1)"))
      }
    }
                                      
    eval(parse(text = aatie_split_text))
    
    # Drop the aatie-level subnetwork variables
    nets <- substr(aatie_split_text, 1, regexpr(" <- ", aatie_split_text) - 1)
    drop_var_aatie_text <- paste0(nets, " <- ", nets, " |> select(-starts_with('subnetwork.aatie'))")
    eval(parse(text = drop_var_aatie_text))
  
    final_splits <- nets |> paste0(collapse = ", ")
    final_list_text <- paste0("final_split_results <- list(", final_splits, ")")
    eval(parse(text = final_list_text))
  
    names(final_split_results) <- final_splits |> strsplit(split = ", ") |> unlist() 
    names(final_split_results) <- gsub("egor.data.", "", names(final_split_results))
  
  } else {
    final_list_text <- paste0("final_split_results <- list(", egor.alter.splits, ")")
    eval(parse(text = final_list_text))

    names(final_split_results) <- egor.alter.splits |> strsplit(split = ", ") |> unlist()
    names(final_split_results) <- gsub("egor.data.", "", names(final_split_results))
  }
  
  return(final_split_results)
}