#' Create Summary Variables for an Egor Object
#' 
#' Uses an EgoWeb codebook to add statistical summary variables onto the
#' input egor object -- dummy variables for multiple selection question values,
#' recoded categorical variables using the codebook labels, 
#' ego-level totals, ego-level means, and ego-level proportions.
#' 
#' @param egor.data An egor object.
#' @param codebook.address The address to the corresponding EgoWeb codebook.
#' @param additional.multselect.vars An optional vector of strings listing variables not in the codebook that should be processed as multiple selection variables.
#' @param additional.multselect.vars.labels A list providing corresponding labels to be used with the additional.multselect.vars.
#' @param prop.denom A string specifying how denominators for ego-level proportions should be calculated, where 'struct.alters' refers to the total alter count for each ego.
#' @param additional.numeric.vars An optional vector of strings listing variables not in the codebook that should be processed as numeric.
#' @param output.csv.address An optional address to which a summary of the newly constructed variables should be written. 
#' @export
make_summary_vars <- function(egor.data, codebook.address, additional.multselect.vars = NULL, additional.multselect.vars.labels = list(""),
                              prop.denom = "struct.alters", additional.numeric.vars = NULL, output.csv.address = "") {
  
  ##################
  ### Run Checks ###
  ##################
  
  # Check that supplied data is egor type
  if(! "egor" %in% class(egor.data)){
    stop("egor.data is not egor-type. Run process_egoweb() first.")
  }
  
  # Check that supplied codebook is .csv (could expand this to allow .xlsx also)
  if(! substr(codebook.address, nchar(codebook.address) - 3, nchar(codebook.address)) == ".csv") {
    stop("codebook.address must be the file address for a .csv codebook")
  }
  
  #####################
  ### Read codebook ###
  #####################
  
  codebook <- read.csv(codebook.address)
  
  # Replace any spaces in variable names with periods to match how they'll get read in
  codebook$Question.Title <- gsub(" ", "\\.", codebook$Question.Title)
  
  ########################################################################
  ### Make dichotomous dummies for all multiple_select variable values ###
  ########################################################################
  
  # Grab the multiple select variables
  multselect_vars <- codebook$Question.Title[codebook$Response.Type == "MULTIPLE_SELECTION" & codebook$Subject.Type %in% c("EGO", "ALTER")]
  
  # Also grab the multiple select variables THAT HAVE A MAX OF 1 for later; these can only have one response value per obs
  exclusive_vars <- codebook$Question.Title[codebook$Response.Type == "MULTIPLE_SELECTION" & codebook$Subject.Type %in% c("EGO", "ALTER") & codebook$Max %in% 1]
  
  # Construct split_def as a list containing the values and their corresponding labels for each question (from the codebook)  
  split_def <- codebook$Options[codebook$Response.Type == "MULTIPLE_SELECTION" & codebook$Subject.Type %in% c("EGO", "ALTER")] |>
    strsplit(split = "; '")
  
  # IF ADDITIONAL MULTSELECT VARIABLES SPECIFIED: add those in and their definitions
  multselect_vars <- c(multselect_vars, additional.multselect.vars)
  if(! is.null(additional.multselect.vars)){
    for(i in 1:length(additional.multselect.vars)){
      split_def[[length(split_def) + 1]] <- additional.multselect.vars.labels[[i]]
    }
  }
  
  # Construct values as just the right-hand side values from split_def, and labels as the left-hand
  values <- split_def
  labels <- split_def
  for(i in 1:length(split_def)) {
    for(j in 1:length(split_def[[i]])) {
      labels[[i]][j] <- substr(split_def[[i]][j], 1, regexpr("=", split_def[[i]][j]) - 2) |>
        trimws()
      values[[i]][j] <- substr(split_def[[i]][j], nchar(split_def[[i]][j]) - 1, nchar(split_def[[i]][j])) |>
        trimws()
    }
  }
  
  # Create dummy variable for each of these variables' possible values
  # And for exclusive variables, also make a new recoded version
  for(i in 1:length(multselect_vars)){
    
    ivar  <- multselect_vars[i]
    ivals <- values[[i]]
    ilabs <- labels[[i]]
    # Remove single apostrophes from the labels
    ilabs <- gsub("\\'", "", ilabs)
    
    # Store whether this variable was alter-level or ego-level (assume alter if it was additionally specified)
    # If the subject type is "EGO_ID", treat it as "EGO"
    ego_alter <- ifelse(ivar %in% additional.multselect.vars, "ALTER", codebook$Subject.Type[codebook$Question.Title == ivar])
    ego_alter <- ifelse(ego_alter %in% "EGO_ID", "EGO", ego_alter)
    
    # Add a leading space and trailing semicolon to match value formatting
    if(ego_alter == "EGO"){
      egor.data$ego[names(egor.data$ego) == ivar] <- paste0(" ", pull(egor.data$ego[names(egor.data$ego) == ivar]), ";")
    }
    if(ego_alter == "ALTER"){
      egor.data$alter[names(egor.data$alter) == ivar] <- paste0(" ", pull(egor.data$alter[names(egor.data$alter) == ivar]), ";")
    }
    
    # Then mutate the new variables, with `di` for dichotomous and `cat` for recoded categoricals
    # If EGO variable:
    if(ego_alter == "EGO"){
      for(j in 1:length(ivals)) {
        newvar <- paste0("`di.", ivar, "_", ilabs[j], "`")
        newvar_unquote <- paste0(ivar, "_", ilabs[j])
        mutate_text <- paste0("egor.data$ego <- egor.data$ego |> mutate(", 
                              newvar, " = ifelse(grepl(paste0(' ', ", ivals[j], ", ';'), ", ivar, "), 1, 0))")
        eval(parse(text = mutate_text))
      }
      if(ivar %in% exclusive_vars) {
        swaps <- paste0("' ", ivals, ";' =", "'", ilabs, "'", collapse = ", ")
        newvar <- paste0("`cat.", ivar, "`")
        recode_text <- paste0("egor.data$ego <- egor.data$ego |> mutate(", newvar, 
                             " = recode(", ivar, ", ", swaps, ", .default = 'Missing'))")
        eval(parse(text = recode_text))
      }
      
      # Then remove the added semicolons at the end of values. We can also trim whitespace simultaneously
      egor.data$ego <- egor.data$ego |> mutate(
        !!sym(ivar) := trimws(!!sym(ivar), whitespace = "[ ;\t\r\n]")
      )
    }
    
    # If ALTER variable:
    if(ego_alter == "ALTER"){
      for(j in 1:length(ivals)){
        newvar <- paste0("`di.", ivar, "_", ilabs[j], "`")
        newvar_unquote <- paste0(ivar, "_", ilabs[j])
        mutate_text <- paste0("egor.data$alter <- egor.data$alter |> mutate(", 
                              newvar, " = ifelse(grepl(paste0(' ', ", ivals[j], ", ';'), ", ivar, "), 1, 0))")
        eval(parse(text = mutate_text))
        
        # Make ego-level totals
        maketotals_text <- paste0("egor.data$alter <- egor.data$alter |> group_by(.egoID) |> mutate(", paste0("`tot.", 
                                                                                                              newvar_unquote, "`"),  "= sum(", newvar, ", na.rm = TRUE))")
        eval(parse(text = maketotals_text))
      }
      if(ivar %in% exclusive_vars) {
        swaps <- paste0("' ", ivals, ";' =", "'", ilabs, "'", collapse = ", ")
        newvar <- paste0("`cat.", ivar, "`")
        recode_text <- paste0("egor.data$alter <- egor.data$alter |> mutate(", newvar, 
                              " = recode(", ivar, ", ", swaps, ", .default = 'Missing'))")
        eval(parse(text = recode_text))
      }
      
      # Then remove the added semicolons at the end of values. We can also trim whitespace simultaneously
      egor.data$alter <- egor.data$alter |> mutate(
        !!sym(ivar) := trimws(!!sym(ivar), whitespace = "[ ;\t\r\n]")
      )
    }
  }
  
  # Convert those alter variable totals to proportions, calculating the denominator
  egor.data$alter <- egor.data$alter |>
    group_by(.egoID) |>
    mutate(struct.alters = sum(duplicated(.altID) == FALSE),
           prop.denom = eval(parse(text = prop.denom))) |>
    mutate(across(.cols = starts_with("tot."), .fns = ~ .x / prop.denom, .names = "prop.{.col}")) %>%
    select(-prop.denom)
  names(egor.data$alter) <- gsub("prop.tot.", "prop.", names(egor.data$alter))
  
  
  ########################################
  ### Make means for numeric variables ###
  ########################################
  # NOTE: numeric variables may be labelled as EGO in the codebook,
  # But we want to use the data from the alter level to calculate means at the ego level
  
  # Only proceed if there is at least one numeric var that's not an identifier
  if("NUMERICAL" %in% codebook$Response.Type[! codebook$Subject.Type %in% "EGO_ID"] |
     ! is.null(additional.numeric.vars)){
    
    # Identify numeric variables that aren't identifiers, including those additionally specified
    numeric_vars <- c(codebook$Question.Title[codebook$Response.Type == "NUMERICAL" & (! codebook$Subject.Type %in% "EGO_ID")], additional.numeric.vars)
  
    # Check that all values are numeric for these variables
    numeric_error_test <- rep(0, length(numeric_vars))
    numeric_error_list <- vector("list", length(numeric_vars))

    for(i in 1:length(numeric_vars)) {
      numeric_uniques <- egor.data$alter |> select(numeric_vars[i]) |> pull() |> unique()
      numeric_test    <- ! is.na(suppressWarnings(as.numeric(numeric_uniques)))
      if(FALSE %in% numeric_test){
        numeric_error_test[i] <- 1
        numeric_error_list[[i]] <- numeric_uniques[numeric_test %in% FALSE]
      }
    }
    names(numeric_error_list) <- numeric_vars
    if(1 %in% numeric_error_test){
      numeric_error_vars <- numeric_vars[numeric_error_test == 1] |> paste(collapse = ", ")
    
      error_message <- paste0("Non-numeric values present in the following variable(s) specified as numeric: ", numeric_error_vars, 
                            ". The values are: ")
      stop(list(error_message, numeric_error_list))
    }
  
    # If all are good, continue processing
    for(i in 1:length(numeric_vars)) {
    
      ivar <- numeric_vars[i]
    
      # Drop all negative values, which represent missing responses, when calculating mean
      makemeans_text <- paste0("egor.data$alter <- egor.data$alter |> group_by(.egoID) |> mutate(", paste0("`mean.", ivar, "`"), 
                               " = mean(as.numeric(", ivar, "[", ivar,  ">= 0]), na.rm = TRUE))")
      eval(parse(text = makemeans_text))
    }
  }
  
  ##################################################################
  ### Move all tot, prop, and mean variables from $alter to $ego ###
  ##################################################################
  
  mergedata <- egor.data$alter |> 
    select(.egoID, starts_with("tot."), starts_with("prop."), starts_with("mean."), struct.alters) |>
    distinct()
  egor.data$ego <- egor.data$ego |>
    left_join(mergedata)
  egor.data$alter <- egor.data$alter |>
    select(-starts_with("tot."), -starts_with("prop."), -starts_with("mean."), -struct.alters)
  
  #################################################
  ### Make summary output, if address specified ###
  #################################################
  
  if(output.csv.address != "") {
    output.measures <- egor.data$ego |>
      select(Interview.ID, .egoID, starts_with("tot\\."), starts_with("prop\\."), starts_with("di\\."), starts_with("mean\\."), struct.alters) |>
      mutate(across(.cols = c(starts_with("prop\\."), starts_with("mean\\.")), .fns = function(x) {round(x, digits = 2)}))
    
    write.csv(output.measures, output.csv.address, row.names = FALSE)
  }
  
  return(egor.data)
}