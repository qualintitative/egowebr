#' Define New Variables for an Egor Object
#' 
#' Provides a single function for defining new variables at the ego-, alter-, 
#' and aatie-levels of an egor object.
#' 
#' @param egor.data An egor object.
#' @param ego.formula An optional expression to be executed within a dplyr::mutate to define new ego-level variables.
#' @param alter.formula An optional expression to be executed within a dplyr::mutate to define new alter-level variables.
#' @param aatie.formula An optional expression to be executed within a dplyr::mutate to define new aatie-level variables.
#' @export
define_vars <- function(egor.data, ego.formula = list(""), alter.formula = list(""), aatie.formula = list("")) {
  
  if(! "egor" %in% class(egor.data)){
    stop("egor.data must be an egor-type object.")
  }
  
  ###########################
  ### Apply Ego Formulas  ###
  ###########################
  
  if(TRUE %in% (ego.formula != "")){
    
    # Activate the alter level
    egor.data <- egor.data |> egor::activate(ego)
    
    # Mutate the variable(s)
    ego_form_text <- paste0("egor.data <- egor.data |> mutate(", ego.formula, ")")
    eval(parse(text = ego_form_text))
  }
  
  #############################
  ### Apply Alter Formulas  ###
  #############################
  
  if(TRUE %in% (alter.formula != "")){
    
    # Activate the alter level
    egor.data <- egor.data |> egor::activate(alter)
    
    # Mutate the variable(s)
    alter_form_text <- paste0("egor.data <- egor.data |> mutate(", alter.formula, ")")
    eval(parse(text = alter_form_text))
  }
  
  ############################
  ### Apply Aatie Formulas ###
  ############################
  
  if(TRUE %in% (aatie.formula != "")){
    
    # Activate the aatie level
    egor.data <- egor.data |> egor::activate(aatie)
    
    # Mutate the variable(s)
    aatie_form_text <- paste0("egor.data <- egor.data |> mutate(", aatie.formula, ")")
    eval(parse(text = aatie_form_text))
  }
    
  return(egor.data)
}