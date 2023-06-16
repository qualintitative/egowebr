#' Process EgoWeb export data
#' 
#' Takes .csv data exported from EgoWeb, removes certain undesirable 
#' observations, and can optionally return the data structured as
#' an egor-class object to cooperate with the egor package.
#' 
#' @param alter.csv A .csv file containing alter-level data.
#' @param edges.csv A .csv file containing alter-alter data.
#' @param egos.csv A .csv file containing ego-level data, optional.
#' @param ID.vars A vector specifying the identifier variable for each of: ego, alter, source of an alter-alter link, and target of an alter-alter link.
#' @param alter.drops An optional string specifying a condition to identify records that should be dropped from the alter-level data.
#' @param edges.drops An optional string specifying a condition to identify records that should be dropped from the alter-alter data.
#' @param egos.drops An optional string specifying a condition to identify records that should be dropped from the ego-level data.
#' @param return.egor A logical value to indicate whether the EgoWeb data should be restructured and returned as an egor class object. Defaults to TRUE.
#' @param ego.vars An optional vector listing variables from the ego-level dataset that should be merged onto the alter-level data.
#' @export
process_egoweb <- function(alter.csv = "", edges.csv = "", egos.csv = NULL,
                           ID.vars = list(ego = "EgoID", 
                                          alter = "Alter.Number", source = "Alter.1.Number", 
                                          target = "Alter.2.Number"),
                           alter.drops = "",
                           edges.drops = "",
                           egos.drops = "",
                           return.egor = TRUE,
                           ego.vars = NULL,
                           ...){
  
  # Check that if egos.csv is not specified, no egos.drops is given
  if(is.null(egos.csv) & egos.drops != ""){
    stop("Cannot specify egos.drops without providing an egos.csv dataset")
  }
  
  # Create ID vars list and read files
  IDv <- modifyList(eval(formals()$ID.vars), ID.vars)
  alters.df <- read.csv(file = alter.csv)
  edges     <- read.csv(file = edges.csv)
  
  # Construct the code for the specified drop conditions and execute
  if(alter.drops != "") {
    alter.drops <- paste0("alters.df <- alters.df |> filter(! ", alter.drops, ")")
    eval(parse(text = alter.drops))
  }
  if(edges.drops != "") {
    edges.drops <- paste0("edges <- edges |> filter(! ", edges.drops, ")")
    eval(parse(text = edges.drops))
  }
  if(egos.drops != "") {
    egos.drops <-  paste0("egos  <- egos  |> filter(! ", egos.drops, ")")
    eval(parse(text = egos.drops))
  }
  
  
  # Remove records from alters that we never want
  alters.df <- alters.df |>
    filter(! (!!sym(IDv$alter)) == 0) |>
    # Also delete cases where there was one or fewer alters in the attribute file -- no network data
    #group_by(!!sym(IDv$ego)) |>
    #filter(length(unique(!!sym(IDv$alter))) >= 2) |>
    ungroup()
  
  # Stop here if return.egor = FALSE
  if(return.egor == FALSE){
    return(list(alters = alters.df, edges = edges))
  } else {
    
    if (! is.null(egos.csv)) {
      egos <- read.csv(file = egos.csv)
    } else {
      alterID.index <- which(names(alters.df) == IDv$alter)
      
      # Deduplicate the alters file down to singular EgoID
      egos <- alters.df[1:(alterID.index - 1)] |>
        distinct()
      # If there are any duplicate EgoID's still, then stop. Structure needs to be changed
      if(TRUE %in% (egos |> select(!!sym(IDv$ego)) |> pull() |> duplicated())){
        stop("At least one variable in alters.df left of alter ID is not unique on Ego ID")
      }
    }  
    
    # Order and add ego variables to alters data if not null
    egos <- egos[order(egos[[IDv$ego]]),]
    
    #egos <- egos[order(as.numeric(egos[[IDv$ego]])), ]
    alters.df <- alters.df[order(alters.df[[IDv$ego]],
                                 alters.df[[IDv$alter]]), ]
    
    if (! is.null(ego.vars)) {
      message("Adding ego variables to alters data.")
      alters.df <- merge(alters.df, egos[c(IDv$ego, ego.vars)], 
                         by = IDv$ego)
      alters.df <- alters.df[order(alters.df[[IDv$ego]],
                                   alters.df[[IDv$alter]]), ]
    }
    
    # Starting the egor() function processing here:
    id_vars <- list(alters.df[[IDv$ego]], alters.df[[IDv$alter]], egos[[IDv$ego]], 
                    edges[[IDv$ego]], edges[[IDv$source]], edges[[IDv$target]])
    id_vars <- id_vars[!is.null(id_vars)]
    all_numeric <- all(purrr::map_lgl(id_vars, is.numeric))
    if (!all_numeric & !all(purrr::map_lgl(id_vars, is.character))) {
      alters.df[[IDv$ego]] <- as.character(alters.df[[IDv$ego]])
      if (!is.null(alters.df[[IDv$alter]])) 
        alters.df[[IDv$alter]] <- as.character(alters.df[[IDv$alter]])
      if (!is.null(egos)) 
        egos[[IDv$ego]] <- as.character(egos[[IDv$ego]])
      if (!is.null(edges)) {
        edges[[IDv$ego]] <- as.character(edges[[IDv$ego]])
        edges[[IDv$source]] <- as.character(edges[[IDv$source]])
        edges[[IDv$target]] <- as.character(edges[[IDv$target]])
      }
    }
    
    if (!tibble::is_tibble(alters.df)) {
      alters.df <- as_tibble(alters.df)
    }
    alters.df <- select(alters.df, `:=`(!!IDv$alter, if (!is.null(edges) || 
                                                         IDv$alter %in% colnames(alters.df)) 
      !!IDv$alter), `:=`(!!IDv$ego, !!IDv$ego), everything())
    # I don't think egos will ever be null here, but keeping this just in case
    if (is.null(egos)) {
      egos <- tibble(.egoID = unique(alters.df[[IDv$ego]]))
    } else {
      if (!tibble::is_tibble(egos)) {
        egos <- as_tibble(egos)
      }
      egos <- select(egos, `:=`(!!IDv$ego, !!IDv$ego), 
                     everything())
    }
    if (is.null(edges)) {
      if (all_numeric) {
        edges <- tibble(.egoID = 0, .srcID = 0, .tgtID = 0)[0, 
                                                            ]
      } else {
        edges <- tibble(.egoID = "0", .srcID = "0", 
                        .tgtID = "0")[0, ]
      }
    } else {
      if (!tibble::is_tibble(edges)) {
        edges <- as_tibble(edges)
      }
      edges <- select(edges, `:=`(!!IDv$ego, !!IDv$ego), 
                      `:=`(!!IDv$source, !!IDv$source), `:=`(!!IDv$target, 
                                                             !!IDv$target), everything())
    }
    
    if (any(duplicated(egos[[IDv$ego]]))) 
      stop("Duplicated ego IDs in `ego` data.", call. = FALSE)
    if (!all(alters.df[[IDv$ego]] %in% egos[[IDv$ego]])) 
      stop("There is at least one ego ID in the `alter` data with no corresponding entry in the `ego` data.", 
           call. = FALSE)
    if (!all(c(edges[[IDv$ego]] %in% egos[[IDv$ego]]))) 
      stop("There is at least one ego ID in the `alter-alter` data with no corresponding entry in the `alter` data.", 
           call. = FALSE)
    
    # Check for whether there are any alters in the aaties file that aren't in the alter data
    alter_aatie_ids_consistent <- purrr::map_lgl(egos[[IDv$ego]], 
                                                 function(x) {
                                                   sym_ego_id <- rlang::sym(IDv$ego)
                                                   alter <- filter(alters.df, !!sym_ego_id == x)
                                                   aatie <- filter(edges, !!sym_ego_id == x)
                                                   all(c(aatie[[IDv$source]], aatie[[IDv$target]]) %in% 
                                                         alter[[IDv$alter]])
                                                 })
    if (!all(alter_aatie_ids_consistent)) 
      stop("There is at least one alter referenced in the `alter-alter` data that is not listed in the `alter` data. Errors were found for egos: ", 
           paste(egos[[IDv$ego]][!alter_aatie_ids_consistent], 
                 collapse = " "), call. = FALSE)
    
    egos <- egos |>
      rename(.egoID = !!sym(IDv$ego))
    
    alters.df <- alters.df |>
      rename(.egoID = !!sym(IDv$ego),
             .altID = !!sym(IDv$alter)) |>
      # Make .egoID and .altID the first two columns; otherwise as_igraph() from egor doesn't work
      relocate(.egoID, .altID)
    
    edges <- edges |>
      rename(.egoID = !!sym(IDv$ego),
             .srcID = !!sym(IDv$source),
             .tgtID = !!sym(IDv$target)) |>
      relocate(.egoID, .srcID, .tgtID)
    
    egor_res <- list(ego = egos, alter = alters.df, aatie = edges)
    class(egor_res) <- c("egor", class(egor_res))
    
    # Design stuff that I'm not using right now
    #egor_res$ego <- .gen.ego_design(egor_res, ego_design, parent.frame())
    #alter_design(egor_res) <- alter_design
    egor::activate(egor_res, "ego")
    return(egor_res)
  }
}