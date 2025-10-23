#' Drop Egos or Alters from all levels of an Egor object
#' 
#' Provides a function for removing data for specified egos, alters of an egor object
#' 
#' @param egor.data An egor object.
#' @param ego.drops A vector of egos specified by .egoID for which all data should be dropped
#' @param alter.drops A vector of alters specified with the format ".egoID|.altID" for which all data should be dropped
#' @export
dropIDs <- function(egor.data, ego.drops, alter.drops) {
  
  # Define function to filter out those egos, alters
  filter_df <- function(df) {
    # To remove egos
    if(! all(is.na(ego.drops))) {
      df <- df[df$.egoID %in% ego.drops == FALSE, ]
    }
    
    if(! all(is.na(alter.drops))) {
      # To remove alters, first figure out which dataframe is being hit
      if(! ".altID" %in% names(df) & ! ".srcID" %in% names(df)) {     # If neither .altID nor .srcID exists, then this is the $ego dataframe - nothing to remove
        df <- df
      } else if(".altID" %in% names(df)) {                            # Else if .altID exists, this is the $alter dataframe
        df$.cID <- paste0(df$.egoID, "|", df$.altID)
        df <- df[df$.cID %in% alter.drops == FALSE, ]
      } else {                                                        # Else this is the $aatie dataframe
        df$.cID.src <- paste0(df$.egoID, "|", df$.srcID)
        df$.cID.tgt <- paste0(df$.egoID, "|", df$.tgtID)
        df <- df[df$.cID.src %in% alter.drops == FALSE, ]
        df <- df[df$.cID.tgt %in% alter.drops == FALSE, ]
      }
    }
    df
  }
  
  # Helper to preserve class and attributes when applying
  lapply_preserve_class <- function(x, FUN) {
    cl <- class(x)
    at <- attributes(x)
    out <- lapply(x, FUN)
    attributes(out) <- at
    class(out) <- cl
    out
  }
  
  filtered.network <- lapply(egor.data, function(x) lapply_preserve_class(x, filter_df))
  
  filtered.network
}