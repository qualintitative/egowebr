#' Construct Network Measures
#' 
#' Adds commonly used network data measures onto each subnetwork level of 
#' the input egor object. 
#' 
#' @param egor.data An egor object.
#' @export
construct_measures <- function(egor.data) {
  
  EffectiveSize <- function(x){
    e <- igraph::ecount(x)
    v <- igraph::vcount(x)
    v - 2*e / v
  }
  
  BrokerageWrapper <- function(x){
    v <- igraph::vcount(x)
    e <- igraph::ecount(x)
    pe <- v*(v-1)/2
    ne <- pe - e
    br <- ne/pe
  }
  
  ShortestPathWrapper <- function(x){
    sp <- igraph::shortest.paths(x)
    msp <- mean(sp[is.finite(sp)])
  }
  
  # Calculate these measures for each subnetwork level
  for(i in 1:length(egor.data)) {
    
    # Convert to list of network graphs for use with igraph functions
    egor.data_graphs <- egor::as_igraph(egor.data[[i]])
    
    ###########################################
    ### Calculation of alter-level measures ###
    ###########################################
    egor.data_constraint  <- lapply(egor.data_graphs, function(x) {igraph::constraint(x)}) |> stack() |>
      select(values) |>
      rename(struct.constraint = values)
    egor.data_degree  <- lapply(egor.data_graphs, function(x) {igraph::degree(x)}) |> stack() |>
      select(values) |>
      rename(struct.degree = values)
    egor.data_eigen   <- lapply(egor.data_graphs, function(x) {igraph::evcent(x)$vector}) |> stack() |>
      select(values) |>
      rename(struct.eigen = values)
    egor.data_between <- lapply(egor.data_graphs, function(x) {igraph::betweenness(x)}) |> stack() |>
      select(values) |>
      rename(struct.between = values)
    
    egor.data[[i]]$alter <- egor.data[[i]]$alter |>
      cbind(egor.data_constraint, egor.data_degree, egor.data_eigen, egor.data_between)
    
    
    #########################################
    ### Calculation of ego-level measures ###
    #########################################
    egor.data_density <- lapply(egor.data_graphs, function(x) {igraph::graph.density(x)}) |> stack() |>
      select(values) |>
      rename(struct.density = values)
    egor.data_components <- lapply(egor.data_graphs, function(x) {igraph::clusters(x, mode = "strong")$no}) |> stack() |>
      select(values) |>
      rename(struct.components = values)
    egor.data_transitivity <- lapply(egor.data_graphs, function(x) {igraph::transitivity(x, type = c("global"))[1]}) |> stack() |>
      select(values) |>
      rename(struct.transitiviy = values)
    egor.data_degreecent <- lapply(egor.data_graphs, function(x) {igraph::centralization.degree(x)$centralization}) |> stack() |>
      select(values) |>
      rename(struct.degree.cent = values)
    egor.data_betweencent <- lapply(egor.data_graphs, function(x) {igraph::centralization.betweenness(x)$centralization}) |> stack() |>
      select(values) |>
      rename(struct.between.cent = values)
    egor.data_diameter <- lapply(egor.data_graphs, function(x) {igraph::diameter(x)}) |> stack() |>
      select(values) |>
      rename(struct.diameter = values)
    egor.data_eff.size <- lapply(egor.data_graphs, function(x) {EffectiveSize(x)}) |> stack() |>
      select(values) |>
      rename(struct.eff.size = values)
    egor.data_brokerage <- lapply(egor.data_graphs, function(x) {BrokerageWrapper(x)}) |> stack() |>
      select(values) |>
      rename(struct.brokerage = values)
    egor.data_shortest.path <- lapply(egor.data_graphs, function(x) {ShortestPathWrapper(x)}) |> stack() |>
      select(values) |>
      rename(struct.shortest.path = values)
    
    egor.data[[i]]$ego <- egor.data[[i]]$ego |>
      cbind(egor.data_density, egor.data_components, egor.data_transitivity, egor.data_degreecent, egor.data_betweencent,
            egor.data_diameter, egor.data_eff.size, egor.data_brokerage, egor.data_shortest.path)
  }
  
  return(egor.data)
}