#' Takes centrality measures of actors in the network
#'
#' @param file String. File to read.
#' @param write Boolean. Wheter or not to save the results to a .rds file in the working directory.
#'
#' @return
#' @export
#'
tira_medida <- function(file, write = T){
  edges <- readr::read_csv2(file) %>%
    dplyr::select("source" = from, "target" = to) %>%
    dplyr::distinct()

graf <- igraph::graph_from_data_frame(edges)
net <- intergraph::asNetwork(graf)
t <- tibble::tibble(
  name = igraph::V(graf)$name,
  degree = igraph::degree(graf, mode = "all", normalized = T),
  eigen = igraph::eigen_centrality(graf, directed = T)$vector,
  betweenness = igraph::betweenness(graf, directed = T, normalized = T),
  closeness = igraph::closeness(graf, mode = "total", normalized = T)
  # flowbet = sna::flowbet(net, rescale = T),
  # clustering = igraph::transitivity(graf, type = "local")
  ) %>%
  dplyr::mutate(dplyr::across(where(is.numeric), round, 3))
if(write == T) {
readr::write_rds(t, path = "medidas.rds")
}
t
}
