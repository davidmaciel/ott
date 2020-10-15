
#' Exploration plot
#'
#' @param file Path to csv file of edges. Must be without header.
#' @param filtro String. Relation to filter the multiplex network. One of predefined options.
#'
#' @return a visNetwork object
#' @export
#'
explora <- function(file, filtro = c("Dá ordens para", "Trabalha com", "Fornece drogas para", "Íntimo de",
                                     "Protege", "tudo", "Compra drogas de")){
  filtro <- match.arg(filtro)

edges <- readr::read_csv2(file) %>%
  dplyr::distinct()

nodes <- dplyr::tibble(
  id = sort(unique(c(edges$from, edges$to))),
  title = sort(paste0("<b>",unique(c(edges$from, edges$to)), "<b>"))

)
if(filtro != "tudo"){
  edges <- edges %>%
    dplyr::filter(title == filtro)
}
 edges <- edges %>%
  dplyr::mutate(
   arrows = "to",
   title = paste0("<i>", title, "<i>"),
 )



visNetwork::visNetwork(nodes, edges, main = filtro, height = "700px", width = "100%") %>%
  visNetwork::visIgraphLayout(layout = "layout_with_fr", randomSeed = 22) %>%
  visNetwork::visEdges(color = list(highlight = "darkblue", hover = "darkblue"),hoverWidth = 2, smooth = T) %>%
  visNetwork::visOptions(highlightNearest = list(enabled = T,
                                                 degree = 1,
                                                 labelOnly = F,
                                                 hover = T),
                         nodesIdSelection = T,
                         manipulation = T)

}


