
make_rege <- function(file, relation, plotar = T, method = "complete"){
edges <- readr::read_csv2("data-raw/vínculos.csv") %>%
  dplyr::distinct()
edges <- edges %>% filter(title == relation) %>%
  rename("source" = from, "target" = to) %>% select(-title)
nodes <- unique(c(edges$source, edges$target))
g <- igraph::graph_from_data_frame(edges, vertices = nodes)
net <- intergraph::asNetwork(g)
reg <- sna::redist(net) %>% as.dist()
clust <- hclust(reg, method = method)
if(plotar == T){
  plot(clust)
}
list(network = net, dist = reg, cluster = clust)
}


