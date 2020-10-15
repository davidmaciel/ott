aglomera <- function(file, write = T, dest = NULL, cut = 3){
  medidas <- readr::read_rds(file)

  d <- medidas %>%
    dplyr::select(-name)
  clusters <- dist(d) %>% hclust()
  plot(clusters)
  medidas <- medidas %>%
    dplyr::mutate(cluster = cutree(clusters, k = cut))
  if (write == T) {
    readr::write_rds(medidas, path = dest)
  }
  medidas
}
