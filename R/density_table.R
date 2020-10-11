density_table <- function(net, cluster, k){
  clusters <- cutree(cluster, k)
  net <-
    network::set.vertex.attribute(net, attrname = "cluster", clusters)
  df <- intergraph::asDF(net)
  edges <- df$edges
  nodes <- df$vertexes
  edges <-
    edges %>% dplyr::left_join(nodes, by = c("V1" = "intergraph_id")) %>%
    dplyr::left_join(
      nodes,
      by = c("V2" = "intergraph_id"),
      suffix = c("_source", "_target")
    )

  cluster_sizes <- nodes %>% dplyr::group_by(cluster) %>% dplyr::count()
  edges %>% dplyr::group_by(cluster_source, cluster_target) %>%
    dplyr::count() %>%
    dplyr::rename("ties" = n) %>%
    dplyr::left_join(cluster_sizes, by = c("cluster_source" = "cluster")) %>%
    dplyr::left_join(
      cluster_sizes,
      by = c("cluster_target" = "cluster"),
      suffix = c("_source", "_target")
    ) %>%
    dplyr::mutate(n = n_source * n_target,
           prop = ties/n) %>%
    dplyr::select(cluster_source, cluster_target, ties,n, prop)

  }
