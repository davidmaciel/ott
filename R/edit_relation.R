edit_relation <-
  function(file,
           from,
           to,
           title = c("Dá ordens para",
                     "Trabalha com",
                     "Fornece drogas para",
                     "Intimo de",
                     "protege"),
           edit = c("add", "remove"),
           write = T) {
    edit <- match.arg(edit)
    title <- match.arg(title)
    edges <- readr::read_csv2(file) %>%
      dplyr::distinct()

    user_edges <- dplyr::tibble("from" = from,
                                "to" = to,
                                "title" = title)

    if (edit == "add") {
      edges <- dplyr::bind_rows(edges, user_edges)
    }

    if (edit == "remove") {
      edges <- dplyr::anti_join(edges, user_edges)
    }
    if (write == T) {
      edges %>% readr::write_csv2(path = file, col_names = T)
    }
    edges
  }
