# Source - https://stackoverflow.com/a/77410246
# Posted by Allan Cameron, modified by community. See post 'Timeline' for change history
# Retrieved 2026-05-27, License - CC BY-SA 4.0

library(igraph)
library(data.tree)
library(tidygraph)
library(ggraph)

dirs <- list.dirs(path = ".", full.names = TRUE, recursive = TRUE)
dirs <- dirs[!grepl("/\\.git($|/)", dirs)]
dirs <- dirs[!grepl("/\\.Rproj.user($|/)", dirs)]

do.call('rbind', 
  strsplit(dirs, '/') |>
  lapply(\(x) sapply(seq_along(x), \(y) paste(x[1:y], collapse = '/'))) |>
  lapply(\(x) cbind(head(x, -1), tail(x, -1)))
  ) |>
  as.data.frame() |>
  unique() |>
  graph_from_data_frame() |>
  as_tbl_graph() %>%
  mutate(label = gsub('^.*/(.*)$', '\\1', name)) |>
  ggraph(layout = 'tree') + 
  geom_edge_diagonal(color = 'gray') +
  geom_node_point(shape = 21, fill = 'lightblue') +
  geom_node_text(aes(label = label), size = 3, nudge_x = 0.4) +
  coord_flip(clip = 'off') +
  scale_y_reverse() +
  theme_graph()
