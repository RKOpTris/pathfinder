init_paths <- function(){
  my_paths <- generate_paths()
  distances <- my_points %>% tibble::column_to_rownames("waypoint") %>% dist(upper = T, diag = T) %>% as.matrix()
  for(i in 1:nrow(my_paths)){
    my_paths$distance[i] <- get_distance(my_paths$p1[i], my_paths$p2[i])
  }
  my_paths
}