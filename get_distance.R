get_distance <- function(p1, p2){
  distances <- my_points %>% tibble::column_to_rownames("waypoint") %>% dist(upper = T, diag = T) %>% as.matrix()
  distances[p1, p2]
}