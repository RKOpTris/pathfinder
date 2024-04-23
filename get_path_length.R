get_path_length <- function(charvec){
  path_taken <- get_path(charvec)
  for(i in 1:nrow(path_taken)){
    path_taken$distance[i] <- get_distance(path_taken$p1[i], path_taken$p2[i])
  }
  sum(path_taken$distance)
}