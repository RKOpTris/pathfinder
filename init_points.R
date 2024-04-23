init_points <- function(n_points = 10, my_seed = 1138, seed_n = 2){
  set.seed(my_seed)
  for(i in 1:seed_n){
    my_points <- generate_points(n_points, 0)
  }
  my_points
}