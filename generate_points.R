generate_points <- function(n, mean = 0, sd = 1){
  data.frame(waypoint = letters[1:n],
             x = rnorm(n, mean, sd),
             y = rnorm(n, mean, sd))
}