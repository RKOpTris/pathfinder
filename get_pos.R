get_pos <- function(x, waypoint){
  as.numeric(x[x$waypoint == waypoint, c("x", "y")])
}