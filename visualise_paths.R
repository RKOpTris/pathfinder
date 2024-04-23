visualise_paths <- function(){
  plot(y ~ x, my_points, type = "n", las = 1)
  text(y ~ x, my_points, labels = waypoint, cex = 2)
  
  a2b <- my_paths
  for(i in 1:nrow(a2b)){
    p1 <- a2b$p1[i]
    p2 <- a2b$p2[i]
    a <- get_pos(my_points, p1)
    b <- get_pos(my_points, p2)
    points(a[1], a[2], col = "red", pch = 16)
    arrows(a[1], a[2], b[1], b[2], length = 0, lty = 2)
  }
}