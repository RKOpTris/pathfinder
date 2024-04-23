plot_path <- function(charvec){
  path_taken <- get_path(charvec)
  p1 <- path_taken$p1
  p2 <- path_taken$p2
  path_taken1 <- my_points[match(p1, my_points$waypoint), ]
  path_taken2 <- my_points[match(p2, my_points$waypoint), ]
  plot_cols <- scico::scico(length(charvec), palette = "hawaii")
  arrows(path_taken1$x, path_taken1$y, path_taken2$x, path_taken2$y, col = plot_cols, lwd = 3)
  points(path_taken1$x[1], path_taken1$y[1], cex = 8, lwd = 3)
}