generate_paths <- function(){
  waypoints <- my_points$waypoint
  p1 <- c()
  while(length(unique(p1)) < length(waypoints)){
    p1 <- c(p1, sample(waypoints, 1))
  }
  p2 <- sample(waypoints, length(p1), replace = T)
  paths <- data.frame(p1, p2)
  #remove p1 == p2
  paths <- paths[-which(paths$p1 == paths$p2), ]
  row.names(paths) <- NULL
  #remove duplicates
  duplicates <- c()
  for(i in 1:(nrow(paths) - 1)){
    p2 <- paths$p1[i]
    p1 <- paths$p2[i]
    rem <- paths[(i + 1):nrow(paths), ]
    dup <- rem[rem$p1 == p1 & rem$p2 == p2, ]
    if(length(dup) < 0){
      next
    } else {
      duplicates <- c(duplicates, as.numeric(row.names(dup)))
    }
  }
  paths <- paths[-duplicates, ]
  paths
}