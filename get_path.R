get_path <- function(charvec){
  data.frame(p1 = charvec[1:(length(charvec) - 1)],
             p2 = charvec[2:length(charvec)])
}