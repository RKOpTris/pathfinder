str_to_waypoints <- function(string){
  string %>% str_split("") %>% unlist()
}