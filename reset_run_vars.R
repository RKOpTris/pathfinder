reset_run_vars <- function(){
  run_vars$current_waypoint <- NA
  run_vars$distance_travelled <- 0
  run_vars$route_travelled <- c()
  run_vars$all_points_visited <- F
  run_vars$fail_reason <- ""
  run_vars$search_runs <- 0
  run_vars$waypoint_probs <- matrix(1, nrow = nrow(my_points), dimnames = list(my_points$waypoint))
}