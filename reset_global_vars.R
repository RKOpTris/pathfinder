reset_global_vars <- function(start_waypoint){
  global_vars$total_runs <- 0
  global_vars$total_attempts <- 0
  global_vars$max_runs <- 100000
  global_vars$successful_routes <- list()
  global_vars$successful_distances <- numeric()
  global_vars$successful_runs <- numeric()
  global_vars$run_code <- character()
  global_vars$start_waypoint <- start_waypoint
  global_vars$max_visits <- 3
  global_vars$number_successes <- 0
  global_vars$number_fails <- 0
  global_vars$best_distance_success <- Inf
  global_vars$search_runtime <- 0
}