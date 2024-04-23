succeed_run <- function(record_all_codes){
  message(paste0(global_vars$total_runs, ": path found!"))
  global_vars$number_successes <- global_vars$number_successes + 1
  global_vars$successful_routes[[global_vars$number_successes]] <- c(global_vars$start_waypoint, run_vars$route_travelled)
  global_vars$successful_distances[global_vars$number_successes] <- run_vars$distance_travelled
  global_vars$successful_runs <- c(global_vars$successful_runs, global_vars$total_runs)
  if(record_all_codes){
    global_vars$run_code <- c(global_vars$run_code, "S")
  }
  reset_run_vars()
  global_vars$total_attempts <- global_vars$total_attempts + 1
}