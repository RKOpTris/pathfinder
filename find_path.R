find_path <- function(start_waypoint = "a", max_visits = 3, max_runs = 100000, record_all_codes = T, visit_penalty = 0.25){
  start_time <- Sys.time()
  reset_run_vars()
  reset_global_vars(start_waypoint)
  global_vars$start_waypoint <- start_waypoint
  global_vars$max_runs <- max_runs
  global_vars$max_visits <- max_visits
  #while(global_vars$total_runs < global_vars$max_runs){
  while(global_vars$number_successes < 20 & global_vars$total_runs < global_vars$max_runs){
    
    if(global_vars$total_runs == 0){
      reset_run_vars()
    }
    
    global_vars$total_runs <- global_vars$total_runs + 1
    run_vars$search_runs <- run_vars$search_runs + 1
    
    # travelling loop
    if(run_vars$distance_travelled == 0){
      (run_vars$current_waypoint <- global_vars$start_waypoint)
      run_vars$waypoint_probs[run_vars$current_waypoint, 1] <- run_vars$waypoint_probs[run_vars$current_waypoint, 1] * visit_penalty
    } else {
      (run_vars$current_waypoint <- run_vars$route_travelled[length(run_vars$route_travelled)])
      run_vars$waypoint_probs[run_vars$current_waypoint, 1] <- run_vars$waypoint_probs[run_vars$current_waypoint, 1] * visit_penalty
      ##### this may not be the best place to put this!
      if(run_vars$distance_travelled > global_vars$best_distance_success){
        run_vars$fail_reason <- "exceeded best distance"
        fail_run(record_all_codes)
        next
      }
    }
    
    successful_runs <- length(global_vars$successful_distances)
    if(successful_runs > 4 && length(table(global_vars$successful_distances[length(global_vars$successful_distances):(length(global_vars$successful_distances) - 2)])) == 1){
      report_success()
      print("Best solution likely found!")
      break
    }
    
    if(length(table(run_vars$route_travelled)) == nrow(my_points)){
      run_vars$all_points_visited <- T
    }
    
    if(run_vars$all_points_visited && run_vars$current_waypoint == global_vars$start_waypoint){
      global_vars$best_distance_success <- run_vars$distance_travelled
      succeed_run(record_all_codes)
      next
    }
    if(length(run_vars$route_travelled) > 0){
      if(any(table(run_vars$route_travelled) > global_vars$max_visits)){
        run_vars$fail_reason <- "visiting waypoint too many times"
        fail_run(record_all_codes)
        next
      }
    }
    # get waypoint options and select one at random
    all_paths <- rbind(my_paths,
                       rename(my_paths[c(2, 1, 3)], p1 = p2, p2 = p1))
    (possible_destinations <- all_paths %>% filter(p1 == run_vars$current_waypoint))
    run_vars$waypoint_probs
    (weighted_probs <- run_vars$waypoint_probs[possible_destinations$p2, 1])
    (weighted_probs <- weighted_probs / sum(weighted_probs))
    # travel
    (chosen_destination <- possible_destinations[sample(1:nrow(possible_destinations), 1, prob = weighted_probs), ])
    # update route_travelled
    (run_vars$route_travelled <- c(run_vars$route_travelled, chosen_destination$p2))
    # update distance_travelled
    (run_vars$distance_travelled <- run_vars$distance_travelled + chosen_destination$distance)
  }
  global_vars$search_runtime <- Sys.time() - start_time
  report_success()
}