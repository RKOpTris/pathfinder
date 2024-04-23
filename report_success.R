report_success <- function(){
  list(
    search_runtime <- global_vars$search_runtime,
    total_runs = global_vars$total_runs,
    best_distance_success = global_vars$best_distance_success,
    best_route = global_vars$successful_routes[[length(global_vars$successful_routes)]],
    successful_distances = global_vars$successful_distances,
    successful_routes = global_vars$successful_routes,
    successful_runs = global_vars$successful_runs,
    run_code = global_vars$run_code
  )
}