fail_run <- function(record_all_codes){
  #message(paste("Search failed because", run_vars$fail_reason))
  #message(global_vars$total_runs)
  global_vars$number_fails <- global_vars$number_fails + 1
  if(record_all_codes){
    error_code <- ifelse(run_vars$fail_reason == "exceeded best distance", "F-XD", "F-MW")
    global_vars$run_code <- c(global_vars$run_code, error_code)
  }
  reset_run_vars()
  global_vars$total_attempts <- global_vars$total_attempts + 1
}