error_code_summary <- function(run_object){
  start_runs <- which(run_object$run_code == "S") - 1
  run_analysis <- run_object$run_code %>% paste(collapse = ",") %>% str_split(",S,") %>% sapply(str_split, ",")
  run_analysis <- sapply(run_analysis, table)
  lapply(run_analysis, function(x){
    y <- x %>% data.frame()
    names(y) <- c("Error", "Frequency")
    y <- y[y$Error != "S", ]
    y$Proportion <- y$Frequency / sum(y$Frequency)
    y
  })
}