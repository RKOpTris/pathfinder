error_code_for_plot <- function(error_code_summary_data){
  do.call(rbind, lapply(1:length(error_code_summary_data), function(x){
    y <- error_code_summary_data[[x]]
    y$Run <- x
    y
  }))
}