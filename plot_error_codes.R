plot_error_codes <- function(x){
  x %>% 
    error_code_summary %>% 
    error_code_for_plot %>%
    ggplot(aes(Run, Proportion, fill = Error)) + 
    scale_y_continuous(breaks = seq(0, 1, 0.1)) +
    scale_x_continuous(breaks = seq(1, length(x$successful_runs))) +
    geom_bar(stat = "identity", position = "fill") +
    theme_bw() +
    theme(axis.title = element_text(size = 25),
          axis.text = element_text(size = 15),
          legend.title = element_text(size = 25),
          legend.text = element_text(size = 15))
}