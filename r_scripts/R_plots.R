#________________________________________________________
# require libraries
  library(tidyverse)
#________________________________________________________

#________________________________________________________
# plot timeseries data by qc color
# takes a data fram with columns labeled var and value with variable and value to plot
plot_timeseries_qc <- function(df) {
  
  p <- ggplot(df, aes(datetime, value, color = qc)) +
         geom_line() +
         facet_wrap(~hh_id, ncol = 1, scales = "free") +
         theme_minimal() +
         theme(legend.position = "top") +
         ylab(paste(first(var), first(units)))
  # return plot
  return(p)

}
#________________________________________________________

#________________________________________________________
# plot timeseries data by qc color
# takes a data fram with columns labeled var and value with variable and value to plot
boxplot_timeseries_qc <- function(df) {
  
  p <- ggplot(df, aes(value, color = first(qc))) +
    geom_line() +
    facet_wrap(~hh_id, ncol = 1, scales = "free") +
    theme_minimal() +
    theme(legend.position = "top") +
    ylab(paste(first(var), first(units)))
  # return plot
  return(p)
  
}
#________________________________________________________

