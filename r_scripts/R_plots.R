#________________________________________________________
# require libraries
  library(tidyverse)
#________________________________________________________

#________________________________________________________
# plot timeseries data by qc color
plot_timeseries_qc <- function() {
  
  p <- ggplot(field_temp_merged_all, aes(datetime, temp, color = qc)) +
         geom_line() +
         facet_wrap(~hh_id, ncol = 1, scales = "free") +
         theme_minimal() +
         theme(legend.position = "top") +
         ylab(first(units))
}
#________________________________________________________
