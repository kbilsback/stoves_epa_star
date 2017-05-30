#________________________________________________________
# require libraries
  library(tidyverse)
#________________________________________________________

#________________________________________________________
# plot timeseries data (default by qc color)
# takes a data frame and a column name of variable to plot
field_timeseries_plot <- function(df, y_var, color_var = "qc", x_var = "datetime", facet_var = "hh_id", y_units = "units") {
  
  p <- ggplot(df, aes_string(y = y_var, x = x_var, color = color_var)) +
         geom_line() +
         scale_fill_discrete(drop = FALSE) +
         facet_wrap(~df[[facet_var]], ncol = 1, scales = "free") +
         theme_minimal() +
         theme(legend.position = "top") +
         ylab(paste0(y_var, " (", df[[1, y_units]], ")")) +
         xlab(x_var)

  # return plot
  return(p)

}
#________________________________________________________

#________________________________________________________
# plot timeseries data by a fill color
# takes a data frame and a column name of variable to plot
field_boxplot <- function(df, y_var, fill_var = "qc", x_var = "hh_id", y_units = "units") {

  p <-   ggplot(df, aes_string(y = y_var, x = x_var, fill = fill_var)) +
          geom_boxplot() +
          scale_fill_discrete(drop = FALSE) +
          theme_minimal() +
          theme(legend.position = "top") +
          ylab(paste0(y_var, " (", df[[1, y_units]], ")")) +
          xlab(x_var)

  # return plot
  return(p)
  
}
#________________________________________________________
