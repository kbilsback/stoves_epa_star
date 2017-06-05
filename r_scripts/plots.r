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

#________________________________________________________
# plot field data
field_pointplot <- function(df, y_var, color_var = "qc", x_var = "hh_id", y_units = "units") {
  
  p <- ggplot(df, aes_string(y = y_var, x = x_var, color = color_var)) +
        geom_point() +
        scale_fill_discrete(drop = FALSE) +
        theme_minimal() +
        theme(legend.position = "top") +
        ylab(paste0(y_var, " (", df[[1, y_units]], ")")) +
        xlab(x_var)
  
  # return plot
  return(p)
  
}
#________________________________________________________

#________________________________________________________
# plot stove fuel combinations tested
plot_test_list <- function(test_list) {
  
  p <- ggplot(test_list, aes(y = fuel_type, x = stove_type)) + 
       geom_tile(colour = "white", width = 0.9, height = 0.9, aes(fill = field_site)) +
       scale_fill_discrete(na.value = 'grey95') +
       theme_minimal() +
       theme(legend.position = "none") +
       theme(axis.text.x = element_text(angle = 35, hjust = 0.95)) +
       theme(axis.text = element_text(size = 14)) +
       geom_text(aes(label = hh_id, size = 8)) +
       xlab("") + ylab("")
  
  # return plot
  return(p)
  
}
#________________________________________________________

#________________________________________________________
# plot stove fuel combinations tested
#plot_meta_hist <- function(df, var, x_lab = var, bwidth = 15) {

#  p <- ggplot(df, aes_string(var)) +
#        geom_histogram(binwidth = bwidth, stat = "count") +
#        theme_minimal() +
#        theme(legend.position = "top") +
#        xlab(x_lab)

  # return plot
#  return(p)

#}
#________________________________________________________

#________________________________________________________
# plot outlier boxplot
plot_outliers <- function(df, var, xlab = var) {

  data <- dplyr::mutate(p_times, 
                        value_norm = (var - mean(var, na.rm = TRUE)) / sd(var, na.rm = TRUE),
                        outlier = ifelse(is_outlier(var), as.character(hh_id), NA))

  p <- ggplot(data, aes_string(x = var, y = value_norm)) +
        geom_boxplot() +
        geom_text(aes(label = outlier), na.rm = TRUE, hjust = -0.3, size = 4) +
        theme_minimal() +
        ylab("z score normalized value") +
        xlab("") +
        theme(axis.text.x = element_text(angle = 35, hjust = 0.95, size = 30)) +
        theme(axis.text.y = element_text(size = 30),
              axis.title=element_text(size=40))

  # return plot
  return(p)
  
}
#________________________________________________________
