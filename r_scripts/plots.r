#________________________________________________________
# require libraries
  library(tidyverse)
#________________________________________________________

#________________________________________________________
# plot timeseries data (default by qc color)
# takes a data frame and a column name of variable to plot
field_timeseries_plot <- function(df, y_var, color_var = "qc", x_var = "datetime", facet_var = "hh_id", y_units = "units") {

  ggplot(df, aes_string(y = y_var, x = x_var, color = color_var)) +
    geom_line() +
    scale_fill_discrete(drop = FALSE) +
    facet_wrap(~df[[facet_var]], ncol = 1, scales = "free") +
    theme_minimal() +
    theme(legend.position = "top") +
    ylab(paste0(y_var, " (", df[[1, y_units]], ")")) +
    xlab(x_var)

}
#________________________________________________________

#________________________________________________________
# plot timeseries data by a fill color
# takes a data frame and a column name of variable to plot
field_boxplot <- function(df, y_var, fill_var = "qc", x_var = "hh_id", y_units = "units") {

  ggplot(df, aes_string(y = y_var, x = x_var, fill = fill_var)) +
    geom_boxplot() +
    scale_fill_discrete(drop = FALSE) +
    theme_minimal() +
    theme(legend.position = "top") +
    ylab(paste0(y_var, " (", df[[1, y_units]], ")")) +
    xlab(x_var)

}
#________________________________________________________

#________________________________________________________
# plot field data
field_pointplot <- function(df, y_var, color_var = "qc", x_var = "hh_id", y_units = "units") {

  ggplot(df, aes_string(y = y_var, x = x_var, color = color_var)) +
    geom_point() +
    scale_fill_discrete(drop = FALSE) +
    theme_minimal() +
    theme(legend.position = "top") +
    ylab(paste0(y_var, " (", df[[1, y_units]], ")")) +
    xlab(x_var)

}
#________________________________________________________

#________________________________________________________
# plot stove fuel combinations tested
plot_test_list <- function(test_list, x_var = stove_type, y_var = fuel_type,
                           id = hh_id, fill_color = field_site) {

  ggplot(test_list, aes_string(y = y_var, x = x_var)) + 
    geom_tile(colour = "white", width = 0.9, height = 0.9,
              aes_string(fill = fill_color)) +
    scale_fill_discrete(na.value = 'grey95') +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 35, hjust = 0.95),
          axis.text = element_text(size = 14),
          legend.position = "top") +
    geom_text(aes(label = id, size = 8)) +
    xlab("") + ylab("")

}
#________________________________________________________

#________________________________________________________
# plot stove fuel combinations tested
plot_meta_hist <- function(df, var, x_lab = var, bwidth = 15) {

  ggplot(df, aes_string(var)) +
    geom_histogram(binwidth = bwidth, stat = "count") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    xlab(x_lab)

}
#________________________________________________________

#________________________________________________________
# plot outlier boxplot
plot_outliers <- function(df, var, xlab = var) {

  data <- dplyr::mutate(p_times, 
                        value_norm = (var - mean(var, na.rm = TRUE)) / sd(var, na.rm = TRUE),
                        outlier = ifelse(is_outlier(var), as.character(hh_id), NA))

  ggplot(data, aes_string(x = var, y = value_norm)) +
    geom_boxplot() +
    geom_text(aes(label = outlier), na.rm = TRUE, hjust = -0.3, size = 4) +
    theme_minimal() +
    ylab("z score normalized value") +
    xlab("") +
    theme(axis.text.x = element_text(angle = 35, hjust = 0.95, size = 30)) +
    theme(axis.text.y = element_text(size = 30),
          axis.title=element_text(size=40))

}
#________________________________________________________
