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
plot_test_list <- function(test_list, x_var = "stove_type", y_var = "fuel_type",
                           id = "hh_id", fill_color = "field_site") {

  ggplot(test_list, aes_string(y = y_var, x = x_var)) + 
    geom_tile(colour = "white", width = 0.9, height = 0.9,
              aes_string(fill = fill_color)) +
    scale_fill_discrete(na.value = 'grey95') +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 35, hjust = 0.95),
          axis.text = element_text(size = 14),
          legend.position = "top") +
    geom_text(aes(label = id), size = 4) +
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
# plot dodge bar chart

plot_dodge <- function(df, y_var, y_label, filter_var, x_var = "sample_id",
                       x_label = "stove type", facet_1 = "stove", fill_color = "fuel") {

  p_df <- df %>%
          dplyr::filter(fuel_type == filter_var) %>%
          dplyr::group_by_(facet_1, fill_color, x_var) %>%
          dplyr::summarise_at(y_var, mean, na.rm = TRUE)

  ggplot(p_df, aes_string(x = x_var, y = y_var, fill = fill_color)) +
    geom_col(position = "dodge") +
    theme_bw() + 
    facet_wrap(as.formula(paste("~", facet_1)), scales = "free", ncol = 1) +
    ylab(y_label) +
    xlab(x_label) +
    theme(text = element_text(size = 18), legend.position = "top")
}

#________________________________________________________

#________________________________________________________
# plot dot plot with geom smooth

plot_dot_line <- function(df, y_var, y_label, filter_var, x_var = "fp",
                          x_label = "firepower (kW)", facet_1 = "stove", plot_color = "sample_id") {
  
  p_df <- df %>%
          dplyr::filter(sample_id != "start_up", sample_id != "shutdown") %>%
          dplyr::filter(fuel_type == filter_var) 
  
  m <- p_df %>%
       dplyr::group_by_(facet_1) %>%
       dplyr::do(model = lm(paste(eval(y_var), "~", eval(x_var)), .)) %>%
       dplyr::mutate(eqn = get_lm_eqn(model))

  eqn <- data.frame(eqn = unclass(m$eqn),
                    stove = m$stove)

  ggplot(p_df, aes_string(x = x_var, y = y_var)) +
    geom_point(aes_string(color = plot_color), size = 2) +
    geom_smooth(method = "lm", formula = 'y ~ x',
                color = 'black') +
    geom_text(aes(x = -Inf, y = Inf, label = eqn),
              data = eqn, color = 'black', size = 7,
              parse = TRUE, vjust = "inward", hjust = "inward") + 
    theme_bw() + 
    facet_wrap(as.formula(paste("~", facet_1)), scales = "free", ncol = 2) +
    ylab(y_label) +
    xlab(x_label) +
    theme(text = element_text(size = 18), legend.position = "top")
}

#________________________________________________________
#
#________________________________________________________
## plot dot plot with geom smooth

field_plot_dot_line <- function(df, y_var, y_label, x_var = "firepower",
                                x_label = "firepower (kW)", facet_1 = "var", plot_color = "hh_id") {

  m <- df %>%
       dplyr::group_by_(facet_1) %>%
       dplyr::do(model = lm(paste(eval(y_var), "~", eval(x_var)), .)) %>%
       dplyr::mutate(eqn = get_lm_eqn(model))
  
  eqn <- data.frame(eqn = unclass(m$eqn),
                    var = m$var)
  
  ggplot(df, aes_string(x = x_var, y = y_var)) +
    geom_point(aes_string(color = plot_color), size = 2) +
    geom_smooth(method = "lm", formula = 'y ~ x',
                color = 'black') +
    geom_text(aes(x = -Inf, y = Inf, label = eqn),
              data = eqn, color = 'black', size = 7,
              parse = TRUE, vjust = "inward", hjust = "inward") + 
    theme_bw() + 
    facet_wrap(as.formula(paste("~", facet_1)), scales = "free", ncol = 2) +
    ylab(y_label) +
    xlab(x_label) +
    theme(text = element_text(size = 18), legend.position = "top")
}

#________________________________________________________

#________________________________________________________
# plot timeseries data (default by qc color)
# takes a data frame and a column name of variable to plot
field_timeseries_plot_co <- function(df, y_var, y_var2, color_var = "height", x_var = "datetime", facet_var = "hh_id", y_units = "units") {
  
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