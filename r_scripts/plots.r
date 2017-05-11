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

#________________________________________________________________
# boxplot with outlier labels
p_box_outliers <- function(df, grep_str, scale = 1){
  # select data
  df <- subset(df, select =  c(grep(grep_str, colnames(df), value=TRUE),"date", "id"))
  
  # rename columns
  names(df) <- gsub(grep_str, "", colnames(df))
  
  # melt and group
  df <- melt(df,  id.vars = c("date","id"), variable.name = "series")
  df <- group_by(df, series)
  
  # add outlier experment id
  df <- mutate(df, outlier = ifelse(is_outlier(value), as.character(id), NA))
  
  # scale
  df$value <- df$value/scale
  
  # plot
  p <- ggplot(df, aes(x = series, y = value)) +
    geom_boxplot() +
    theme_bw() +
    geom_text(aes(label = outlier), na.rm = TRUE, nudge_y = 0, nudge_x = 0.3, size = 4)
  
  # return plot
  return(p)
}
#________________________________________________________________

#________________________________________________________________
# stacked bar
p_stacked_bar <- function(df, grep_str = "^time_.*[^0-9]$", scale = 1){
  # subset data
  df_times <- subset(df, select =  grep(grep_str, colnames(df), value=TRUE))/scale
  
  # number of columns  
  cols <- ncol(df_times)
  
  # convert times
  for(i in cols:2){
    df_times[,i] <- (df_times[,i] - df_times[,i-1])
  }
  
  df_times[,1] <- 0
  
  df_times$date <- df$date
  
  df_times$id <- df$id
  
  # rename columns
  names(df_times) <- gsub("^time_", "", colnames(df_times))
  
  # melt and group
  df_times <- melt(df_times,  id.vars = c("date","id"), variable.name = "series")
  
  df_times <- group_by(df_times, series)
  
  # plot
  p <- ggplot(df_times, aes(x = id, y = value, fill = series)) +
    geom_bar(stat = "identity", colour = "black", position = "stack") + 
    theme_bw()
  
  # return plot
  return(p)
}
#________________________________________________________________

#________________________________________________________________
# stacked bar
p_stacked_bar_date <- function(df, grep_str = "^time_.*[^0-9]$", scale = 1){
  # subset data
  df_times <- subset(df, select =  grep(grep_str, colnames(df), value=TRUE))/scale
  
  # number of columns  
  cols <- ncol(df_times)
  
  # convert times
  for(i in cols:2){
    df_times[,i] <- (df_times[,i] - df_times[,i-1])
  }
  
  df_times[,1] <- 0
  
  df_times$date <- as.factor(df$date)
  
  # rename columns
  names(df_times) <- gsub("^time_", "", colnames(df_times))
  
  # melt and group
  df_times <- melt(df_times,  id.vars = c("date"), variable.name = "series")
  
  df_times <- group_by(df_times, series)
  
  # plot
  p <- ggplot(df_times, aes(x = date, y = value, fill = series)) +
    geom_bar(stat = "identity", colour = "black", position = "stack") + 
    theme_bw()
  
  # return plot
  return(p)
}
#________________________________________________________________
