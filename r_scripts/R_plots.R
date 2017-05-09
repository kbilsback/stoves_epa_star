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
