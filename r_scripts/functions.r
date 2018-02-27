#________________________________________________________
# require libraries
  library(tidyverse)
  library(lubridate)
#________________________________________________________

#________________________________________________________
# check for outliers
  is_outlier <- function(x) {
    
    out <- (x < quantile(x, 0.25, na.rm = TRUE) - 1.5 * IQR(x, na.rm = TRUE)) | 
           (x > quantile(x, 0.75, na.rm = TRUE) + 1.5 * IQR(x, na.rm = TRUE))
    
  # return
    return(out)
}
#________________________________________________________

#________________________________________________________
# filter data for time periods of interest only
# requires df with time windows (id, start, end)
# df with id, time
# appends rep variable
filter_times2 <- function(times, df){
    
    rows <- nrow(times)
    
    # loop idsx
    for(i in 1:rows){
      # filter by date and time
      tmp <- dplyr::filter(df, date == times$date[i]) %>%
             dplyr::filter(test_id == times$test_id[i],
                           time >= times$start[i],
                           time <= times$end[i]) %>%
             dplyr::mutate(sample_id = times$sample_id[i])
      
      # if first match
      if(exists("out", inherits = FALSE) == FALSE & nrow(tmp) > 0){
        out <- tmp
      }
      
      # if not first match with data
      if(exists("out", inherits = FALSE) == TRUE & nrow(tmp) > 0){
        out <- rbind(out, tmp)
      }
      # end for loop
    }
    
    # return
    return(out)
}
#________________________________________________________

#________________________________________________________
# filter data for time periods of interest only
# requires df with time windows (id, start, end)
# df with id, time
# appends rep variable
filter_times <- function(times, df){
  
  rows <- nrow(times)
  
  # loop idsx
  for(i in 1:rows){
    # filter by date and time
    tmp <- dplyr::filter(df, date == times$date[i]) %>%
      dplyr::filter(hh_id == times$hh_id[i],
                    time >= times$start[i],
                    time <= times$end[i])
    
    # if first match
    if(exists("out", inherits = FALSE) == FALSE & nrow(tmp) > 0){
      out <- tmp
    }
    
    # if not first match with data
    if(exists("out", inherits = FALSE) == TRUE & nrow(tmp) > 0){
      out <- rbind(out, tmp)
    }
    # end for loop
  }
  
  # return
  return(out)
}
#________________________________________________________

#________________________________________________________
# Calculate the molecular weight of study pollutants
# Molecuar weights are calculated using the average
# standard atomic weights of each individual elements
#
# Atomic weights are from the NIST Physical Reference Data Website
calc_mw <- function(tbl){

  dplyr::mutate(tbl, mw = (num_c * 12.0106) +
                          (num_h * 1.007975) +
                          (num_o * 15.9994),
                       mw = ifelse(other == "S" & !is.na(other),
                                   mw + 32.0675, mw))

}
#________________________________________________________

#________________________________________________________
# return the molecular weight of carbon
mw_c <- function() {12.0106}
#________________________________________________________

#________________________________________________________
# convert ppmv to ug/m^3
# mw = molecular weight g/mol
# t = temperature oC
# p = pressure kPa
convert_ppmv_mgm3 <- function(ppmv, mw, t = 25, p = 101325){

  (ppmv * mw)*(p /(8.3144 * (t + 273.15))) * (1 / 1000)
  
}
#________________________________________________________

#________________________________________________________
get_lm_eqn <- function(m){

  eq <- substitute(~~R^2~"="~r2, 
                   list(r2 = format(summary(m)$r.squared, digits = 3)))

  as.character(as.expression(eq))

}
#________________________________________________________

#________________________________________________________
# apply kirchstetter loading correction to microaeth data
ma_loading_corr <- function(data) {
  a <- 0.88
  b <- 0.12
  data %>% 
    dplyr::mutate(Tr = exp(-atn / 100)) %>%
    dplyr::mutate(rK = (a * Tr + b)) %>%
    dplyr::mutate(bc_corr = bc / (0.6 * rK))
  
}
#________________________________________________________

#________________________________________________________
pm_r2 = function(df){
  m = lm((pm_ef) ~ fp, df, na.omit(TRUE))
  eq <- substitute(~~R^2~"="~r2, 
                   list(r2 = round(summary(m)$r.squared, digits = 2)))
  as.character(as.expression(eq))
}
#________________________________________________________

#________________________________________________________
bc_r2 = function(df){
  m = lm((bc_ef) ~ fp, df, na.omit(TRUE))
  eq <- substitute(~~R^2~"="~r2, 
                   list(r2 = round(summary(m)$r.squared, digits = 2)))
  as.character(as.expression(eq))
}
#________________________________________________________

#________________________________________________________
pm_p = function(df){
  eq <- substitute(~~rho~"="~p, 
                   list(p = format(round(cor(df$fp, df$pm_ef,
                                             use = "complete.obs",
                                             method = "spearman"), 2),
                                   nsmall = 2)))
  as.character(as.expression(eq))
}
#________________________________________________________

#________________________________________________________
bc_p = function(df){
  eq <- substitute(~~rho~"="~p, 
                   list(p = format(round(cor(df$fp, df$bc_ef,
                                             use = "complete.obs",
                                             method = "spearman"), 2),
                                   nsmall = 2)))
  as.character(as.expression(eq))
}
#_
#________________________________________________________

#________________________________________________________
co_p = function(df){
  eq <- substitute(~~rho~"="~p, 
                   list(p = format(round(cor(df$fp, df$co_ef,
                                             use = "complete.obs",
                                             method = "spearman"), 2),
                                   nsmall = 2)))
  as.character(as.expression(eq))
}
#_
#________________________________________________________
