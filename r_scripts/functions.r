#________________________________________________________
# require libraries
  library(tidyverse)
  library(lubridate)
#________________________________________________________

#________________________________________________________
# pad with NAs
  pad  <- function(x, n) {
    len.diff <- n - length(x)
    c(rep(NA, len.diff), x) 
  }   
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
filter_times3 <- function(times, df){
  
  rows <- nrow(times)
  
  # loop idsx
  for(i in 1:rows){
    # filter by date and time
    tmp <- dplyr::filter(df, date == times$date[i]) %>%
      dplyr::filter(time >= times$start[i],
                    time <= times$end[i]) %>%
      dplyr::mutate(test_id = times$test_id[i],
                    sample_id = times$sample_id[i])
    
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
# filter data for time periods of interest only
# requires df with time windows (id, start, end)
# df with id, time
# appends rep variable
filter_times4 <- function(times, df){
  
  rows <- nrow(times)
  
  # loop idsx
  for(i in 1:rows){
    # filter by date and time
    tmp <- dplyr::filter(df, date == times$date[i]) %>%
           dplyr::filter(id == times$id[i],
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

#________________________________________________________
pm_sum = function(df){
  eq <- substitute(~~PM[2.5]~":"~m~" "(s1 - s2), 
                   list(m = format(round(median(df$pm_ef), 1), nsmall = 1),
                        s1 = as.numeric(unname(format(round(quantile(df$pm_ef, 0.25), 1), nsmall = 1))),
                        s2 = as.numeric(unname(format(round(quantile(df$pm_ef, 0.75), 1), nsmall = 1)))))
  as.character(as.expression(eq))
}
#________________________________________________________

#________________________________________________________
co_sum = function(df){
  eq <- substitute(~~CO~":"~m ~" "(s1 - s2), 
                   list(m = format(round(median(df$co_ef), 0), nsmall = 0),
                        s1 = as.numeric(unname(format(round(quantile(df$co_ef, 0.25), 0), nsmall = 0))),
                        s2 = as.numeric(unname(format(round(quantile(df$co_ef, 0.75), 0), nsmall = 0)))))
  as.character(as.expression(eq))
}
#________________________________________________________

#________________________________________________________
fp_sum = function(df){
  eq <- substitute(~~FP~":"~m ~" "(s1 - s2), 
                   list(m = format(round(median(df$fp), 1), nsmall = 1),
                        s1 = as.numeric(unname(format(round(quantile(df$fp, 0.25), 1), nsmall = 1))),
                        s2 = as.numeric(unname(format(round(quantile(df$fp, 0.75), 1), nsmall = 1)))))
  as.character(as.expression(eq))
}
#________________________________________________________

#________________________________________________________
mce_sum = function(df){
  eq <- substitute(~~MCE~":"~m ~" "(s1 - s2), 
                   list(m = format(round(median(df$mce), 2), nsmall = 2),
                        s1 = as.numeric(unname(format(round(quantile(df$mce, 0.25), 2), nsmall = 2))),
                        s2 = as.numeric(unname(format(round(quantile(df$mce, 0.75), 2), nsmall = 2)))))
  as.character(as.expression(eq))
}
#________________________________________________________

#________________________________________________________
rho_temp_fp = function(df){
  eq <- substitute(~~rho~"="~p, 
                   list(p = format(round(cor(df$temp, df$log_fp,
                                             use = "complete.obs",
                                             method = "spearman"), 2),
                                   nsmall = 2)))
  as.character(as.expression(eq))
}
#________________________________________________________

#________________________________________________________
r2_temp_fp = function(df){

  m = lm(log_fp ~ temp, df, na.omit(TRUE))
  round(summary(m)$r.squared, digits = 2)

}
#________________________________________________________

#________________________________________________________
r2_fp_pm_rate = function(df){
  m = lm(log(pm_rate) ~ fp, df, na.omit(TRUE))
  eq <- substitute(~~"R-squared"~"="~r2, 
                   list(r2 = round(summary(m)$r.squared, digits = 2)))
  as.character(as.expression(eq))
}
#________________________________________________________

##________________________________________________________
# filter data for time periods of interest only
# requires df with time windows (id, start, end)
# df with id, time
filter_times5 <- function(times, df){
  
  ids <- unique(times$test_id)
  
  # loop ids
  for(i in 1:length(ids)){
    
    tmp <- dplyr::filter(df,
                         date == times$date[i],
                         #id == times$test_id[i],
                         time >= times$start[i],
                         time <= times$end[i])
    
    if(nrow(tmp) > 0){
      tmp$id <- test_times$test_id[i]
    }
    
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
# calculate the molecular weight of study pollutants
# weights are calculated using the average
# standard atomic weights of each individual elements
#
# atomic weights are from the NIST Physical Reference Data Website
calc_mw <- function(pol_properties){
  
  pol_properties$mw <- (pol_properties$num_c * 12.0106) +
    (pol_properties$num_h * 1.007975) +
    (pol_properties$num_o * 15.9994)
  
  pol_properties <- dplyr::mutate(pol_properties,
                                  mw = ifelse(ions == "Na" & !is.na(ions),
                                              mw + 22.98976928, mw)) %>%
    dplyr::mutate(mw = ifelse(ions == "N" & !is.na(ions),
                              mw + 14.006855, mw)) %>%
    dplyr::mutate(mw = ifelse(ions == "K" & !is.na(ions),
                              mw + 39.0983, mw)) %>%
    dplyr::mutate(mw = ifelse(ions == "Mg" & !is.na(ions),
                              mw + 24.3055, mw)) %>%
    dplyr::mutate(mw = ifelse(ions == "Ca" & !is.na(ions),
                              mw + 40.078, mw)) %>%
    dplyr::mutate(mw = ifelse(ions == "Cl" & !is.na(ions),
                              mw + 35.4515, mw)) %>%
    dplyr::mutate(mw = ifelse(ions == "S" & !is.na(ions),
                              mw + 32.065, mw)) 
  
  # return the molecular weight
  return(pol_properties$mw)
}
#________________________________________________________

#________________________________________________________
# convert ppbv to ug/m^3
# mw = molecular weight g/mol
# t = in Kelvin
# p = pressure pascals
convert_ppbv_ugpmc <- function(ppbv, MW){
  
  ((MW * 1e6 * 85000 * ppbv * 1e-9) / (8.3144 * 298.15))
  
}
#_______________________________________________________

#________________________________________________________
# convert ppbv to ug/m^3
# mw = molecular weight g/mol
# t = in Kelvin
# p = pressure pascals
convert_ppmv_ugpmc <- function(ppmv, MW){
  
  ((MW * 1e6 * 85000 * ppmv * 1e-6) / (8.3144 * 298.15))
  
}
#________________________________________________________
