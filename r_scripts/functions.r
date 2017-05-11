#________________________________________________________
# require libraries
  library(tidyverse)
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
filter_temp <- function(times, df){
    
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
filter_times <- function(times, data){

  ids <- dplyr::distinct(as.charcter(times$hh_id))
 
  # loop ids
    for(i in 1:length(ids)){

      tmp <- dplyr::filter(df,
                           as.character(id) == as.character(times$hh_id[i]),
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
        dplyr::mutate(mw = ifelse(ions == "Cl" & !is.na(ions),
                                  mw + 32.0675, mw)) 

  # return the molecular weight
  return(pol_properties$mw)
}
#________________________________________________________

#________________________________________________________
# function to recast timezones 

recastTimezone.POSIXct <- function(x, tz) return(
  as.POSIXct(as.character(x), origin = as.POSIXct("1970-01-01"), tz = tz))
#________________________________________________________