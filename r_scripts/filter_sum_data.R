#________________________________________________________
# load relevant libraries
library(tidyverse)
library(lubridate)
library(dplyr)
#________________________________________________________

#________________________________________________________
# load field metadata and convert each column to appropriate R class
filter_sum_data <- function(xx,field_temp_meta,bad_files){
  

  
  #For uganda data, find which files are from Uganda, and find the hh_id in the file name.  Then join with the meta data to get the logger id, field_site, and stove_type
  field_temp_meta_uganda <- dplyr::filter(field_temp_meta,grepl("uganda",field_site, fixed=TRUE))
  field_sumsarized_events_uganda <-  dplyr::filter(xx,grepl("Uganda",filename, fixed=TRUE)) %>%
    dplyr::mutate(hh_id = substring(filename, 19, 21)) %>%       
    dplyr::left_join(dplyr::select(field_temp_meta_uganda, hh_id, logger_id, field_site, stove),
                     by = "hh_id") %>%
    dplyr::filter(!is.na(hh_id)) %>%
    dplyr::mutate(hh_id = as.factor(hh_id)) %>%# 
    dplyr::mutate(field_site = as.factor(field_site)) %>%# should alaready be factor
    dplyr::mutate(stove = as.factor(stove)) # should alaready be factor
  
  #Honduras has no data in the meta data file, it is all in the filename.
  #Need to deal with duplicate loggers on a stove
  field_sumsarized_events_honduras <-  dplyr::filter(xx,grepl("honduras",filename, fixed=TRUE)) %>%
    dplyr::mutate(hh_id = substring(filename, 
                                    sapply(filename, function(x) unlist(gregexpr('_',x,perl=TRUE))[1])+1, 
                                    sapply(filename, function(x) unlist(gregexpr('_',x,perl=TRUE))[2])-1)) %>%
    dplyr::filter(!is.na(hh_id)) %>%
    dplyr::mutate(hh_id = as.factor(hh_id)) %>%# should alaready be factor
    dplyr::mutate(logger_id = substring(filename, 
                                        sapply(filename, function(x) unlist(gregexpr('_',x,perl=TRUE))[3])+1, 
                                        sapply(filename, function(x) unlist(gregexpr('_',x,perl=TRUE))[4])-1)) %>%
    dplyr::mutate(field_site = "honduras") %>%  
    dplyr::mutate(field_site = as.factor(field_site)) %>%# should alaready be factor
    dplyr::mutate(stove = substring(filename, 
                                    sapply(filename, function(x) unlist(gregexpr('_',x,perl=TRUE))[5])+1, 
                                    sapply(filename, function(x) unlist(gregexpr('_',x,perl=TRUE))[6])-1)) %>%
    dplyr::mutate(stove = as.factor(stove)) # should alaready be factor
  
  
  #Need to deal with duplicate loggers on a stove.            
  field_sumsarized_events_china <-  dplyr::filter(xx,!grepl("Uganda|India|honduras",filename)) %>%
    dplyr::mutate(hh_id = substring(filename, 
                                    sapply(filename, function(x) unlist(gregexpr('_',x,perl=TRUE))[3])+1, 
                                    sapply(filename, function(x) unlist(gregexpr('_',x,perl=TRUE))[4])-1)) %>%  
    dplyr::mutate(stove = substring(filename, 
                                    sapply(filename, function(x) unlist(gregexpr('_',x,perl=TRUE))[1])+1, 
                                    sapply(filename, function(x) unlist(gregexpr('_',x,perl=TRUE))[2])-1)) %>%  
    dplyr::mutate(logger_id = substring(filename, 
                                        sapply(filename, function(x) unlist(gregexpr('_',x,perl=TRUE))[4])+1, 
                                        sapply(filename, function(x) unlist(gregexpr('_',x,perl=TRUE))[5])-1)) %>%  
    dplyr::mutate(field_site = "china") %>%  
    dplyr::filter(!is.na(hh_id)) %>%
    dplyr::mutate(logger_id = as.factor(logger_id)) %>%# should alaready be factor
    dplyr::mutate(hh_id = as.factor(hh_id)) %>%# should alaready be factor
    dplyr::mutate(field_site = as.factor(field_site)) %>%# should alaready be factor
    dplyr::mutate(stove = as.factor(stove)) # should alaready be factor
  
  
  #Need to adjust timestamps for files with comments that include 'add 11.5hr' (thermocouples in India)
  #Need to deal with duplicate loggers on a stove.  
  field_temp_meta_india <- dplyr::filter(field_temp_meta,grepl("india",field_site, fixed=TRUE))
  field_sumsarized_events_india <-  dplyr::filter(xx,grepl("India",filename, fixed=TRUE)) %>%
    dplyr::mutate(logger_id = substring(filename, 
                                        sapply(filename, function(x)  tail(unlist(gregexpr('_',x,perl=TRUE)),3)[1])+1, 
                                        sapply(filename, function(x) tail(unlist(gregexpr('_',x,perl=TRUE)),3)[2])-1)) %>%
    dplyr::left_join(dplyr::select(field_temp_meta_india, hh_id, logger_id, field_site, stove),
                     by = "logger_id") %>%
    dplyr::filter(!is.na(logger_id)) %>%
    dplyr::mutate(logger_id = as.factor(logger_id)) %>%# should alaready be factor
    dplyr::mutate(hh_id = as.factor(hh_id)) %>%# should alaready be factor
    dplyr::mutate(field_site = "india") %>%  
    dplyr::mutate(field_site = as.factor(field_site)) %>%# should alaready be factor
    dplyr::mutate(stove = as.factor(stove)) # should already be factor
  
  
  #Re-combine the country specific data sets for further analysis, and add stove descriptors from above stove_codes
  field_sumsarized_events_all <-   rbind(field_sumsarized_events_china, field_sumsarized_events_uganda,
                                         field_sumsarized_events_india,field_sumsarized_events_honduras) %>% 
    dplyr::mutate(file_indices = match(filename, unique(filename))) %>%
    dplyr::left_join(dplyr::select(field_sumsarized_csvdurations, filename,
                                   logging_duration_days),  by = "filename")
  

  # Remove data from bad files:
  field_sumsarized_events_all <- dplyr::filter(field_sumsarized_events_all,any(!grepl(bad_files,filename)))

  }
#________________________________________________________
