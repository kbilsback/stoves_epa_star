#________________________________________________________
# load relevant libraries
library(tidyverse)
library(lubridate)
library(dplyr)
#________________________________________________________

#________________________________________________________
# load field metadata and convert each column to appropriate R class
filter_sum_data <- function(input_data,field_temp_meta,join_duration,cooking_group){
  

  
  #For uganda data, find which files are from Uganda, and find the hh_id in the file name.  Then join with the meta data to get the logger id, field_site, and stove_type
  field_temp_meta_uganda <- dplyr::filter(field_temp_meta,grepl("uganda",field_site, fixed=TRUE,ignore.case=TRUE))
  field_sumsarized_events_uganda <-  dplyr::filter(input_data,grepl("Uganda",filename, fixed=TRUE)) %>%
    dplyr::mutate(hh_id = substring(filename, 19, 21)) %>%       
    dplyr::left_join(dplyr::select(field_temp_meta_uganda, hh_id, logger_id, field_site, stove,notes,start_date,end_date),
                                    by = "hh_id") %>%
    dplyr::filter(!is.na(hh_id)) %>%
    dplyr::mutate(hh_id = as.factor(hh_id)) %>%# 
    dplyr::mutate(field_site = as.factor(field_site)) %>%# should already be factor
    dplyr::mutate(stove_use_category = if_else(grepl("1",substr(hh_id,3,3),ignore.case=TRUE),"Primary",if_else(grepl("2",substr(hh_id,3,3),ignore.case=TRUE),"Secondary",
                                    if_else(grepl("3",substr(hh_id,3,3),ignore.case=TRUE),"Tertiary",  if_else(grepl("4",substr(hh_id,3,3),ignore.case=TRUE),"Quaternary","Primary"))))) %>%
    dplyr::mutate(stove = as.factor(stove)) # should already be factor
  
  
  #Honduras has no data in the meta data file, it is all in the filename.
  #Need to deal with duplicate loggers on a stove
  field_sumsarized_events_honduras <-  dplyr::filter(input_data,grepl("honduras",filename, fixed=TRUE)) %>%
    dplyr::mutate(hh_id = substring(filename, 
                                    sapply(filename, function(x) unlist(gregexpr('_',x,perl=TRUE))[1])+1, 
                                    sapply(filename, function(x) unlist(gregexpr('_',x,perl=TRUE))[2])-1)) %>%
    dplyr::filter(!is.na(hh_id)) %>%
    dplyr::mutate(hh_id = as.factor(hh_id)) %>%# should already be factor
    dplyr::mutate(logger_id = substring(filename, 
                                        sapply(filename, function(x) unlist(gregexpr('_',x,perl=TRUE))[3])+1, 
                                        sapply(filename, function(x) unlist(gregexpr('_',x,perl=TRUE))[4])-1)) %>%
    dplyr::mutate(field_site = "honduras") %>%  
    dplyr::mutate(field_site = as.factor(field_site)) %>%# should already be factor
    dplyr::mutate(stove = substring(filename, 
                                    sapply(filename, function(x) unlist(gregexpr('_',x,perl=TRUE))[5])+1, 
                                    sapply(filename, function(x) unlist(gregexpr('_',x,perl=TRUE))[6])-1)) %>%
    dplyr::mutate(stove_use_category = if_else(grepl("Primary",filename,ignore.case=TRUE),"Primary",if_else(grepl("Secondary",filename,ignore.case=TRUE),"Secondary",
                                    if_else(grepl("Tertiary",filename,ignore.case=TRUE),"Tertiary", if_else(grepl("Quaternary",filename,ignore.case=TRUE),"Quaternary","Primary"))))) %>%
    dplyr::mutate(notes = "NA") %>% 
    dplyr::mutate(start_date = as.POSIXct(NA)) %>% 
    dplyr::mutate(end_date = as.POSIXct(NA)) %>% 
    dplyr::mutate(stove = as.factor(stove)) # should already be factor
  
  
    
  #Need to deal with duplicate loggers on a stove.            
  field_sumsarized_events_china <-  dplyr::filter(input_data,!grepl("Uganda|India|honduras",filename)) %>%
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
    dplyr::mutate(logger_id = as.factor(logger_id)) %>%# should already be factor
    dplyr::mutate(hh_id = as.factor(hh_id)) %>%# should already be factor
    dplyr::mutate(field_site = as.factor(field_site)) %>%# should already be factor
    dplyr::mutate(stove_use_category = if_else(grepl("Primary",filename,ignore.case=TRUE),"Primary",if_else(grepl("Secondary",filename,ignore.case=TRUE),"Secondary",
                                        if_else(grepl("Tertiary",filename,ignore.case=TRUE),"Tertiary", if_else(grepl("Quaternary",filename,ignore.case=TRUE),"Quaternary","Primary"))))) %>%
    dplyr::mutate(notes = "NA") %>% 
    dplyr::mutate(start_date = as.POSIXct(NA)) %>% 
    dplyr::mutate(end_date = as.POSIXct(NA)) %>% 
    dplyr::mutate(stove = as.factor(stove)) # should already be factor
  
  
  #Need to deal with duplicate loggers on a stove.  
  field_temp_meta_india <- dplyr::filter(field_temp_meta,grepl("india",field_site, fixed=TRUE))
  field_sumsarized_events_india <-  dplyr::filter(input_data,grepl("India",filename, fixed=TRUE)) %>%
    dplyr::mutate(logger_id = substring(filename, 
                                        sapply(filename, function(x) tail(unlist(gregexpr('_',x,perl=TRUE)),3)[1])+1, 
                                        sapply(filename, function(x) tail(unlist(gregexpr('_',x,perl=TRUE)),3)[2])-1)) %>%
    dplyr::mutate(logger_id = if_else((nchar(logger_id)<3), substring(filename, 
                                        sapply(filename, function(x) tail(unlist(gregexpr('_',x,perl=TRUE)),2)[1])+1, 
                                        sapply(filename, function(x) tail(unlist(gregexpr('_',x,perl=TRUE)),2)[2])-1),  
                                        logger_id)) %>%
    dplyr::left_join(dplyr::select(field_temp_meta_india, hh_id, logger_id, field_site, stove,notes,start_date,end_date),
                                        by = "logger_id") %>%
    dplyr::filter(!is.na(logger_id)) %>%
    dplyr::mutate(logger_id = as.factor(logger_id)) %>%# should already be factor
    dplyr::mutate(hh_id = as.factor(hh_id)) %>%# should already be factor
    dplyr::mutate(field_site = "india") %>%  
    dplyr::mutate(field_site = as.factor(field_site)) %>% # should already be factor
    dplyr::mutate(stove_use_category = if_else(grepl("Primary",filename,ignore.case=TRUE),"Primary",if_else(grepl("Secondary",filename,ignore.case=TRUE),"Secondary",
                                        if_else(grepl("Tertiary",filename,ignore.case=TRUE),"Tertiary", if_else(grepl("Quaternary",filename,ignore.case=TRUE),"Quaternary","Primary"))))) %>%
    dplyr::mutate(stove = as.factor(stove)) # should already be factor
  
  
  #Re-combine the country specific data sets for further analysis, and add stove descriptors from above stove_codes
  field_sumsarized_events_all <-   rbind(field_sumsarized_events_china, field_sumsarized_events_uganda,
                                         field_sumsarized_events_india,field_sumsarized_events_honduras) %>% 
                          dplyr::mutate(filename = if_else(grepl("_",substr(filename,1,1),
                                      ignore.case=TRUE), substring(filename, 2),filename)) %>%
                          dplyr::mutate(file_indices = match(filename, unique(filename))) %>%
                          dplyr::mutate(start_date = as.POSIXct(start_date)) %>%
                          dplyr::mutate(end_date = as.POSIXct(end_date) )
                                                          
  
  #Group cooking events based on the cooking_group variable.  Must be greater than X minutes, 
  #into which they were grouped initially by Geocene
  if (cooking_group>0){
  cooking_events_timeseries_group <- data.frame(filename=character(),
                                          start_time=as.POSIXct(character()),
                                          duration_minutes=as.numeric(),
                                          hh_id=factor(),
                                          stove=factor(),
                                          logger_id=factor(), 
                                          field_site=factor(),  
                                          stove_use_category=factor(),
                                          start_date = as.POSIXct(character()), 
                                          end_date = as.POSIXct(character()),
                                          notes=character(),
                                          event_num=as.numeric(),
                                          use_flag = as.logical())
  
  
  for (i in unique(field_sumsarized_events_all$file_indices)) {
    #Grab data from file i, and keep only the entries that are marked as cooking
    temp <- dplyr::filter(field_sumsarized_events_all,file_indices == i) %>%
        dplyr::filter(!duplicated(start_time)) 
    #if any cooking time is found, make an event from it.
    if (dim(temp)[1]>1) {
      
      #If date is NA, give it a good one.
          if (is.na(as.POSIXct(temp$end_date[1],origin = "1970-01-01 UTC"))) { 
            start_date_temp = as.POSIXct(min(temp$start_time),origin = "1970-01-01 UTC")
            end_date_temp = as.POSIXct(max(temp$start_time),origin = "1970-01-01 UTC")
          } else if (temp$end_date[1] == as.POSIXct('2017-01-01',origin = "1970-01-01 UTC")) { #If end date is 2017-01-01, this was used in the log sheet as code that we did not know the end date.  use the values from the files in this case.
            start_date_temp = as.POSIXct(min(temp$start_time),origin = "1970-01-01 UTC")
            end_date_temp = as.POSIXct(max(temp$start_time),origin = "1970-01-01 UTC")
          } else { #If not NA and not 2017-01-01, must have been found in the meta data, use it.
            start_date_temp = as.POSIXct(min(temp$start_date),origin = "1970-01-01 UTC")
            end_date_temp = as.POSIXct(max(temp$end_date),origin = "1970-01-01 UTC")
          }      
      
      #This section is different from the one for the time series because we already have events here, so we don't need to look at the indices,
      #Rather we just look at the times of starting/stopping directly.
      #Here we care about the difference between the end of the first cooking event and the start of the next one.
      event_end_time <- as.numeric(temp$start_time)+temp$duration_minutes*60 #Time of the end of each cooking event in UNIX time (seconds from 1970)
      event_start_time <- as.numeric(temp$start_time) ##Time of the start of each cooking event in UNIX time
      #This is the basis for grouping.
      difftime_events_minutes <- (event_start_time[2:length(event_end_time)] - event_end_time[1:length(event_end_time)-1])/60 #Difference in time between end of first and start of second.  
      
      breakstart <- c(0,which((difftime_events_minutes>cooking_group) == TRUE))+1 #Start of a cooking event, by index
      breakend <- c(which((difftime_events_minutes>cooking_group) == TRUE),dim(temp)[1]) #End of cooking event. 
      #Last point includes the last point of an actual cooking event. 
      
      #Add cooking events to the cooking_events_timeseries_group data frame.
      cooking_events_timeseries_group <- rbind(cooking_events_timeseries_group,
                                         data.frame(filename = temp$filename[breakstart],
                                           start_time= as.POSIXct(temp$start_time[breakstart],origin = "1970-01-01 UTC"),
                                           duration_minutes=(event_end_time[breakend]-event_start_time[breakstart])/60,
                                           hh_id=as.factor(temp$hh_id[breakstart]),
                                           stove=factor(temp$stove[breakstart]),
                                           logger_id=factor(temp$logger_id[breakstart]),
                                           field_site=as.factor(temp$field_site[breakstart]),
                                           stove_use_category=factor(temp$stove_use_category[breakstart]),
                                           end_date = end_date_temp,
                                           start_date = start_date_temp,
                                           notes = temp$notes[1],
                                           event_num=temp$event_num[breakstart],
                                           use_flag=as.logical(rep(1,length(breakstart)[1])))) 
      }
  }
  cooking_events_timeseries_group
} else {field_sumsarized_events_all}
  }
#________________________________________________________
