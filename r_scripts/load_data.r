#________________________________________________________
# load relevant libraries
library(tidyverse)
library(lubridate)

#________________________________________________________

#________________________________________________________
# load temp data and convert each column to appropriate R class
load_field_temp <- function(){
  
  # files are read in files twice to extract data and logger id
  lapply(list.files("../data/field/temp",
                    pattern = ".csv",
                    full.names = TRUE),
         function(x)
           readr::read_csv(x,
                           skip = 7,
                           col_names = c("date", "time", "temp"),
                           col_types =
                             cols(
                               date = col_character(),
                               time = col_character(),
                               temp = col_double()),
                           na = c("", "NA")) %>%
           dplyr::mutate(logger_id = 
                           gsub("Serial Number:,,|,","",
                                readr::read_delim(x,
                                                  "\n",
                                                  n_max = 1,
                                                  skip = 1, 
                                                  col_types = 
                                                    cols(.default = col_character())
                                )
                           )
           )
  ) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(date = as.Date(ifelse(grepl("0[0-9]/", date),
                                        as.Date(date, format = "%d/%m/%Y"),
                                        as.Date(date, format = "%m/%d/%y")
    ),
    origin = "1970-01-01"),
    datetime = as.POSIXct(ifelse(grepl("PM|AM", time),
                                 as.POSIXct(paste(date, time),
                                            format = "%Y-%m-%d %I:%M:%S %p"),
                                 as.POSIXct(paste(date, time),
                                            format = "%Y-%m-%d %H:%M:%S")),
                          origin = "1970-01-01"),
    time = as.numeric(hms(format(datetime, "%H:%M:%S")))
    )
}
#________________________________________________________

#________________________________________________________
# load sums data and convert each column to appropriate R class
load_field_sums <- function(){
  
  # files are read in files twice to extract data and logger id
  lapply(list.files("../data/field/sums",
                    pattern = ".csv",
                    full.names = TRUE),
         function(x)
           readr::read_csv(x, 
                           skip = 20,
                           col_names = c("datetime", "units", "stove_temp"),
                           col_types = 
                             cols(
                               datetime = col_character(),
                               units = col_character(),
                               stove_temp = col_double()
                             ),
                           na = c("", "NA")
           ) %>%
           dplyr::mutate(logger_id = 
                           gsub(".*: ", "", readr::read_csv(x, 
                                                            skip = 1,
                                                            n_max = 1,
                                                            col_names = "id",
                                                            col_types =
                                                              cols(id = col_character())
                           )
                           )
           )
  ) %>%
    dplyr::bind_rows() %>%  # bind data from all files
    # convert time to secs in day and fix file problems
    dplyr::mutate(datetime = as.POSIXct(gsub("00", "16", datetime), 
                                        format = "%d/%m/%y %I:%M:%S %p"),
                  date = as.Date(datetime),
                  time = as.numeric(hms(format(datetime, "%H:%M:%S")))
    )
}
#________________________________________________________

#________________________________________________________ 
# load grav data and convert each column to appropriate R class
load_field_grav <- function(){
  
  lapply(list.files("../data/field/grav",
                    pattern = "grav.csv",
                    full.names = TRUE),
         function(x) readr::read_csv(x, col_names = TRUE,
                                     col_types = 
                                       cols(
                                         .default = col_double(),
                                         id = col_character(),
                                         pre_date = col_character(),
                                         blank_id = col_character(),
                                         post_date = col_date(format = "%d/%m/%Y"),
                                         post_pressure = col_character(),
                                         post_blank_id = col_character(),
                                         notes = col_character()
                                       ),
                                     na = c("", "NA")
         )
  ) %>% 
    dplyr::bind_rows() %>%
    dplyr::mutate(pre_date = as.Date(ifelse(grepl("^IN[A-Z]", pre_date),
                                            as.Date(pre_date, format = "%d/%m/%y"),
                                            as.Date(pre_date, format = "%m/%d/%y")),
                                     origin = "1970-01-01")
    )
  
}

# load grav data and convert each column to appropriate R class
load_field_grav <- function(){
  
  lapply(list.files("../data/field/grav",
                    pattern = "grav.csv",
                    full.names = TRUE),
         function(x) readr::read_csv(x, col_names = TRUE,
                                     col_types = 
                                       cols(
                                         .default = col_double(),
                                         id = col_character(),
                                         pre_date = col_character(),
                                         blank_id = col_character(),
                                         post_date = col_date(format = "%d/%m/%Y"),
                                         post_pressure = col_character(),
                                         post_blank_id = col_character(),
                                         notes = col_character()
                                       ),
                                     na = c("", "NA")
         )
  ) %>% 
    dplyr::bind_rows() %>%
    dplyr::mutate(pre_date = as.Date(ifelse(grepl("^IN[A-Z]", pre_date),
                                            as.Date(pre_date, format = "%d/%m/%y"),
                                            as.Date(pre_date, format = "%m/%d/%y")),
                                     origin = "1970-01-01")
    )
  
}
#________________________________________________________

#________________________________________________________ 
# load aqe data and convert each column to appropriate R class
load_field_aqe <- function(){
  
  lapply(list.files("../data/field/aqe",
                    pattern = "AQE.csv",
                    full.names = TRUE),
         function(x)
           readr::read_csv(x,
                           skip = 4,
                           
                           col_names = c("tag", "date", "time", "temp_units",
                                         "pol_units", "flow_units", "t_amb",
                                         "t_stack", "t_preheat", "o2", "co",
                                         "co2", "stack_draft", "so2", "velocity",
                                         "pressure", "rh", "dew_point",
                                         "wet_bulb_temp", "vocs"),
                           
                           col_types = 
                             cols(
                               .default = col_double(),
                               tag = col_character(),
                               date = col_date(format = "%m/%d/%y"),
                               time = col_character(),
                               temp_units = col_character(),
                               pol_units = col_character(),
                               flow_units = col_character()),
                           na = c("", "NA", "   NA")
           )
  ) %>% 
    dplyr::bind_rows() %>%
    dplyr::mutate(datetime = as.POSIXct(paste(date, time), 
                                        format = "%Y-%m-%d %H:%M:%S"),
                  time = as.numeric(hms(time)) # convert time to secs in day
    ) %>%
  dplyr::mutate(datetime_char = as.character(datetime)) #Making a char time variable to allow joining
  
}
#________________________________________________________

#________________________________________________________ 
# load aqe data and convert each column to appropriate R class
load_field_ma <- function(){
  
  lapply(list.files("../data/field/microaeth",
                    pattern = "MA",
                    full.names = TRUE),
         function(x)
           readr::read_csv(x,
                           skip = 17,
                           col_names = c("date", "time", "ref", "sen",
                                         "atn", "flow", "pcb_temp",
                                         "status", "battery", "bc"),
                           col_types = 
                             cols(
                               .default = col_double(),
                               date = col_date(format = ""),
                               time = col_character()),
                           na = c("", "NA")
           )
  ) %>% 
    dplyr::bind_rows() %>%
    dplyr::mutate(datetime = as.POSIXct(paste(date, time), 
                                        format = "%Y-%m-%d %H:%M:%S"),
                  time = as.numeric(hms(time)) # convert time to secs in day
    )
}
#________________________________________________________

#________________________________________________________
# load grav data and convert each column to appropriate R class
load_lab_grav <- function(){
  
  readr::read_csv("../data/lab/grav/grav.csv",
                  skip = 1,
                  col_names = c("id", "date", "sample_id", "start_time",
                                "end_time", "pm_mass", "pm_ef", "ir_atn",
                                "uv_atn", "mce", "fp", "bc_mass", "bc_ef",
                                "pm_flag", "bc_flag", "pm_rate", "bc_rate"),
                  col_types = 
                    cols(
                      id = col_character(),
                      date = col_date(format = "%m/%d/%y"),
                      sample_id = col_character(),
                      start_time = col_double(),
                      end_time = col_double(),
                      pm_mass = col_double(),
                      pm_ef = col_double(),
                      ir_atn = col_double(),
                      uv_atn = col_double(),
                      mce = col_double(),
                      fp = col_double(),
                      bc_mass = col_double(),
                      bc_ef = col_double(),
                      pm_flag = col_integer(),
                      bc_flag = col_integer(),
                      pm_rate = col_double(),
                      bc_rate = col_double()
                    ),
                  na = c("", "NaN")
  )
  
}
#________________________________________________________

#________________________________________________________ 
# load aqe data and convert each column to appropriate R class
load_field_ecoc <- function(){
  
  lapply(list.files("../data/field/ecoc",
                    pattern = "ecoc.csv",
                    full.names = TRUE),
         function(x)
           readr::read_csv(x,
                           skip = 4,
                           
                           col_names = c("filter_id",	"optics_mode","oc_ugsqcm",
                                         "oc_unc",	"ec_ugsqcm","ec_unc",
                                         "cc_ugsqcm","cc_unc","tc_ugsqcm","tc_unc","ectc_ratio",
                                         "pk1c_ugsqcm","pk2c_ugsqcm","pk3c_ugsqcm","pk4c_ugsqcm",
                                         "pyrolc_ugsqcm","ec1c_ugsqcm","ec2c_ugsqcm","ec3c_ugsqcm",
                                         "ec4c_ugsqcm","ec5c_ugsqcm","ec6c_ugsqcm","date","time","cal_const",
                                         "puch_area_cm2","fid1","fid2","calibration_area","num_points","splittime_sec",
                                         "manual_split_sec","init_abs","abs_coef","inst_name","atmpres_mmHg","optical_ec",
                                         "analyst","laser_correction","begin_int","end_int","tran_time","parameter_file",
                                         "empty1", "empty2"),
                           col_types = 
                             cols(
                               .default = col_double(),
                               filter_id = col_character(),
                               optics_mode = col_character(),
                               date = col_date(format = "%m/%d/%y"),
                               time = col_time(format = ""),
                               fid1 = col_character(),
                               fid2 = col_character(),
                               manual_split_sec = col_character(),
                               inst_name = col_character(),
                               analyst = col_character(),
                               parameter_file = col_character(),
                               empty1 = col_character(),
                               empty2 = col_character()
                             ),
                           na = c("", "na", "-")
           )
  ) %>% 
    dplyr::bind_rows() %>%
    dplyr::mutate(datetime = as.POSIXct(paste(date, time), 
                                        format = "%Y-%m-%d %H:%M:%S"),
                  time = as.numeric(hms(time)) # convert time to secs in day
    ) 
  
}


#________________________________________________________

#________________________________________________________
# load sumsarized data and convert each column to appropriate R class
# Load files of Sumsarizer output for the thermocouple data analyzed by RP
load_field_sumsarized_timeseries <- function(xx){
  
  
  lapply(list.files(paste0("../data/field/sumsarized/",xx, collapse=NULL),
                    pattern = ".csv",
                    full.names = TRUE),
         function(x)
           readr::read_csv(x, 
                           skip = 1,
                           col_names = c("filename", "datetime", "stove_temp", "state","datapoint_id","dataset_id"),
                           col_types = 
                             cols(
                               filename = col_character(),
                               datetime = col_character(),
                               stove_temp = col_double(),
                               state = col_logical(),
                               datapoint_id = col_character(),
                               dataset_id = col_character()
                             ),
                           na = c("", "NA")
           ) %>%
           dplyr::mutate(logging_duration_days = as.numeric(difftime(max(datetime),min(datetime),units = "days")))  
         
  ) %>%
    dplyr::bind_rows() %>%
    
    # convert time to secs in day and fix file problems
    dplyr::mutate(datetime = parse_date_time(gsub("/00", "/16", datetime),orders = c("y-m-d HMS", "m/d/y HMS"))) %>%
    dplyr::mutate(filename = gsub("__","_",filename)) %>% 
    dplyr::mutate(filename = gsub(" ","_",filename)) %>%
    dplyr::mutate(filename = if_else(grepl("uganda",filename,ignore.case=TRUE),paste("000000000",filename,sep = ""),filename)) %>%
    dplyr::mutate(filename = if_else(grepl("honduras",filename,ignore.case=TRUE),paste("_",filename,sep = ""),filename))
  
}

#________________________________________________________

#________________________________________________________
# load sumsarized data and convert each column to appropriate R class. field_sumsarized_csvdurations
# The data loaded here is just the logging duration, since the event data is imported using the events function.
load_field_sumsarized <- function(){
  
  
    lapply(c(list.files("../data/field/sumsarized/sensor_wise_csvs",
                    pattern = ".csv",
                    full.names = TRUE),list.files("../data/field/sumsarized/Thermocouple_Data_sumsarized",
                                                  pattern = ".csv",
                                                  full.names = TRUE)),
         function(x)
          readr::read_csv(x, 
                           skip = 1,
                           col_names = c("filename", "datetime", "stove_temp", "state","datapoint_id","dataset_id"),
                           col_types = 
                             cols(
                               filename = col_character(),
                               datetime = col_character(),
                               stove_temp = col_double(),
                               state = col_logical(),
                               datapoint_id = col_character(),
                               dataset_id = col_character()
                             ),
                           na = c("", "NA")
           ) %>%
           dplyr::mutate(logging_duration_days = as.numeric(difftime(max(datetime),min(datetime),units = "days")))  %>%
           dplyr::mutate(datetime = gsub("/00", "/16", datetime)) %>%
           dplyr::mutate(datetime = gsub("2000", "2016", datetime)) %>%
           dplyr::mutate(file_start_date= as.POSIXct(min(datetime))) %>%
           dplyr::mutate(file_end_date= as.POSIXct(max(datetime)))
                                           
  ) %>%
    dplyr::bind_rows() %>%
    
    # convert time to secs in day and fix file problems
    dplyr::filter(!duplicated(filename)) %>% 
    dplyr::select(filename,logging_duration_days,file_start_date,file_end_date) %>% 
    dplyr::mutate(filename = gsub("Cat1 1 Data/","Cat1Data_",filename)) %>% 
    dplyr::mutate(filename = gsub("Cat1 1 Data copy/","Cat1Data_",filename)) %>% 
    dplyr::mutate(filename = gsub("Cat1 1 Data copy 2/","Cat1Data_",filename)) %>% 
    dplyr::mutate(filename = gsub("Cat1 1 Data copy 3/","Cat1Data_",filename)) %>% 
    dplyr::mutate(filename = gsub("Cat1 1 Data copy 4/","Cat1Data_",filename)) %>% 
    dplyr::mutate(filename = gsub("Cat2 Data/","Cat2Data_",filename)) %>% 
    dplyr::mutate(filename = gsub("Cat3 Data/","Cat3Data_",filename)) %>% 
    dplyr::mutate(filename = gsub("Cat4 Data/","Cat4Data_",filename)) %>% 
    dplyr::mutate(filename = gsub("__","_",filename)) %>% 
    dplyr::mutate(filename = gsub(" ","_",filename)) 
  
}

#________________________________________________________

#________________________________________________________
# load sumsarized data csvs only.  Don't want thermocouple because the file names are needed to build null cooking events for accounting.
load_field_sumsarized_csvs <- function(){
  
  
  lapply(list.files("../data/field/sumsarized/sensor_wise_csvs",
                                                    pattern = ".csv",
                                                    full.names = TRUE),
         function(x)
           readr::read_csv(x, 
                           skip = 1,
                           col_names = c("filename", "datetime", "stove_temp", "state","datapoint_id","dataset_id"),
                           col_types = 
                             cols(
                               filename = col_character(),
                               datetime = col_character(),
                               stove_temp = col_double(),
                               state = col_logical(),
                               datapoint_id = col_character(),
                               dataset_id = col_character()
                             ),
                           na = c("", "NA")
           ) %>%
           dplyr::mutate(logging_duration_days = as.numeric(difftime(max(datetime),min(datetime),units = "days")))  %>%
           dplyr::mutate(datetime = gsub("/00", "/16", datetime)) %>%
           dplyr::mutate(datetime = gsub("2000", "2016", datetime)) %>%
           dplyr::mutate(file_start_date= as.POSIXct(min(datetime))) %>%
           dplyr::mutate(file_end_date= as.POSIXct(max(datetime)))
         
  ) %>%
    dplyr::bind_rows() %>%
    
    # convert time to secs in day and fix file problems
    dplyr::filter(!duplicated(filename)) %>% 
    dplyr::select(filename,logging_duration_days,file_start_date,file_end_date) %>% 
    dplyr::mutate(filename = gsub("Cat1 1 Data/","Cat1Data_",filename)) %>% 
    dplyr::mutate(filename = gsub("Cat1 1 Data copy/","Cat1Data_",filename)) %>% 
    dplyr::mutate(filename = gsub("Cat1 1 Data copy 2/","Cat1Data_",filename)) %>% 
    dplyr::mutate(filename = gsub("Cat1 1 Data copy 3/","Cat1Data_",filename)) %>% 
    dplyr::mutate(filename = gsub("Cat1 1 Data copy 4/","Cat1Data_",filename)) %>% 
    dplyr::mutate(filename = gsub("Cat2 Data/","Cat2Data_",filename)) %>% 
    dplyr::mutate(filename = gsub("Cat3 Data/","Cat3Data_",filename)) %>% 
    dplyr::mutate(filename = gsub("Cat4 Data/","Cat4Data_",filename)) %>% 
    dplyr::mutate(filename = gsub("__","_",filename)) %>% 
    dplyr::mutate(filename = gsub(" ","_",filename)) 
  
}


#________________________________________________________

#________________________________________________________
# load sumsarized data and convert each column to appropriate R class
load_field_sumsarized_events <- function(){
  
  
  lapply("../data/field/sumsarized/sumsarizer_events.csv",
         function(x)
           readr::read_csv(x, 
                           skip = 1,
                           col_names = c("filename", "event_num", "start_time", "duration_minutes","max_temp","min_temp","stdev_temp"),
                           col_types = 
                             cols(
                               filename = col_character(),
                               event_num = col_double(),
                               start_time = col_character(),
                               duration_minutes = col_double(),
                               max_temp = col_double(),
                               min_temp = col_double(),
                               stdev_temp = col_double()
                             ),
                           na = c("", "NA")
           )
         
  ) %>%
    dplyr::bind_rows() %>%
    
    # convert time to secs in day and fix file problems
    dplyr::mutate(start_time = gsub("/00", "/16", start_time))  %>%
    dplyr::mutate(filename = gsub("__","_",filename)) %>% 
    dplyr::mutate(start_time = mdy_hm(start_time))
}


#________________________________________________________

#________________________________________________________
# load Monte Carlo data and convert each column to appropriate R class
load_field_montecarlo<- function(){
  
  
  lapply("../r_markdown/figures/monte carlo output combined v1.csv",
         function(x)
           readr::read_csv(x, 
                           skip = 1,
                           col_names = c("PM Field", "PM Tier 1", "PM Tier 2", "PM Tier 3","PM Tier 4","CO Field","CO Tier 1",
                                         "CO Tier 2","CO Tier 3","CO Tier 4"),
                           col_types = 
                             cols(
                               `PM Field` = col_double(),
                               `PM Tier 1` = col_double(),
                               `PM Tier 2` = col_double(),
                               `PM Tier 3` = col_double(),
                               `PM Tier 4` = col_double(),
                               `CO Field` = col_double(),
                               `CO Tier 1` = col_double(),
                               `CO Tier 2` = col_double(),
                               `CO Tier 3` = col_double(),
                               `CO Tier 4` = col_double()
                               
                             ),
                           na = c("", "NA")
           )
         
  ) %>%
    dplyr::bind_rows()
    
}


#________________________________________________________

#________________________________________________________
# load EF Summary Table data and convert each column to appropriate R class
load_field_EFs <- function(){
  
  
  lapply("../data/field/aqe/EF Summary Table.csv",
         function(x)
           readr::read_csv(x, 
                           skip = 1,
                           col_names = c("field_site", "event_num", "datetime","hh_id", "stove","pm25_EF_gperkg","notes","BC_EF_gperkg","CO_EF_gperkg"),
                           col_types = 
                             cols(
                               field_site = col_character(),
                               event_num = col_double(),
                               datetime = col_character(),
                               hh_id = col_character(),
                               stove = col_character(),
                               pm25_EF_gperkg = col_double(),
                               notes = col_character(),
                               BC_EF_gperkg = col_double(),
                               CO_EF_gperkg = col_double()
                             ),
                           na = c("", "NA")
           )
         
  ) %>%
    dplyr::bind_rows() %>%
    
    dplyr::mutate(datetime = mdy(datetime))
}


#________________________________________________________

#________________________________________________________
# load calibrated data and convert each column to appropriate R class
load_field_lascar <- function(){
  
  
    lapply(list.files("../data/field/lascar CO",
                    pattern = ".txt",
                    full.names = TRUE),
         function(x)
            readr::read_delim(x, 
                           skip = 2, delim = "\t",
                           col_names = c("calibratedCOppm", "time_hrs", "date", "time","uncalibrated","empty","ln_calibratedCOppm"),
                           col_types = 
                             cols(
                               calibratedCOppm = col_double(),
                               time_hrs = col_double(),
                               date = col_character(),
                               time = col_character(),
                               uncalibrated = col_double(),
                               empty = col_double(),
                               ln_calibratedCOppm = col_double()
                             ),
                           na = c("", "NA")
           ) %>%
           dplyr::mutate(filename = x)
         
  ) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(datetime = parse_date_time(paste(date, time),orders = c("y-m-d HMS", "m/d/y HMS"))) %>%
    dplyr::filter(!is.na(uncalibrated)) %>%
    dplyr::mutate(height = substring(filename, 
                                     sapply(filename, function(x) tail(unlist(gregexpr('_',x,perl=TRUE)),1)[1])+1, 
                                     sapply(filename, function(x) unlist(gregexpr('.txt',x,perl=TRUE))-1))) %>%
    dplyr::mutate(lascar_id = substring(filename, 
                                     sapply(filename, function(x) tail(unlist(gregexpr('_',x,perl=TRUE)),1)[1])-2, 
                                     sapply(filename, function(x) tail(unlist(gregexpr('_',x,perl=TRUE)),1)[1])-1)) 
}


#________________________________________________________

