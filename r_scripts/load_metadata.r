#________________________________________________________
# load libraries
  library(tidyverse)
#________________________________________________________

#________________________________________________________
# load field meta data
load_field_meta <- function(){

  file <- list.files("../data/field/meta", "field_meta.csv", full.names = TRUE)

  print(file)

  data <- read.csv(file, header = TRUE, stringsAsFactors = FALSE, fill = TRUE, na.strings = c("NA"))

  data <- dplyr::mutate(data, date = as.character(as.Date(date, "%m/%d/%y"))) %>%
          dplyr::mutate(date = as.POSIXct(date, tz = "Asia/Calcutta"))
    # will need to add additional exceptions for other field sites (use replace function)

  data <- dplyr::mutate(data, pre_bkgd_start = as.numeric(substr(pre_bkgd_start, 1, 2)) * 60 * 60 + 
                        as.numeric(substr(pre_bkgd_start, 4, 5)) * 60 +
                        as.numeric(substr(pre_bkgd_start, 7, 8)))

  data <- dplyr::mutate(data, pre_bkgd_end = as.numeric(substr(pre_bkgd_end, 1, 2)) * 60 * 60 + 
                        as.numeric(substr(pre_bkgd_end, 4, 5)) * 60 +
                        as.numeric(substr(pre_bkgd_end, 7, 8)))

  data <- dplyr::mutate(data, sample_start = as.numeric(substr(sample_start, 1, 2)) * 60 * 60 + 
                        as.numeric(substr(sample_start, 4, 5)) * 60 +
                        as.numeric(substr(sample_start, 7, 8)))

  data <- dplyr::mutate(data, sample_end = as.numeric(substr(sample_end, 1, 2)) * 60 * 60 + 
                        as.numeric(substr(sample_end, 4, 5)) * 60 +
                        as.numeric(substr(sample_end, 7, 8)))
    
  data <- dplyr::mutate(data, post_bkgd_start = as.numeric(substr(post_bkgd_start, 1, 2)) * 60 * 60 + 
                        as.numeric(substr(post_bkgd_start, 4, 5)) * 60 +
                        as.numeric(substr(post_bkgd_start, 7, 8)))

  data <- dplyr::mutate(data, post_bkgd_end = as.numeric(substr(post_bkgd_end, 1, 2)) * 60 * 60 + 
                        as.numeric(substr(post_bkgd_end, 4, 5)) * 60 +
                        as.numeric(substr(post_bkgd_end, 7, 8)))

  data <- dplyr::mutate(data, field_site = as.factor(field_site),
                        hh_id = as.factor(hh_id),
                        stove_type = as.factor(stove_type),
                        fuel_type = as.factor(fuel_type))

  # return 
  return(data)

}
#________________________________________________________

#________________________________________________________
# load field flow rates
load_field_flows <- function(){

  return(read_csv("../data/field/meta/inst_flows.csv",
                  col_names = TRUE,
                  col_types = cols(hh_id = col_character(),
                                   inst = col_factor(levels = c("filter_1", "filter_2",
                                                                   "bypass", "aqe", "microaeth",
                                                                   "smps")),
                                   pre_flow_setting = col_double(),
                                   pre_flow_1 = col_double(),
                                   pre_flow_2 = col_double(),
                                   pre_flow_3 = col_double(),
                                   post_flow_setting = col_double(),
                                   post_flow_1 = col_double(),
                                   post_flow_2 = col_double(),
                                   post_flow_3 = col_double(),
                                   notes = col_character()),
                  na = c("", "NA")))

}
#________________________________________________________

#________________________________________________________
# load field filter meta data 
load_field_filter_meta <- function(){

  return(read_csv("../data/field/meta/field_grav_meta.csv",
                  col_names = TRUE, 
                  col_types = cols(
                                   date = col_date(format = "%m/%d/%y"),
                                   hh_id = col_character(),
                                   filter_type = col_character(),
                                   cart_id = col_character(),
                                   quartz_id = col_character(),
                                   teflon_id = col_character(),
                                   cart_type = col_factor(levels = c("single", "double")),
                                   notes = col_character()),
                  na = c("", "NA")))

}
#________________________________________________________

#________________________________________________________
# load field temp meta data 
load_field_temp_meta <- function(){

  return(read_csv("../data/field/meta/field_temp_meta.csv",
                  col_names = TRUE,
                  col_types = cols(field_site = col_factor(levels = c("india", "uganda",
                                                                      "china", "honduras")),
                                   hh_id = col_character(),
                                   alt_id = col_character(),
                                   logger_type = col_factor(levels = c("omega", "sums")),
                                   logger_id = col_character(),
                                   notes = col_character()),
                  na = c("", "NA")))

}
#________________________________________________________

#________________________________________________________
# load field notes
load_field_notes <- function(){

  return(read_csv("../data/field/meta/field_notes.csv",
                  col_names = TRUE,
                  col_types = cols(hh_id = col_character(),
                                   inst = col_character(),
                                   notes = col_character(),
                                   qc = col_character()),
                  na = c("", "NA"))) 

}
#________________________________________________________   