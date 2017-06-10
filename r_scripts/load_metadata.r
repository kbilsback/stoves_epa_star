#________________________________________________________
# load libraries
  library(tidyverse)
#________________________________________________________

#________________________________________________________
# load field meta data
load_field_meta <- function(){

  return(read_csv("../data/field/meta/field_meta.csv",
                  col_names = TRUE,
                  col_types = cols(.default = col_character(),
                                   test_num = col_integer(),
                                   field_site = col_factor(levels = c("india", "uganda",
                                                                      "china", "honduras")),
                                   date = col_date(format = "%m/%d/%y"),
                                   pre_bkgd_start = col_time(format = ""),
                                   pre_bkgd_end = col_time(format = ""),
                                   sample_start = col_time(format = ""),
                                   sample_end = col_time(format = ""),
                                   post_bkgd_start = col_time(format = ""),
                                   post_bkgd_end = col_time(format = ""),
                                   fuel_pre_weigh = col_double(),
                                   fuel_post_weigh = col_double()),
                  na = c("", "NA")))

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