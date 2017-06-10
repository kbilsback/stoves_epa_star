#________________________________________________________
# load libraries
  library(tidyverse)
#________________________________________________________

#________________________________________________________
# load field meta data
load_field_meta <- function(){

  return(read_csv("../data/field/meta/field_meta.csv",
                  col_names = TRUE,
                  col_types = cols(
                    .default = col_character(),
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
                  col_types = cols(
                    .default = col_double(),
                    hh_id = col_character(),
                    inst = col_factor(levels = c("filter_1", "filter_2", "bypass",
                                                 "aqe", "microaeth", "smps")),
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
                    .default = col_character(),
                    date = col_date(format = "%m/%d/%y"),
                    cart_type = col_factor(levels = c("single", "double"))),
                  na = c("", "NA")))

}
#________________________________________________________

#________________________________________________________
# load field temp meta data 
load_field_temp_meta <- function(){

  return(read_csv("../data/field/meta/field_temp_meta.csv",
                  col_names = TRUE,
                  col_types = cols(
                    .default = col_character(),
                    field_site = col_factor(levels = c("india", "uganda",
                                                       "china", "honduras")),
                    logger_type = col_factor(levels = c("omega", "sums"))),
                  na = c("", "NA")))

}
#________________________________________________________

#________________________________________________________
# load field notes
load_field_notes <- function(){

  return(read_csv("../data/field/meta/field_notes.csv",
                  col_names = TRUE,
                  col_types = cols(
                    .default = col_character()),
                  na = c("", "NA"))) 

}
#________________________________________________________

#________________________________________________________
# load field notes
load_field_events <- function(){
  
  return(read_csv("../data/field/meta/field_events.csv",
                  col_names = TRUE,
                  col_types = cols(
                    .default = col_character(),
                    field_site = col_factor(levels = c("india", "uganda",
                                                       "china", "honduras")),
                    time = col_time(format = "")),
                  na = c("", "NA"))) 
  
}
#________________________________________________________