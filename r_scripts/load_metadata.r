#________________________________________________________
# load relevant libraries
  library(tidyverse)
  library(lubridate)
  library(readxl)
  #________________________________________________________

#________________________________________________________
# load field metadata and convert each column to appropriate R class
load_field_meta <- function(){

    readr::read_csv("../data/field/meta/field_meta.csv",
                  col_names = TRUE,
                  col_types =
                    cols(
                      .default = col_character(),
                      test_num = col_integer(),
                      field_site = col_factor(levels = c("india", "uganda",
                                                         "china", "honduras")),
                      date = col_date(format = "%m/%d/%y"),
                      pre_bkgd_start = col_time(),
                      pre_bkgd_end = col_time(),
                      sample_start = col_time(),
                      sample_end = col_time(),
                      post_bkgd_start = col_time(),
                      post_bkgd_end = col_time(),
                      fuel_pre_weigh = col_double(),
                      fuel_post_weigh = col_double()),
                  na = c("", "NA")
                  ) %>%
    dplyr::mutate(datetime_sample_start = as_datetime(as.POSIXct(date) + sample_start)) %>%
   dplyr::mutate(datetime_sample_end = as_datetime(as.POSIXct(date) + sample_end)) %>%
   dplyr::mutate_if(is.difftime, funs(as.numeric(hms(.))))  # convert times to secs in day

}
#________________________________________________________

#________________________________________________________
# load field flow rates and convert each column to appropriate R class
load_field_flows <- function(){

  readr::read_csv("../data/field/meta/inst_flows.csv",
           col_names = TRUE,
           col_types = 
             cols(
               .default = col_double(),
               hh_id = col_character(),
               inst = col_factor(levels = c("filter_1", "filter_2", "bypass",
                                            "aqe", "microaeth", "smps")),
               notes = col_character()
               ),
           na = c("", "NA")
           )

}
#________________________________________________________

#________________________________________________________
# load field filter metadata and convert each column to appropriate R class
load_field_filter_meta <- function(){

  readr::read_csv("../data/field/meta/field_grav_meta.csv",
           col_names = TRUE,
           col_types = 
             cols(
               .default = col_character(),
               date = col_date(format = "%m/%d/%y"),
               cart_type = col_factor(levels = c("single", "double"))
               ),
           na = c("", "NA")
           )

}
#________________________________________________________

#________________________________________________________
# load temp metadata and convert each column to appropriate R class
load_field_temp_meta <- function(){

  readr::read_csv("../data/field/meta/field_temp_meta.csv",
           col_names = TRUE,
           col_types = 
             cols(
               .default = col_character(),
               field_site = col_factor(levels = c("india", "uganda",
                                                  "china", "honduras")
                                       ),
               logger_type = col_factor(levels = c("omega", "sums")),
               start_date = col_character(),
               end_date = col_character()
               
               ),
           na = c("", "NA")
           ) %>%
             # convert time to secs in day and fix file problems
            dplyr::mutate(start_date = mdy(start_date))  %>%
            dplyr::mutate(end_date = mdy(end_date))
}
#________________________________________________________

#________________________________________________________
# load field notes and convert each column to appropriate R class
load_field_notes <- function(){

  readr::read_csv("../data/field/meta/field_notes.csv",
           col_names = TRUE,
           col_types = 
             cols(
               .default = col_character()
               ),
           na = c("", "NA")
           )

}
#________________________________________________________

#________________________________________________________
# load field events and convert each column to appropriate R class
load_field_events <- function(){

  readr::read_csv("../data/field/meta/field_events.csv",
           col_names = TRUE,
           col_types = 
             cols(
               .default = col_character(),
               field_site = col_factor(levels = c("india", "uganda",
                                                  "china", "honduras")
                                       ),
               time = col_time(format = "")
               ),
           na = c("", "NA")
           )
  
}
#________________________________________________________

#________________________________________________________
# load field lhvs and convert each column to appropriate R class
load_field_fuel_lhv <- function(){
  
  readr::read_csv("../data/field/meta/field_fuel_lhv.csv",
                  col_names = TRUE,
                  col_types = 
                    cols(
                      .default = col_character(),
                      field_site = col_factor(levels = c("india", "uganda",
                                                         "china", "honduras")),
                      lhv = col_integer()
                    ),
                  na = c("", "NA")
  )
  
}
#________________________________________________________

#________________________________________________________
# load field lhvs and convert each column to appropriate R class
load_field_fuel_carbon <- function(){
  
  readr::read_csv("../data/field/meta/field_fuel_carbon.csv",
                  col_names = c("sample_id", "nitrogen", "carbon", "notes"),
                  skip = 16,
                  n_max = 81,
                  col_types = 
                    cols(
                      .default = col_character()
                    ),
                  na = c("", "NA")
  )
  
}
#________________________________________________________

#________________________________________________________
# load lab metadata and convert each column to appropriate R class
load_lab_meta <- function(){
  
  readr::read_csv("../data/lab/meta/lab_meta.csv",
                  col_names = TRUE,
                  col_types =
                    cols(.default = col_character()),
                  na = c("", "NA")
                  )
}


#________________________________________________________

#________________________________________________________

# load field lhvs and convert each column to appropriate R class
load_honduras_behavior <- function(){
  
  asdf <- read_excel("../data/field/behavioral/honduras_behavior_R00_phase1.xlsx") %>%
        dplyr::mutate(date = gsub(" UTC","",date)) %>%
        dplyr::mutate(date = as.POSIXct(strptime(date, "%a %b %d %H:%M:%S %Y",tz = "UTC")))

}

#________________________________________________________

#________________________________________________________

# load air exchange rate information based on CO decays.
load_air_exchange_rates_india <- function(){
  
  read_excel("../data/field/lascar CO/20160410_Lascar_Calibration.xlsx",sheet = "air exchange rates",skip = 1)
  
}
#________________________________________________________

