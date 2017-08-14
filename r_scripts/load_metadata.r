#________________________________________________________
# load relevant libraries
  library(tidyverse)
  library(lubridate)
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
               start_date = col_date(format = "%m/%d/%y")

               ),
           na = c("", "NA")
           )

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
