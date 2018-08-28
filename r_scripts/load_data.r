#________________________________________________________
# load relevant libraries
  library(tidyverse)
  library(lubridate)
  library(readxl)

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
#file <- "../data/lab/grav/grav.csv"
load_lab_grav <- function(){
  #test <-
  readr::read_csv("../data/lab/grav/grav.csv",
                  skip = 1,
                  col_names = c("id", "date", "sample_id", "start_time",
                                "end_time", "pm_mass", "pm_ef", "ir_atn",
                                "uv_atn", "mce", "fp", "bc_mass", "bc_ef",
                                "pm_flag", "bc_flag", "pm_rate", "bc_rate", "co_mass", "co_ef", "bb_pm_flag", "bb_bc_flag"),
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
                      bc_rate = col_double(),
                      co_mass = col_double(),
                      co_ef = col_double(),
                      bb_pm_flag = col_double(),
                      bb_bc_flag = col_double()
                    ), na = c("", "NaN")
                  )
}
#________________________________________________________

#________________________________________________________
# load jetter data and convert each column to appropriate R class
#file <- "../data/other_studies/jetter_data.csv"
load_wbt_data <- function(){
  readr::read_csv("../data/other_studies/wbt_data.csv",
                  col_names = c("id", "study", "var", "value", "units",
                                "n_test", "pol", "stove", "fuel",
                                "fuel_notes", "protocol", "protocol_notes", "ref", "notes"),
                  skip = 1,
                  col_types = 
                    cols(
                      id = col_character(),
                      study = col_character(),
                      var = col_character(),
                      value = col_number(),
                      units = col_character(),
                      n_test = col_integer(),
                      pol = col_character(),
                      stove = col_character(),
                      fuel = col_character(),
                      fuel_notes = col_character(),
                      protocol = col_character(),
                      protocol_notes = col_character(),
                      ref = col_character(),
                      notes = col_character()),
                  na = c("")
  )
}
#________________________________________________________

#________________________________________________________
# load jetter data and convert each column to appropriate R class
#file <- "../data/other_studies/roden_data.csv"
load_field_data <- function(){
  readr::read_csv("../data/other_studies/field_data.csv",
                  col_names = c("study", "id", "var", "value", "units",
                                "pol", "stove", "stove_code", "fuel",
                                "fuel_notes", "protocol", "protocol_notes", "ref", "notes"),
                  skip = 1,
                  col_types = 
                    cols(
                      study = col_character(),
                      id = col_character(),
                      var = col_character(),
                      value = col_double(),
                      units = col_character(),
                      pol = col_character(),
                      stove = col_character(),
                      stove_code = col_character(),
                      fuel = col_character(),
                      fuel_notes = col_character(),
                      protocol = col_character(),
                      protocol_notes = col_character(),
                      ref = col_character(),
                      notes = col_character()
                    ),
                  na = c("")
  )
}
#________________________________________________________

#________________________________________________________
# load grav data and convert each column to appropriate R class
#file <- "../data/field/rose_field_data.csv"
load_rose_data_v1 <- function(){
  #test <-
  readr::read_csv("../data/field/emissions/rose_field_data_v1.csv",
                  col_names = c("field_site", "hh_id", "stove", "fuel",
                                "pm_ef", "bc_ef", "co_ef", "notes"),
                  skip = 1,
                  col_types = 
                  cols(
                    field_site = col_character(),
                    hh_id = col_character(),
                    stove = col_character(),
                    fuel = col_character(),
                    pm_ef = col_double(),
                    bc_ef = col_double(),
                    co_ef = col_double(),
                    notes = col_character()
                  ),
                  na = c("")
  )
}
#________________________________________________________

#________________________________________________________
# load grav data and convert each column to appropriate R class
#file <- "../data/field/rose_field_data_v2.csv"
load_rose_data_v2 <- function(){
  #test <-
  readr::read_csv("../data/field/emissions/rose_field_data_v2.csv",
                  na = c(""),
                  col_types = 
                  cols(
                    country = col_character(),
                    id = col_character(),
                    stove = col_character(),
                    fuel = col_character(),
                    test_length = col_double(),
                    oc_ef = col_double(),
                    om_ef = col_double(),
                    ec_ef = col_double(),
                    om_ec_ef = col_double(),
                    ocec_ratio = col_double(),
                    pm_ef = col_double(),
                    bc_ef = col_double(),
                    mce = col_double(),
                    co_ef = col_double()
                  ))
}
#________________________________________________________

#________________________________________________________
# load temp data and convert each column to appropriate R class
#file <- "../data/lab/temp/hood_a/10A.xlsx"
load_lab_temp_a <- function(){
  
  # files are read in files twice to extract data and logger id
  #test <-
  lapply(list.files("../data/lab/temp/hood_a",
                    pattern = ".xlsx",
                    full.names = TRUE),
         function(file)
          #test <-
           readxl::read_excel(file, sheet = 2,
                              range = cell_cols(c("J", "K"))) %>%
           dplyr::bind_cols(readxl::read_excel(file, sheet = 2,
                                               range = cell_cols(c("AB")))) %>%
           dplyr::mutate(test_id = gsub(".*a/","", file),
                         test_id = gsub("[:.:]xlsx","", test_id)) %>%
           dplyr::mutate(time = as.numeric(hms(format(Time, "%H:%M:%S"))),
                         date = as.Date(Time))) %>%
  dplyr::bind_rows() %>%
  dplyr::rename("water_temp"= `Water Temp (deg K)`,
                "exhaust_temp" = `TC C1 (deg K)`,
                "datetime" = Time)
}
#________________________________________________________

#________________________________________________________
# load temp data and convert each column to appropriate R class
#file <- "../data/lab/temp/hood_a/10A.xlsx"
load_lab_temp_b <- function(){
  
  # files are read in files twice to extract data and logger id
  #test <-
  lapply(list.files("../data/lab/temp/hood_b/",
                    pattern = ".csv",
                    full.names = TRUE),
         function(file)
           #test <-
           readr::read_csv(file)) %>%
    dplyr::bind_rows() %>%
    dplyr::rename("water_temp"= "temp")
}
#________________________________________________________

#________________________________________________________
# load grav data and convert each column to appropriate R class
#file <- "../data/lab/grav/grav.csv"
load_flue_temp <- function(){
  #test <-
  readr::read_csv("../data/lab/temp_fp/flue_temp.csv",
                  skip = 1,
                  col_names = c("id", "date", "time", "temp"),
                  col_types = 
                    cols(
                      id = col_character(),
                      date = col_character(),
                      time = col_time(format = ""),
                      temp = col_double()
                    ),
                  na = c("", "NaN")
  )
}
#________________________________________________________

#________________________________________________________
# load grav data and convert each column to appropriate R class
#file <- "../data/lab/grav/grav.csv"
load_firepower <- function(){
  #test <-
  readr::read_csv("../data/lab/temp_fp/firepower.csv",
                  skip = 1,
                  col_names = c("id", "date", "time", "firepower"),
                  col_types = 
                    cols(
                      id = col_character(),
                      date = col_character(),
                      time = col_time(format = ""),
                      firepower = col_double()
                    ),
                  na = c("", "NaN")
  )
}
#________________________________________________________