#________________________________________________________
# Libraries
  library(tidyverse)
#________________________________________________________

#________________________________________________________
# Load temp file
load_field_temp <- function(){
  
  if (grepl("IN", file)) {
    timezone = "America/Denver"  # data in wrong timezone
  }

  data <- read.csv(file, fill = TRUE, stringsAsFactors = FALSE, col.names = c("date", "time", "temp", "logger_id"))

  data <- dplyr::mutate(data, logger_id = data[2,3]) %>%
          dplyr::filter(grepl("^[0-9]", date))

  if(nchar(data$time[1]) == 10){
    data <- dplyr::mutate(data, time = as.character(strftime(strptime(time, "%I:%M:%S %p"), "%H:%M:%S")))
  } 
  if(substring(data$date[1], 1, 1) == "0"){
    data <- dplyr::mutate(data, date = as.character(as.Date(date, "%d/%m/%Y"))) 
  }else{
    data <- dplyr::mutate(data, date = as.character(as.Date(date, "%m/%d/%y")))
  }

  data <- dplyr::mutate(data, datetime = as.POSIXct(paste(date, time), 
                        format = "%Y-%m-%d %H:%M:%S", tz = timezone))

  data <- dplyr::mutate(data, date = as.POSIXct(date, tz = timezone))

  data <- dplyr::mutate(data, time = as.character(as.POSIXct(strptime(time, "%H:%M:%S")))) %>%
          dplyr::mutate(time = as.numeric(substr(time, 12, 13)) * 60 * 60 + 
                               as.numeric(substr(time, 15, 16)) * 60 +
                               as.numeric(substr(time, 18, 19)))

  data <- dplyr::mutate(data, temp = as.numeric(temp))

  # return
  return(data)
}
#________________________________________________________

#________________________________________________________
# Load sums file
# file <- "../data/sums/XXX.csv"
load_field_sums <- function(file){

  return(lapply(list.files("../data/field/grav",
                           pattern = "grav.csv",
                           full.names = TRUE),
                function(x) read_csv(x, col_names = TRUE,
                                     col_types = cols(
                                       .default = col_double(),
                                       id = col_character(),
                                       pre_date = col_date(format = ""),
                                       blank_id = col_character(),
                                       post_date = col_date(),
                                       post_pressure = col_character(),
                                       post_blank_id = col_character(),
                                       notes = col_character()),
                                     na = c("", "NA", NA))) %>% 
           dplyr::bind_rows() %>%
           dplyr::mutate(data,
                         datetime = as.POSIXct(strftime(strptime(datetime,
                                                                 "%d/%m/%y %I:%M:%S %p")), "%Y-%m-%d %H:%M:%S",
                                               tz = timezone)))
  
  # will need to adjust grepl statement for different field sites
  if (grepl("", file)) {
    timezone = "Asia/Calcutta"
  }

  data <- read.csv(file, fill = TRUE, stringsAsFactors = FALSE, header = FALSE, skip = 20, col.names = c("datetime", "units", "stove_temp"))

  data <- dplyr::mutate(data, logger_id = gsub(".*: ", "", read.csv(file, stringsAsFactors = FALSE, nrows = 1, col.names = "id")))
  
  if(substring(data$date[1], 6, 7) == "00"){
    data <- dplyr::mutate(data, datetime = gsub("00", "16", datetime))
  }

  data <- dplyr::mutate(data, datetime = as.POSIXct(strftime(strptime(datetime,
                                                                      "%d/%m/%y %I:%M:%S %p")), "%Y-%m-%d %H:%M:%S",
                                                    tz = timezone)) %>%
          dplyr::mutate(date = as.POSIXct(format(datetime, "%Y-%m-%d"), tz = timezone)) %>%
          dplyr::mutate(time = as.character(datetime)) %>%
          dplyr::mutate(time = as.numeric(substr(time, 12, 13)) * 60 * 60 + 
                               as.numeric(substr(time, 15, 16)) * 60 +
                               as.numeric(substr(time, 18, 19)))

}
#________________________________________________________

#________________________________________________________ 
# Load grav file
load_field_grav <- function(){

  return(lapply(list.files("../data/field/grav",
                           pattern = "grav.csv",
                           full.names = TRUE),
                function(x) readr::read_csv(x, col_names = TRUE,
                                            col_types = cols(
                                              .default = col_double(),
                                              id = col_character(),
                                              pre_date = col_character(),
                                              blank_id = col_character(),
                                              post_date = col_date(format = "%d/%m/%Y"),
                                              post_pressure = col_character(),
                                              post_blank_id = col_character(),
                                              notes = col_character()),
                                            na = c("", "NA"))) %>% 
           dplyr::bind_rows() %>%
           dplyr::mutate(pre_date = as.Date(ifelse(grepl("^IN[A-Z]", pre_date),
                                           as.Date(pre_date, format = "%d/%m/%y"),
                                           as.Date(pre_date, format = "%m/%d/%y")),
                                           origin = "1970-01-01")))

}
#________________________________________________________

#________________________________________________________ 
# Load aqe file
load_field_aqe <- function(){

  return(lapply(list.files("../data/field/aqe",
                           pattern = "AQE.csv",
                           full.names = TRUE),
                function(x) readr::read_csv(x, skip = 1, trim_ws = TRUE,
                                            col_names = c("tag", "date", "time",
                                                          "temp_units", "pol_units",
                                                          "flow_units", "t_amb", "t_stack",
                                                          "t_preheat", "o2", "co", "co2",
                                                          "stack_draft", "so2", "velocity",
                                                          "pressure", "rh", "dew_point",
                                                          "wet_bulb_temp", "vocs"),
                                            col_types = cols(
                                              .default = col_double(),
                                              tag = col_character(),
                                              date = col_date(format = "%m/%d/%y"),
                                              time = col_time(format = "%H:%M:%S"),
                                              temp_units = col_character(),
                                              pol_units = col_character(),
                                              flow_units = col_character()),
                                            na = c("", "NA", "   NA"))) %>% 
           dplyr::bind_rows() %>%
           dplyr::mutate(datetime = as.POSIXct(paste(date, time), 
                                                     format = "%Y-%m-%d %H:%M:%S")) %>%
           dplyr::mutate(time = as.numeric(substr(datetime, 12, 13)) * 60 * 60 +
                                as.numeric(substr(datetime, 15, 16)) * 60 +
                                as.numeric(substr(datetime, 18, 19))))

}
#________________________________________________________