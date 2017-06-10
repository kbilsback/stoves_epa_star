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
# load field temp meta data 
load_field_temp_meta <- function(){

  file <- list.files("../data/field/meta", "field_temp_meta.csv", full.names = TRUE)

  print(file)

  data <- read.csv(file, header = TRUE, stringsAsFactors = FALSE, fill = TRUE, na.strings = c("NA"))

  data <- dplyr::mutate(data, field_site = as.factor(field_site)) %>%
          dplyr::mutate(logger_type = as.factor(logger_type))

  # return 
  return(data)

}
#________________________________________________________

#________________________________________________________
# load field filter meta data 
load_field_filter_meta <- function(){

  file <- list.files("../data/field/meta", "field_grav_meta.csv", full.names = TRUE)

  print(file)

  data <- read.csv(file, header = TRUE, stringsAsFactors = FALSE, fill = TRUE, na.strings = c("", "NA"))
  
  data <- dplyr::mutate(data, date = as.character(as.Date(date, "%m/%d/%y"))) %>%
          dplyr::mutate(date = as.POSIXct(date, tz = "Asia/Calcutta"))
  # will need to add additional exceptions for other field sites (use replace function)

  data <- dplyr::mutate(data, hh_id = as.factor(hh_id),
                              cart_type = as.factor(cart_type),
                              filter_type = as.factor(filter_type))

  # return 
  return(data)
  
}
#________________________________________________________

#________________________________________________________
# load field flow rates
load_field_flows <- function(){

  file <- list.files("../data/field/meta", "inst_flows.csv", full.names = TRUE)

  print(file)
  
  data <- read.csv(file, header = TRUE, stringsAsFactors = FALSE, fill = TRUE, na.strings = c("", "NA"))
  
  data <- dplyr::mutate(data, hh_id = as.factor(hh_id),
                        inst = as.factor(inst))

  # return 
  return(data)

}
#________________________________________________________

#________________________________________________________
# load field notes
load_field_notes <- function(){

  file <- list.files("../data/field/meta", "field_notes.csv", full.names = TRUE)

  print(file)

  notes <- read_csv(file)

  # classes
  notes <- dplyr::mutate(notes, 
                         hh_id = factor(hh_id),
                         qc = factor(qc, levels = c("bad", "maybe", "ok")))

  # return
  return(notes)
}
#________________________________________________________   