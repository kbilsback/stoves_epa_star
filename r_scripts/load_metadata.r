#________________________________________________________
# Libraries
  library(tidyverse)
  library(forcats)
#________________________________________________________

#________________________________________________________ 
# Load single files
load_meta <- function(log){

  # data log
  if(log == "field_meta"){
    filelist <- list.files("../data/field/meta", "field_meta", full.names = TRUE)
    out <- load_field_meta(filelist[1])
  }

  # data log
  if(log == "field_temp_meta"){
    filelist <- list.files("../data/field/meta", "temp_meta", full.names = TRUE)
    out <- load_field_temp_meta(filelist[1])
  }

  # notes
  if(log == "field_notes"){
    filelist <- list.files("../data/field/meta", "notes", full.names = TRUE)
    out <- load_field_notes(filelist[1])
  }

  # return
  return(out)
}
#________________________________________________________

#________________________________________________________
# Load data log
load_field_meta <- function(file){

  data <- read.csv(file, header = TRUE, stringsAsFactors = FALSE, fill = TRUE, na.strings = c("NA"))

  data <- dplyr::mutate(data, date = as.Date(date, "%m/%d/%y")) %>%
          dplyr::mutate(date = as.POSIXct(as.character(date)))

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

  data <- dplyr::mutate(data, field_site = as.factor(field_site)) %>%
          dplyr::mutate(hh_id = as.factor(hh_id))
          dplyr::mutate(stove_type = as.factor(stove_type)) %>%
          dplyr::mutate(fuel_type = as.factor(fuel_type))

  # return 
  return(data)

}
#________________________________________________________

#________________________________________________________
# Load data log
load_field_temp_meta <- function(file){

  data <- read.csv(file, header = TRUE, stringsAsFactors = FALSE, fill = TRUE, na.strings = c("NA"))

  data <- dplyr::mutate(data, field_site = as.factor(field_site)) %>%
          dplyr::mutate(logger_type = as.factor(logger_type))

  # return 
  return(data)

}
#________________________________________________________

#________________________________________________________
# load field notes
load_field_notes <- function(file, grep_str){
  # read csv file
  notes <- read_csv(file)

  # filter for instrument
  notes <- dplyr::filter(notes, grepl(grep_str, notes$inst) == TRUE)

  # classes
  notes <- dplyr::mutate(notes, 
                         hh_id = factor(hh_id),
                         inst = factor(inst),
                         qc = factor(qc, levels = c("bad", "maybe", "ok")))

  # return
  return(notes)
}
#________________________________________________________   