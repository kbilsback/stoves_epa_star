#________________________________________________________
# Libraries
  library(tidyverse)
#________________________________________________________

#________________________________________________________ 
# Load single files
load_singlefiles <- function(inst){

  # data log (synax)
  #if(inst == "field_grav"){
    #filelist <- list.files("../data/field/grav", "india_grav", full.names = TRUE)
    #out <- load_field_grav(filelist[1])
  #}

  # return
  #return(out)
}
#________________________________________________________

#________________________________________________________
# Load multifile folders
load_multifile <- function(fldr, pattern, inst){

  filelist <- list.files(fldr, full.names = TRUE, ignore.case = TRUE)

  # loop files
  for(i in 1:length(filelist)){

    print(filelist[i])

    # temp
    if(inst == "temp"){
      ifelse(i == 1, out <- load_temp_file(filelist[i]), out <- rbind(out, load_temp_file(filelist[i])))
    }
    
    # sums
    if(inst == "sums"){
      ifelse(i == 1, out <- load_sums_file(filelist[i]), out <- rbind(out, load_sums_file(filelist[i])))
    }
    
    # grav
    if (inst == "grav"){
      ifelse(i == 1, out <- load_grav_file(filelist[i]), out <- rbind(out, load_grav_file(filelist[i])))
    }
  }

  # return
  return(out)
}
#________________________________________________________

#________________________________________________________
# Load temp file
# file <- "../data/temp/INXX_loggerid_date.csv"
load_temp_file <- function(file){

  data <- read.csv(file, fill = TRUE, stringsAsFactors = FALSE, col.names = c("date", "time", "temp", "logger_id"))

  data <- dplyr::mutate(data, logger_id = data[2,3]) %>%
          dplyr::filter(grepl("^[0-9]", date))

  if(nchar(data$time[1]) == 10){
    data <- dplyr::mutate(data, time = as.character(strftime(strptime(time, "%I:%M:%S %p"), "%H:%M:%S")))
  } 
  if(substring(data$date[1], 1, 1) == "0"){
    data <- dplyr::mutate(data, date = as.character(as.Date(data$date, "%d/%m/%Y"))) 
  }else{
    data <- dplyr::mutate(data, date = as.character(as.Date(date, "%m/%d/%y")))
  }

  data <- dplyr::mutate(data, datetime = as.POSIXct(paste(data$date, data$time), 
                        format = "%Y-%m-%d %H:%M:%S"))

  data <- dplyr::mutate(data, date = as.POSIXct(date))

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
load_sums_file <- function(file){

  data <- read.csv(file, fill = TRUE, stringsAsFactors = FALSE, header = FALSE, skip = 20, col.names = c("datetime", "units", "stove_temp"))

  data <- dplyr::mutate(data, logger_id = gsub(".*: ", "", read.csv(file, stringsAsFactors = FALSE, nrows = 1, col.names = "id")))
  
  if(substring(data$date[1], 6, 7) == "00"){
    data <- dplyr::mutate(data, datetime = gsub("00", "16", datetime))
  }

  data <- dplyr::mutate(data, datetime = as.POSIXct(strftime(strptime(datetime,
                                                                      "%d/%m/%y %I:%M:%S %p")), "%Y-%m-%d %H:%M:%S")) %>%
          dplyr::mutate(date = as.POSIXct(format(datetime, "%Y-%m-%d"))) %>%
          dplyr::mutate(time = as.character(datetime)) %>%
          dplyr::mutate(time = as.numeric(substr(time, 12, 13)) * 60 * 60 + 
                               as.numeric(substr(time, 15, 16)) * 60 +
                               as.numeric(substr(time, 18, 19)))
  
  data <- dplyr::mutate(data, stove_temp = as.numeric(stove_temp))
  
  # return
  return(data)
}
#________________________________________________________

#________________________________________________________ 
# Load grav file
load_field_grav <- function(file){

  data <- read.csv(file, header = TRUE, stringsAsFactors = FALSE, fill = FALSE, na.strings = c("NA"))

  data <- dplyr::mutate(data, pre_date = as.character(as.Date(data$pre_date, "%d/%m/%y"))) %>%
          dplyr::mutate(post_date = as.character(as.Date(data$post_date, "%d/%m/%Y"))) 


  # return 
  return(data)
  
}
#________________________________________________________