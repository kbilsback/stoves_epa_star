#________________________________________________________
# Libraries
  library(tidyverse)
#________________________________________________________

#________________________________________________________ 
# Load single files
load_singlefiles <- function(log){

  # data log (synax)
  #if(log == "field_meta"){
    #filelist <- list.files("../data/field/meta", "field_meta", full.names = TRUE)
    #out <- load_meta_file(filelist[1])
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
