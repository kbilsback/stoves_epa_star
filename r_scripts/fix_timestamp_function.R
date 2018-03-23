library(plyr)
library(reshape2)
library(lubridate)
library(tools)
library(data.table)

fileCleanerTimeStamp <- function(filerun){
  
  #filerun<-"/Users/ricardopiedrahita/Dropbox/2016 ADB RFP Nigeria/SUMS processing/SUMS analysis R code/r_scripts/TC1_111316.csv"
  
  #extract the header. Import differently based on it.
  header <- read.csv(filerun, nrow=19, blank.lines.skip=F, header=F, stringsAsFactor=F)
  header1 <- read.csv(filerun, nrow=20, blank.lines.skip=F, header=F, stringsAsFactor=F)
  if (identical(header1$V1[21],"Unit")) { #For standard ibutton
    headerstart = 19
    header <- read.csv(filerun, nrow=headerstart, blank.lines.skip=F, header=F, stringsAsFactor=F)
    datas <- as.data.table(read.csv(filerun, skip=headerstart, colClasses=c('character', 'character', 'numeric')))
  } else if (identical(header1$V1[20],"Unit")) { #For weird ibutton
      headerstart = 18
      header <- read.csv(filerun, nrow=headerstart, blank.lines.skip=F, header=F, stringsAsFactor=F)
      datas <- as.data.table(read.csv(filerun, skip=headerstart, colClasses=c('character', 'character', 'numeric')))
    
  } else if  (identical(header$V1[16],"Unit")) { #For cellphone-downloaded ibutton
    headerstart = 15
    header <- read.csv(filerun, nrow=headerstart, blank.lines.skip=F, header=F, stringsAsFactor=F)
    datas <- as.data.table(read.csv(filerun, skip=headerstart, colClasses=c('character', 'character', 'numeric')))
    
  } else if  (identical(header$V2[9],"Unit")) { #For cellphone-downloaded ibutton
    headerstart = 8
    header <- read.csv(filerun, nrow=headerstart, blank.lines.skip=F, header=F, stringsAsFactor=F)
    datas <- as.data.table(read.csv(filerun, skip=headerstart, colClasses=c('character', 'character', 'numeric')))
    
  } else { #For Wellzion
    datas <- as.data.table(read.csv(filerun, sep="\"",fileEncoding="UCS-2LE", header=1, stringsAsFactor=F)) %>%
          dplyr::mutate(Unit = "C") %>%
          dplyr::rename(Date.Time = Timestamp,Value = Thermocouple.C.) %>%
          dplyr::select("Date.Time","Unit","Value") 
    header$V1 = NULL
    datas <- as.data.table(datas)
  }
  

  
  #extract the data
  #check for F files
  datas[Unit=="F", Value:=(Value-32) * 5/9]
  datas[Unit=="F", Unit:="C"]
  
  #If second column is units 'C' then time stamp is in correct format.  If not, need to combine date and time from the first and second column
  
  #check first datetime element
  DT1 <- datas[,length(unique(as.numeric(substring(Date.Time,1,2))))]
  #check second datetime element
  DT2 <- datas[,length(unique(as.numeric(substring(Date.Time,4,5))))]
  #check number of ":" in datetime stamp
  DTl <- unique(sapply(regmatches(datas[,Date.Time], gregexpr(":", datas[,Date.Time])), length))
  dir.create(path=paste(dirname(filerun),'/Corrected Timestamps',sep=""), showWarnings = FALSE)
  
  #If there are more unique values in DT1 than DT2, then the first value is days. This could be defeated if there is only one day of data.
  if((DT1>DT2) & all(DTl==2)){ 
    datas[,Date.Time:=dmy_hm(as.character(Date.Time))]
    #cat(paste(header$V1, collapse="\n"), file=paste(dirname(filerun),'/Corrected Timestamps/',basename(filerun),sep=""))
    write.table(datas, paste(dirname(filerun),'/Corrected Timestamps/',basename(filerun),sep=""), sep=",", append=TRUE, row.names=F, quote=F)
  }else {
    datas[,Date.Time:=mdy_hm(as.character(Date.Time))]
    #cat(paste(header$V1, collapse="\n"), file=paste(dirname(filerun),'/Corrected Timestamps/',basename(filerun),sep=""))
    write.table(datas, paste(dirname(filerun),'/Corrected Timestamps/',basename(filerun),sep=""), sep=",", append=TRUE, row.names=F, quote=F)
  }
}


fileFlip <- function(fileflip){
  #extract the header
  header <- read.csv(fileflip, nrow=8, blank.lines.skip=F, header=F, stringsAsFactor=F)
  #extract the data
  datas <- as.data.table(read.csv(fileflip, skip=8, colClasses=c('character', 'character', 'numeric')))
  
  datas$Date.Time <- lubridate::ymd_hms(datas$Date.Time ) 
  datas <- dplyr::arrange(datas, Date.Time)
  
  cat(paste(header$V1, "\n"), file=paste(file_path_sans_ext(fileflip),'dateflipped.csv',sep=""))
  write.table(datas, paste(file_path_sans_ext(fileflip),'dateflipped.csv',sep=""), sep=",", append=TRUE, row.names=F, quote=F)
  
}


