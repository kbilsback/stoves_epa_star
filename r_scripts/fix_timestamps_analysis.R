source('load.R')
source('iassist_convert_function.R')
source('fix_timestamp_function.R')

###INSTRUCTIONS
#These scripts fix the timestamps on iButton data so that it may be ingested by sumsarized.  
#Corrected data is saved into a folder from where the files were selected.
#Need to update to input Wellzion data and convert it to iButton data.

#interactive file selection to fix time stamps and plot the data.
files_plot <- tk_choose.files(default = "", caption = "Select files",
                              multi = TRUE, filters = NULL, index = 1)


l_ply(files_plot, fileCleanerTimeStamp, .progress='text') #Fix timestamps and headers if necessary.

#l_ply(files_plot, fileFlip, .progress='text')#File may have timestamp flipped...this reverses it.

#l_ply(files_plot, fileCleaner, .progress='text') #Get minute averages out of thermocouple data.

#l_ply(files_plot, iassist.import, .progress='text', zip=F)# Convert iassist data to ibutton formatted data.




filerun <- files_plot[1]






















