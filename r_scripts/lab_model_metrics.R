#________________________________________________________
# require libraries
  library(tidyverse)
#________________________________________________________

#________________________________________________________
rmse_id_avg <- function(model, data_frame){
    # extract list of unique ids
    ids <- unique(data_frame$id)
    # rmse data frame
    rmse <- data.frame (rmse_lpm = numeric(length(ids)), rmse_pct = numeric(length(ids)))
    # rmse for each id
    for (i in seq(from = 1, to = length(ids), by = 1)){
      # data from one id
      id_data <- subset(data_frame, id == ids[i]) 
      # predict
      rmse$rmse_lpm[i] <- rmse_predict_f(model, id_data)
      rmse$rmse_pct[i] <- rmse_predict_f(model, id_data)
    }
    return(rmse)
}
#________________________________________________________
