#________________________________________________________
# require libraries
  library(tidyverse)
#________________________________________________________

#________________________________________________________
# calculate the rmse for each stove fuel combination
rmse_id_avg <- function(model, df){

  rmse <- df %>%
          dplyr::group_by(stove, fuel) %>%
          dplyr::do(data.frame(rmse_kw = rmse_predict(model, .)))

}
#________________________________________________________

#________________________________________________________
# Calculate the root mean square error for a model prediction
rmse_predict <- function(model, df){

  predict <- predict.glm(model, newdata = df)
  residuals <- (predict - df$fp)
  # rmse
  return(sqrt(sum(residuals^2)/length(residuals)))

}
#________________________________________________________
