#________________________________________________________
# require libraries
  library(tidyverse)
#________________________________________________________

#________________________________________________________
# calculate the rmse for each stove fuel combination
rmse_id_avg <- function(model, lab_data){

  rmse <- lab_data %>%
          dplyr::group_by(stove, fuel) %>%
          dplyr::do(data.frame(rmse = rmse_predict(model, .)))

}
#________________________________________________________

#________________________________________________________
# Calculate the root mean square error
calc_rmse <- function(model){
  mod_resid_sq <- residuals(model)^2
  return(sqrt(sum(mod_resid_sq)/length(mod_resid_sq)))
}
#________________________________________________________

#________________________________________________________
# Calculate the root mean square error for a model prediction
rmse_predict <- function(model, lab_data){

  predict <- predict.glm(model, newdata = lab_data)
  residuals <- (predict - lab_data$fp)
  # rmse
  return(sqrt(sum(residuals^2)/length(residuals)))

}
#________________________________________________________
