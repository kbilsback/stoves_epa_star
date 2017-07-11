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

#________________________________________________________
# Transform data
transform <- function(df){
  trans <- as.data.frame(df$bc_rate_trans)  # copy
  trans$id <- df$id                         # id
  trans$sample_id <- df$sample_id           # sample id
  trans$bc_rate_trans <- df$bc_rate_trans   # bc rate transformed by boxcox
  trans$stove <- df$stove                   # stove
  trans$fuel <- df$fuel                     # fuel
  trans$fp_nsqrt <- I(df$fp^-0.5)           # fp square room transformed
  trans$fp_log <- log(df$fp)                # fp log transformed

  trans[, 1] <- NULL # remove copy

  return(trans)
}
#________________________________________________________
