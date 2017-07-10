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
# Calculate the root mean square error for a model prediction
rmse_predict <- function(model, lab_data){

  predict <- predict.glm(model, newdata = lab_data)
  residuals <- (predict - lab_data$fp)
  # rmse
  return(sqrt(sum(residuals^2)/length(residuals)))

}
#________________________________________________________

#________________________________________________________
# Leave one out basic hr
leave_one_out <- function(eqn, lab_data) {

  rmse <- lab_data %>%
          dplyr::group_by(stove, fuel) %>%
          dplyr::do(rmse = leave_one_out(., stove, fuel))

  # model full
  mod <- glm(eqn, data = lab_data)
  rmse$rmse_full = rmse_predict(mod, lab_data)
  # add id column
  rmse$id <- ids
  # return
  return(rmse)

}
#_______________________________________________________

#________________________________________________________
leave_one_out_rmse <- function(lab_data, stove_type, fuel_type){

  # training
  in_data <- lab_data %>%
             dplyr::filter(stove != stove_type) %>%
             dplyr::filter(fuel != fuel_type)

  # removed
  out_data <- lab_data %>%
              dplyr::filter(stove == stove_type) %>%
              dplyr::filter(fuel == fuel_type)

  mod <- glm(eqn, data = in_data)

  rmse <- rmse_predict(mod, out_data)
}
#________________________________________________________