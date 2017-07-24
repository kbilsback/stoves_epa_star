#________________________________________________________
# require libraries
  library(tidyverse)
#________________________________________________________

#________________________________________________________
# calculate the rmse for each stove fuel combination
rmse_id_avg <- function(model, lab_data, emissions){

  rmse <- lab_data %>%
          dplyr::group_by(stove, fuel) %>%
          dplyr::do(data.frame(rmse = rmse_predict(model, ., emissions)))

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
rmse_predict <- function(model, lab_data, emissions){

  predict <- predict.glm(model, newdata = lab_data)
  residuals <- (predict - emissions)
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
  trans$fp_log <- log10(df$fp)                # fp log transformed

  trans[, 1] <- NULL # remove copy

  return(trans)
}
#________________________________________________________

#________________________________________________________
# mfp - analysis
mfp_bootstrap <- function(data, eqn, reps){
  # output data frame
  out <- data.frame(df.initial=numeric(1), select=numeric(1), alpha=numeric(1),
                    df.final=numeric(1), power1=factor(1), power2=factor(1), names=character(1), rep=numeric(1))
  # unique participants
  ids <- unique(lab_data_bc$id)
  # set seed
  set.seed(131313)
  # replicate loop
  for(i in seq(from = 1, to = reps, by = 1)){
    # sample ids (with replacement)
    id_sample <- sort(sample(ids,length(ids), replace = TRUE))
    for(j in seq(from = 1, to = length(id_sample), by = 1)){
      sample_tmp <- subset(lab_data_bc, id == id_sample[j])
      # build sample dataframe
      if(j==1){
        sample_data <- sample_tmp
      }
      else{
        sample_data <- rbind(sample_data, sample_tmp) 
      }
    }
    # run mfp on sample
    if(exists("out_tmp")==TRUE){
      rm(out_tmp)
    }
    try(out_tmp <- mfp_run(sample_data), silent = FALSE)
    #
    if(exists("out_tmp")==TRUE){
      out_tmp$rep <- i
      out <- rbind(out, out_tmp)
    }
  }
  # remove first row
  out = out[-1,]
  # return
  return(out)
}
#________________________________________________________________________

#________________________________________________________________________
# mfp - analysis
mfp_run <- function(sample_data){
  mfp_hr <- mfp(eqn, data = sample_data, select = 0.05)
  out <- as.data.frame(mfp_hr$fptab)
  out$names <- row.names(mfp_hr$fptab)
  return(out)
}
#________________________________________________________________________
