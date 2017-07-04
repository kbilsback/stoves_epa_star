#________________________________________________________
# require libraries
  library(tidyverse)
  library(gridExtra)
  library(ggfortify)
  library(magrittr)
#________________________________________________________

#________________________________________________________
# plot model and boxcox transformation

plot_boxcox_model <- function(data, eqn) {

  f <- formula(eqn)
  f_trans <- formula(gsub(" ~", "^trans ~", eqn))
  
  data <- data %>%
          dplyr::group_by(pol, stove_cat)
  
  models <- data %>%
            dplyr::do(model = lm(f, data = .))
  
  box_cox <- data %>% 
             dplyr::do(box_cox = MASS::boxcox(f, data =., plotit = FALSE)) %>%
             dplyr::mutate(trans = box_cox$x[which.max(box_cox$y)])
  
  data <- data %>%
          dplyr::left_join(dplyr::select(box_cox, stove_cat, trans, pol), by = c("stove_cat", "pol"))
  
  models_trans <- data %>%
                  dplyr::do(model = lm(f_trans, data = .))
  
  p1 <- ggplot(models %>% broom::glance(model),
          aes(x = stove_cat, y = pol, fill = r.squared)) +
          geom_tile(colour = "black") + 
          geom_text(aes(label = round(r.squared, 3))) +
          ggtitle(paste0(eqn,": basic model")) +
          theme_bw() + 
          scale_fill_gradientn(colors = terrain.colors(10),
                               limits = c(0, 1)) + 
          xlab("stove category") +
          ylab("pollutant") +
          theme(text = element_text(size = 16),
                axis.text.x = element_text(angle = 45, hjust = 1))
  
  p2 <- ggplot(models_trans %>% broom::glance(model),
               aes(x = stove_cat, y = pol, fill = r.squared)) +
    geom_tile(colour = "black") + 
    geom_text(aes(label = round(r.squared, 3))) +
    ggtitle(paste0(eqn,": boxcox transformed model")) +
    theme_bw() + 
    scale_fill_gradientn(colors = terrain.colors(10),
                         limits = c(0, 1)) + 
    xlab("stove category") +
    ylab("pollutant") +
    theme(text = element_text(size = 16),
          axis.text.x = element_text(angle = 45, hjust = 1))
  
  grid.arrange(p1, p2, ncol = 2)
  
}

#________________________________________________________


#________________________________________________________
# plot lm summary

plot_simple_lm <- function(data, eqn) {
  
  f <- formula(eqn)
  
  data <- data %>%
          dplyr::group_by(pol, stove_cat)
  
  stove_cats <- unique(data$stove_cat)
  pols <- unique(data$pol)

  lapply(stove_cats, function(y)
    lapply(pols, function(x)
      plot_diagnostics(data,
                       stovecat = y,
                       pol = x,
                       x_var = gsub("val ~ ", "", eqn))))

}

#________________________________________________________

#________________________________________________________

plot_diagnostics <- function(data, stove_cat, pol, x_var) {
  
  data <- data %>%
          dplyr::filter(stove_cat == stove_cat) %>%
          dplyr::filter(pol = pol)

  p1 <- ggplot(data, aes_string(x = x_var, y = val)) +
          geom_point(aes_string(color = "sample_id"), size = 2)
          geom_smooth(method = "lm", formula = 'y ~ x',
                      color = 'black') +
          theme_bw()

  p2 <- autoplot(lm(f, data = data), label.size = 3) +
        theme_bw()

  grid.arrange(p1, p2, ncol = 2)

}

#________________________________________________________

#________________________________________________________
# plot model and boxcox transformation

predict_boxcox_model <- function(train_data, test_data, eqn, pollutant, stove_category) {
  
  f <- formula(eqn)
  f_trans <- formula(gsub(" ~", "^trans ~", eqn))
  
  train_data <- train_data %>%
    dplyr::filter(pol == pollutant) %>%
    dplyr::filter(stove_cat == stove_category)
  
  test_data <- test_data %>%
    dplyr::filter(pol == pollutant) %>%
    dplyr::filter(stove_cat == stove_category) %>%
    dplyr::mutate(equation = eqn)
  
  box_cox <- MASS::boxcox(f, data = test_data, plotit = FALSE)
  transform <- box_cox$x[which.max(box_cox$y)]
  
   test_data <- test_data %>%
                dplyr::mutate(trans = transform)
   
   train_data <- test_data %>%
                 dplyr::mutate(trans = transform)
  
  test_data$pred_val <- predict(lm(f_trans, data = train_data),
                                test_data, by = groups)
  
  test_data <- dplyr::mutate(test_data, pred_val = log(pred_val, trans))
  
  return(test_data)
}

#________________________________________________________
