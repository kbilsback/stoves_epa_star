#________________________________________________________
# require libraries
  library(tidyverse)
  library(gridExtra)
#________________________________________________________

#________________________________________________________
# plot model and boxcox transformation

plot_boxcox_model <- function(data, eqn, response) {
  
  f <- formula(eqn)
  f_trans <- formula(gsub(" ~", "^trans ~", eqn))

  data <- data %>%
          dplyr::filter(pol == response) %>%
          dplyr::group_by(stove_cat)

  models <- data %>%
            dplyr::do(model = lm(f, data = .))
  
  box_cox <- data %>% 
             dplyr::do(box_cox = MASS::boxcox(f, data =., plotit = FALSE)) %>%
             dplyr::mutate(trans = box_cox$x[which.max(box_cox$y)])
  
  data <- data %>%
          dplyr::left_join(dplyr::select(box_cox, stove_cat, trans), by = "stove_cat")

  models_trans <- data %>%
                  dplyr::do(model = lm(f_trans, data = .))

  ggplot(models %>% broom::glance(model),
         aes(x = stove_cat, y = r.squared, color = 'model')) +
    geom_point(size = 5) + 
    geom_point(data = models_trans %>% broom::glance(model),
               aes(x = stove_cat, y = r.squared, color = 'boxcox transformed model'), size = 5) + 
    ggtitle(gsub("val", response, eqn)) +
    theme_bw() + 
    xlab("stove category") +
    ylab("R squared") +
    theme(text = element_text(size = 18), legend.position = "top",
          axis.text.x = element_text(angle = 45, hjust = 1))

}

#________________________________________________________

#________________________________________________________
# plot model and boxcox transformation

plot_boxcox_model_2 <- function(data, eqn) {

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
          theme(text = element_text(size = 16), legend.position = "top",
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
    theme(text = element_text(size = 16), legend.position = "top",
          axis.text.x = element_text(angle = 45, hjust = 1))
  
  grid.arrange(p1, p2, ncol = 2)
  
}

#________________________________________________________

#________________________________________________________
# plot model and boxcox transformation

predict_boxcox_model <- function(train_data, test_data, eqn, response) {
  
  f <- formula(eqn)
  f_trans <- formula(gsub(" ~", "^trans ~", eqn))
  
  train_data <- train_data %>%
                dplyr::group_by(pol, stove_cat)
  
  test_data <- test_data %>%
               dplyr::group_by(pol, stove_cat)
  
  models <- data %>%
    dplyr::do(model = lm(f, data = .))
  
  box_cox <- data %>% 
             dplyr::do(box_cox = MASS::boxcox(f, data =., plotit = FALSE)) %>%
    dplyr::mutate(trans = box_cox$x[which.max(box_cox$y)])
  
  data <- data %>%
    dplyr::left_join(dplyr::select(box_cox, stove_cat, trans), by = "stove_cat")
  
  models_trans <- data %>%
    dplyr::do(model = lm(f_trans, data = .))
  
  
  test_data <- test_data %>%
               dplyr::filter(pol == response) %>%
               dplyr::group_by(stove_cat) %>%
               dplyr::do(pred_val = predict(lm(f, data = train_data),
                                            test_data, by = groups))
  
  box_cox <- data %>% 
    dplyr::do(box_cox = MASS::boxcox(f, data =., plotit = FALSE)) %>%
    dplyr::mutate(trans = box_cox$x[which.max(box_cox$y)])
  
  data <- data %>%
    dplyr::left_join(dplyr::select(box_cox, stove, trans), by = "stove")
  
  models_trans <- data %>%
    dplyr::do(model = lm(f_trans, data = .))
  
  ggplot(models %>% broom::glance(model),
         aes(x = stove, y = r.squared, color = 'model')) +
    geom_point(size = 5) + 
    geom_point(data = models_trans %>% broom::glance(model),
               aes(x = stove, y = r.squared, color = 'boxcox transformed model'), size = 5) + 
    ggtitle(gsub("val", response, eqn)) +
    theme_bw() + 
    xlab("stove type") +
    ylab("R squared") +
    theme(text = element_text(size = 18), legend.position = "top",
          axis.text.x = element_text(angle = 45, hjust = 1))
  
}

#________________________________________________________
