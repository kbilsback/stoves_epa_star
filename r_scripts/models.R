#________________________________________________________
# require libraries
  library(tidyverse)
#________________________________________________________

#________________________________________________________
# plot model and boxcox transformation

plot_boxcox_model <- function(data, eqn, pol) {
  
  f <- formula(eqn)
  f_trans <- formula(gsub(" ~", "^trans ~", eqn))

  data <- data %>%
          dplyr::filter(pol == pol) %>%
          dplyr::group_by(stove)

  models <- data %>%
            dplyr::do(model = lm(f, data = .))
  
  box_cox <- data %>% 
             dplyr::do(box_cox = MASS::boxcox(f, data =., plotit = FALSE)) %>%
             dplyr::mutate(trans = box_cox$x[which.max(box_cox$y)])
  
  data <- data %>%
          dplyr::left_join(dplyr::select(box_cox, stove, trans), by = "stove")

  models_trans <- data %>%
                  dplyr::do(model = lm(f_trans, data = .))

  ggplot(models %>% broom::glance(model),
         aes(x = stove, y = r.squared, color = 'linear model')) +
    geom_point(size = 5) + 
    geom_point(data = models_trans %>% broom::glance(model),
               aes(x = stove, y = r.squared, color = 'transformed linear model'), size = 5) + 
    ggtitle(gsub("val", pol, eqn)) +
    theme_bw() + 
    xlab("stove type") +
    ylab("R squared") +
    theme(text = element_text(size = 18), legend.position = "top",
          axis.text.x = element_text(angle = 45, hjust = 1))

}

#________________________________________________________
