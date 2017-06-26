#________________________________________________________
# require libraries
  library(tidyverse)
#________________________________________________________

#________________________________________________________
# plot dot plot with geom smooth

plot_lab_model <- function(df, y_var, y_label, filter_var, x_var = "fp",
                          x_label = "firepower (kW)", facet_1 = "stove", plot_color = "sample_id") {
  
  p_df <- df %>%
          dplyr::filter(sample_id != "start_up", sample_id != "shutdown") %>%
          dplyr::filter(fuel_type == filter_var) 
  
  m <- p_df %>%
       dplyr::group_by_(facet_1) %>%
       dplyr::do(model = lm(paste(eval(y_var), "~", eval(x_var)), .)) %>%
    
    
       dplyr::do(boxcox = boxcox(paste(eval(y_var), "~", eval(x_var)), data = .))
       dplyr::mutate(eqn = get_lm_eqn(model))

  eqn <- data.frame(eqn = unclass(m$eqn),
                    stove = m$stove)

  ggplot(p_df, aes_string(x = x_var, y = y_var)) +
    geom_point(aes_string(color = plot_color), size = 2) +
    geom_smooth(method = "lm", formula = 'y ~ poly(x,2)',
                color = 'black') +
    geom_text(aes(x = -Inf, y = Inf, label = eqn),
              data = eqn, color = 'black', size = 7,
              parse = TRUE, vjust = "inward", hjust = "inward") + 
    theme_bw() + 
    facet_wrap(as.formula(paste("~", facet_1)), scales = "free", ncol = 2) +
    ylab(y_label) +
    xlab(x_label) +
    theme(text = element_text(size = 18), legend.position = "top")
}

#________________________________________________________
