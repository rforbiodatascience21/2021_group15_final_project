# Project specific functions ----------------------------------------------

# Converting specific columns to factors
factor_columns <- function(data) {
  data <- data %>% 
    mutate(sex = factor(sex, 
                        levels = c("male", 
                                   "female"))) %>%
    mutate_at(vars(spiders, 
                   hepatom, 
                   ascites), 
              list(~ factor(., 
                            levels = c("absent", 
                                       "present")))) %>%
    mutate(mayo.risk.level = factor(mayo.risk.level,
                                    levels =  c("low risk",
                                                "medium risk",
                                                "high risk"))) %>%
    mutate(status = factor(status, levels = c(0, 
                                              1))) %>% 
    mutate(stage = factor(stage, levels = c(1, 
                                            2, 
                                            3, 
                                            4)))
  return(data)
}

