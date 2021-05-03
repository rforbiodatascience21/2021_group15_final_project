# Converting specific columns to factors
factor_columns <- function(data) {
  data <- data %>% 
    mutate(sex = factor(sex, levels = c("male", "female")))
  
  data <- data %>%
    mutate_at(., 
              vars(spiders, hepatom, ascites), 
              list(~ factor(., levels = c("absent", "present"))))
  
  data <- data %>%
    mutate(mayo.risk.level = factor(mayo.risk.level,
                                    levels =  c("low risk",
                                                "medium risk",
                                                "high risk")))
  
  data <- data %>% 
    mutate(status = factor(status, levels = c(0, 1)))
  
  data <- data %>% 
    mutate(stage = factor(stage, levels = c(1, 2, 3, 4)))
  return(data)
}

