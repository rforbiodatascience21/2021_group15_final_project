# Clear workspace ---------------------------------------------------------
rm(list = ls())

# Load libraries ----------------------------------------------------------
library(tidyverse)
library(ggplot2)

# Load data ---------------------------------------------------------------
pbc_data_aug <- read_csv("data/03_pbc_data_aug.csv")

# Wrangle data ------------------------------------------------------------

# we dont want to include the variables we calculated from other variables 
# as these columns cannot be considered independent
pbc_data_aug <- pbc_data_aug %>% 
  select(-edema.score, -mayo.risk, -mayo.risk.level)

# factor status to use as outcome variable
pbc_data_aug <- pbc_data_aug %>% 
  mutate(status = factor(status, levels = c(0, 1)))

# scale the data
pbc_data_scaled <- pbc_data_aug %>% 
  mutate_if(is.numeric, scale) # scale subtracts the mean and divides by the sd 

# Logistic regression model -------------------------------------------------

# we wish to investigate whether the treatment has an effect on survival

# make linear model to predict status
pbc_data_glm <- glm(status ~., data = pbc_data_scaled, family = binomial("logit"))

# tidy to get coefficient summaries
glm_tidy <- tidy(pbc_data_glm, conf.int = TRUE)

# plot estimates of each variable incl the confidence interval
plt_conf <- glm_tidy %>%
  filter(term != "(Intercept)") %>%
  mutate(term = fct_reorder(term, desc(estimate))) %>%
  ggplot(., aes(x = estimate, y = term)) +
  geom_point() +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high)) +
  geom_vline(xintercept = 0,
             color = "grey40",
             linetype = "dashed") +
  theme_classic() +
  theme(
    legend.position = "bottom",
    axis.text.y = element_text(size = 6),
    axis.title.y = element_blank()
  )

# Save plot ---------------------------------------------------------------

ggsave(
  file = "results/plt_conf.png",
  plot = plt_conf,
  width = 12,
  height = 6
)



