# Clear workspace ---------------------------------------------------------
rm(list = ls())

# Load libraries ----------------------------------------------------------
library(tidyverse)
library(ggplot2)
library(broom)

# Load data ---------------------------------------------------------------
pbc_data_aug <- read_csv("data/03_pbc_data_aug.csv")

# Source functions --------------------------------------------------------
source("R/99_project_functions.R")

# Wrangle data ------------------------------------------------------------
pbc_data_aug <- pbc_data_aug %>% 
  mutate(sex = case_when(sex == "female" ~ 0,
                         sex == "male" ~ 1)) %>% 
  mutate_at(., 
            vars(spiders, hepatom, ascites), 
            list(~ case_when(. == "absent" ~ 0,
                             . == "present" ~ 1))) %>% 
  mutate(drug = case_when(drug == "placebo" ~ 0,
                          drug == "D-penicillamine" ~ 1)) %>% 
  mutate(mayo.risk.level = case_when(mayo.risk.level == "low risk" ~ 0,
                                     mayo.risk.level == "medium risk" ~ 1,
                                     mayo.risk.level == "high risk" ~ 2))

# PCA ---------------------------------------------------------------------
pca_fit <- pbc_data_aug %>% 
  select(where(is.numeric)) %>%  
  scale() %>% 
  prcomp()

pca_fit %>%
  augment(pbc_data_aug) %>% # add original dataset back in
  ggplot(aes(.fittedPC1, .fittedPC2, color = stage)) + 
  geom_point(size = 1.5) 

pca_fit %>%
  tidy(matrix = "rotation")

# define arrow style for plotting
arrow_style <- arrow(
  angle = 20, ends = "first", type = "closed", length = grid::unit(8, "pt")
)

# plot rotation matrix
pca_fit %>%
  tidy(matrix = "rotation") %>%
  pivot_wider(names_from = "PC", names_prefix = "PC", values_from = "value") %>%
  ggplot(aes(PC1, PC2)) +
  geom_segment(xend = 0, yend = 0, arrow = arrow_style) +
  geom_text(
    aes(label = column),
    hjust = 1, nudge_x = -0.02, 
    color = "#904C2F"
  ) +
  xlim(-1.25, .5) + ylim(-.5, 1) +
  coord_fixed()  # fix aspect ratio to 1:1

pca_fit %>%
  tidy(matrix = "eigenvalues") %>%
  ggplot(aes(PC, percent)) +
  geom_col(fill = "#56B4E9", alpha = 0.8) +
  scale_x_continuous(breaks = 1:9) +
  scale_y_continuous(
    labels = scales::percent_format(),
    expand = expansion(mult = c(0, 0.01))
  ) 


# Logistic regression -----------------------------------------------------

# create logistic regression model with status as the outcome variable
model <- pbc_data_aug %>% 
  glm(status ~ .,
      data = .,
      family = binomial(link = "logit"))

model_tidy <- tidy(model, conf.int = TRUE)

