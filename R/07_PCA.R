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

# we dont want to include the variables we calculated from other variables 
# as these columns cannot be considered independent
pbc_data_aug <- pbc_data_aug %>% 
  select(-edema.score, -mayo.risk, -mayo.risk.level, -end.age)

# scale the data
pbc_data_scaled <- pbc_data_aug %>% 
  mutate_if(is.numeric, scale) # scale subtracts the mean and divide by the sd

# we can use pivot_wider to dummy code variables
# https://community.rstudio.com/t/factor-to-one-hot-encoding-aka-dummy-variables-using-logicals/69236/2


# PCA ---------------------------------------------------------------------
pca_fit <- pbc_data_scaled %>% 
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
  pivot_wider(names_from = "PC",
              names_prefix = "PC",
              values_from = "value") %>%
  ggplot(aes(PC1, PC2)) +
  geom_segment(xend = 0, yend = 0, arrow = arrow_style) +
  geom_text(
    aes(label = column),
    hjust = 1,
    nudge_x = -0.02,
    color = "#904C2F"
  ) +
  xlim(-0.5, .5) + ylim(-.5, 0.5) +
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



